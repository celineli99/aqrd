# _______________________________#
# PLSC 536
# Final Project
# Started: 2023-11-30
# Last edited: 2023-12-17
#________________________________#

rm(list = ls())

library(haven)
library(dplyr)
library(tidyverse)
library(purrr)
library(fs)
library(ggplot2)
library(vtable)
library(fixest)
library(lubridate)

# Load the data -----------------------------------------------------------
  # Data comes from the German Socio Economic Panel Study
  # Has been merged in STATA from individual data files, and cleaned in R
  # Missing Variables have been already coded as such by a .do file

load("./data/03_clean/soep_clean.RData")

# Restrict to M3-M6 samples, only refugees
soep <- subset(soep, psample == 17 | psample == 18 | psample == 19 | psample == 24)


# Dependent Variables: Employment and Wages -------------------------------
# Individuals who are unemployed (emplyd == 0) have NA (missing) for wages. 
# Create a new wage variable that takes NA as zero:

soep <- soep |> 
  mutate(
    wages_gro_new = if_else(emplyd == 0 & is.na(wages_gro), 0, wages_gro)
    )

# Generate Treatment: Language Training --------------------------
# Pooling all Trainings together:
# Define "completed language training" as: Individual has indicated language end date

soep <- soep |> 
  mutate(
    # Create variable that takes, by each person-year, the earliest time a course ended (definition of a completed course)
    # for those cases where respondents participated in multiple courses
    course_earliest_year = pmin(course_1_end, course_2_end, course_3_end, course_4_end, course_5_end, course_6_end, na.rm = TRUE)
    )

# For each pid, create new variable "earliest course", the year in which earliest course was completed
# this year marks the beginning of "treatment"
earliest_per_pid <- soep |> 
  group_by(pid) |> 
  summarise(earliest_course = if (all(is.na(course_earliest_year))) NA_real_ else min(course_earliest_year, na.rm = TRUE)) |> 
  ungroup()

# Join this information back to the original dataset
# this gives consistent "earliest course information" for each unique person
soep <- soep |> 
  left_join(earliest_per_pid, by = "pid") |> 
  mutate(
    course_earliest_year = earliest_course
  ) |> 
  select(-earliest_course)

# Focusing only on Integration course:
soep <- soep |> 
  mutate(
    # For each person-year, take the year when the integration course (course 1) ended
    integ_course_end_year = course_1_end
  )
# For each pid, create new variable "earliest_integ_course", the year in which the integration course was completed
earliest_integ_per_pid <- soep |>
  group_by(pid) |>
  summarise(earliest_integ_course = if(all(is.na(integ_course_end_year))) NA_real_ else min(integ_course_end_year, na.rm = TRUE)) |>
  ungroup()

# Join this information back to the original dataset
# This provides consistent "earliest integration course completion year" information for each unique person
soep <- soep |>
  left_join(earliest_integ_per_pid, by = "pid") |>
  mutate(
    integ_course_end_year = earliest_integ_course
  ) |>
  select(-earliest_integ_course)

soep_sel <- soep |> select(pid, syear, course_1_end, integ_course_end_year)

# Construct Treatment Variables --------------------------------------------

# Treatment 1: Pooled across courses. Once language course taken, forever treated ----------------

# Create treatment variable: 
# For each pid:
# 0 always if never treated,
# 1 always if treated before survey started,
# 0 first and then 1 if treated during survey period

# Code treatment as 1 if individual has been treated in any one of the previous years
soep <- soep |> 
  group_by(pid) |> 
  mutate(
    treatment = ifelse(is.na(course_earliest_year), 0, ifelse(syear > course_earliest_year, 1, 0))
    )

# Alternatively: Code as 1 if the individual has been treated in that year or any previous year
#soep <- soep |> 
#    group_by(pid) |> 
#    mutate(
#    treatment = ifelse(is.na(course_earliest_year), 0, ifelse(syear >= course_earliest_year, 1, 0))
#    )
  
# Check how many individuals exhibit variation in treatment:
variation_in_treatment <- soep  |> 
  group_by(pid)  |> 
  summarise(has_zero = any(treatment == 0), has_one = any(treatment == 1)) |> 
  filter(has_zero & has_one) |> 
  pull(pid)

soep_variation_treatment <- soep |> 
  filter(pid %in% variation_in_treatment) 

soep_variation_treatment |> 
  distinct(pid) 


# Differentiate courses: --------------------------------------------------

# Treatment: Integration course. Treated if individual has taken integration course, in survey year or thereafter  ----------------

# Code treatment as 1 if individual has been treated in any one of the previous years
soep <- soep |> 
  group_by(pid) |> 
  mutate(
    treatment_integ = ifelse(is.na(integ_course_end_year), 0, ifelse(syear >= integ_course_end_year, 1, 0))
  )


# Treatment Categories ----------------------------------------------------

# Pooled:
  # Classify persons in three types of groups: No treatment, treatment pre-survey, treatment during survey
  pid_types <- soep |> 
    group_by(pid) |> 
    summarise(group = case_when(
      all(treatment == 0) ~ "No Treatment", # has never taken a language course, ever
      all(treatment == 1) ~ "Treatment pre-survey", # has taken a language course before survey started 
      TRUE ~ "Treatment during survey" # has taken language course during survey period
    ))  |> 
    ungroup()
  
  # Join back to original dataset
  soep <- soep |> 
    left_join(pid_types, by = "pid")
  
# Construct variable that indicates for "Treatment during survey" group pre and post language training
soep <- soep |> 
  mutate(
    treatment_type = case_when(
      group == "Treatment pre-survey" ~ "Treatment pre-survey",
      group == "No Treatment" ~ "No Treatment",
      group == "Treatment during survey" & treatment == 0 ~ "pre treatment",
      group == "Treatment during survey" & treatment == 1 ~ "post treatment"
    )
  )

# For Integration Course:
# Classify persons in three types of groups: No treatment, treatment pre-survey, treatment during survey
pid_types_integ <- soep |> 
  group_by(pid) |> 
  summarise(group_integ = case_when(
    all(treatment_integ == 0) ~ "No Treatment", # has never taken a language course, ever
    all(treatment_integ == 1) ~ "Treatment pre-survey", # has taken a language course before survey started 
    TRUE ~ "Treatment during survey" # has taken language course during survey period
  ))  |> 
  ungroup()

# Join back to original dataset
soep <- soep |> 
  left_join(pid_types_integ, by = "pid")

# Construct variable that indicates for "Treatment during survey" group pre and post language training
soep <- soep |> 
  mutate(
    treatment_type_integ = case_when(
      group_integ == "Treatment pre-survey" ~ "Treatment pre-survey",
      group_integ == "No Treatment" ~ "No Treatment",
      group_integ == "Treatment during survey" & treatment_integ == 0 ~ "pre treatment",
      group_integ == "Treatment during survey" & treatment_integ == 1 ~ "post treatment"
    )
  )

# Save the Data -----------------------------------------------------------

save(soep, file = "data/03_clean/soep_plsc.RData")



