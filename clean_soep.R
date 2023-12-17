# _______________________________#
# SOEP Data
# Cleaning and Generating New Variables for later Analysis
# Started: 2023-11-30
# Last edited: 
#________________________________#

rm(list = ls())

library(haven)
library(labelled)
library(sjlabelled)
library(dplyr)
library(tidyverse)
library(purrr)
library(fs)
library(Hmisc)
library(vctrs)
library(ggplot2)
library(gt)
library(plm)
library(panelr)
library(vtable)
library(glmnet)
library(fixest)
library(lubridate)

# Preliminary Information
# Data is the German Socio-Economic Panel Study, received from the German Institute for Economic Research (DIW)
# Used STATA to merge individual data files to combine them into one "long.dta" dataset
# Selected only adults with successful interview using the "netto" variable
# Missing Variables have been already coded as such


# Load the data -----------------------------------------------------------

soep <- read_dta("data/02_temp/long.dta")

# Variable labels: short description of variable
# value labels: labels associated with specific values
# missing values

# Creating Dictionary
dictionary <- generate_dictionary(soep)

# Clean the Data ----------------------------------------------------------

soep <- soep |> 
  mutate(
  # Recode the variables and label them
    
    sex = recode(sex, `2` = 0),
    sex = labelled(sex, "Gender", labels = c("female" = 0, "male" = 1)),
    
    germborn = recode(germborn, `2` = 0),
    germborn = labelled(germborn, "Germanborn", labels = c("not born in Germany" = 0, "born in Germany or immigrated before 1950" = 1)),
    
    migback = recode(migback, `2` = 3, `3` = 2),
    migback = labelled(migback, "Migration Background", labels = c("no migration background" = 1, "indirect migration background" = 2, "direct migration background" = 3)),
    
    arefback = recode(arefback, `2` = 3, `3` = 2),
    arefback = labelled(arefback, "Refugee Background", labels = c("no refugee experience" = 1, "indirect refugee experience" = 2, "direct refugee experience" = 3)),
    
    sampreg = recode(sampreg, `2` = 0),
    sampreg = labelled(sampreg, "Lives in West or Eastern Germany", labels = c("West Germanry" = 1, "East Germany" = 0)),
    
    regtyp = recode(regtyp, `2` = 0),
    regtyp = labelled(regtyp, "Area Type", labels = c("Urban area" = 1, "Rural area" = 0)),
    
    oral_abil_ger = 6 - plj0071,
    oral_abil_ger = labelled(oral_abil_ger, labels = c("not at all" = 1, "not very well" = 2, "average" = 3, "well" = 4, "very well" = 5)),
    
    written_abil_ger = 6 - plj0072,
    written_abil_ger = labelled(written_abil_ger, labels = c("not at all" = 1, "not very well" = 2, "average" = 3, "well" = 4, "very well" = 5)),
    
    read_abil_ger = 6 - plj0073,
    read_abil_ger = labelled(read_abil_ger, labels = c("not at all" = 1, "not very well" = 2, "average" = 3, "well" = 4, "very well" = 5))
  
  ) |> 
  
  # Generate new variables
  mutate(
    foreignborn = if_else(germborn == 1, 0, 1),
    foreignborn = labelled(foreignborn, labels = c("Born abroad" = 1, "Born in Germany" = 0)),
    germ_national = if_else(pgnation == 1, 1, 0),
    
    # Create new variable: migaref
    migaref = case_when(
      germborn == 1 ~ 1, # no migration background
      germborn == 0 & migback == 3 & arefback != 3 | is.na(arefback) ~ 2, # direct migration background but no refugee experience
      germborn == 0 & migback == 3 & arefback == 3 ~ 3, # direct migration background with refugee experience
      TRUE ~ NA_integer_),
    
    age = syear - gebjahr,
    age_at_immig = immiyear - gebjahr,

    years_since_immig = syear - immiyear,

    # Employment dummy
    emplyd = if_else((pglfs == 11 | pglfs == 12), 1, 0),
    # Work experience squared
    pgexpft_sq = pgexpft^2,

    # Logged income
    lnpglabgro = log(pglabnet),
    lnpglabnet = log(pglabnet),
    lnhghinc = log(hghinc),
    
    # Education dummies
    low_educ = if_else((pgisced97 == 1 | pgisced97 == 2), 1, 0), # inadequately, general elementary
    med_educ = if_else((pgisced97 == 3 | pgisced97 == 4), 1, 0), # middle vocational, vocational + high school degree
    high_educ = if_else((pgisced97 == 5 | pgisced97 == 6), 1, 0),# higher vocational, higher education
    
    # Occupation dummies
    occupation = as.integer(substr(pgisco08, 1, 1)),
    
    # Family status
    famstd = case_when(
      pgfamstd %in% c(1, 6, 7) ~ 1,
      pgfamstd %in% c(2, 4, 8) ~ 2,
      pgfamstd == 5 ~ 4,
      TRUE ~ NA_integer_),
    famstd = labelled(famstd, labels = c("Married" = 1, "Separated or Divorced" = 2, "Single" = 3, "Widowed" = 4)),
    married = if_else(famstd == 1, 1, 0),
    
    # Generate Language Dummy from self-reported ability on scale 1-5
    # Code it as 1 if language ability is "very good"
    oral_abil_ger_dum = if_else(oral_abil_ger == 5, 1, 0),
    written_abil_ger_dum = if_else(written_abil_ger == 5, 1, 0),
    read_abil_ger_dum = if_else(read_abil_ger == 5, 1, 0),
    
    good_ger_command = if_else(oral_abil_ger >= 4 | written_abil_ger >= 4 | read_abil_ger >= 4, 1, 0)
  ) |> 
  
  # Rename variables
  rename(
    # Rename relevant variables for ease
    years_of_educ = pgbilzeit,
    years_work_exp = pgexpft,
    years_work_exp_sq = pgexpft_sq,
    wages_gro = pglabgro, 
    wages_net = pglabnet, 
    ln_wages_gro = lnpglabgro,
    ln_wages_net = lnpglabnet,
    ln_hh_inc = lnhghinc,
    
    # Rename the courses
    course_1 = plj0654_h, # bamf integration
    course_2 = plj0499_h, # esf-bamf
    course_3 = plj0508, # entry course german 
    course_4 = plj0535, # other german course
    course_5 = plj0517, # perspectives refugees
    course_6 = plj0526, # perspectives young refugees
    
    # Rename the year in which the courses started
    course_1_year = plj0655,
    course_2_year = plj0500,
    course_3_year = plj0509,
    course_4_year = plj0536,
    course_5_year = plj0518,
    course_6_year = plj0527,
    
    # Year in which courses ended and were completed
    course_1_end = plj0657,
    course_2_end = plj0502,
    course_3_end = plj0511,
    course_4_end = plj0538,
    course_5_end = plj0520,
    course_6_end = plj0529,
    
    # Rename language ability before coming to Germany
    oral_abil_ger_premig = lm0128i01,
    written_abil_ger_premig = lm0128i02,
    read_abil_ger_premig = lm0128i03
  ) 

# Coerce variables into numeric values
# Rename variables
soep <- soep |> 
  mutate(
    sex = as.numeric(sex),
    regtyp = as.numeric(regtyp),
    germborn = as.numeric(germborn),
    oral_abil_ger = as.numeric(oral_abil_ger),
    written_abil_ger = as.numeric(written_abil_ger),
    read_abil_ger = as.numeric(read_abil_ger)
  )



# Save the data -----------------------------------------------------------

save(soep, file = "data/03_clean/soep_clean.RData")

  
