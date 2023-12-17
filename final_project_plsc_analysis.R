# _______________________________#
# PLSC 536
# Final Project Analysis File
# Started: 2023-11-30
# Last edited: 2023-12-17
#________________________________#

rm(list = ls())

library(haven)
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
library(panelView)
library(patchwork)
library(modelsummary)


# Load the data -----------------------------------------------------------
# Data comes from the German Socio-Economic Panel Study
# Has been merged in STATA, load into R using haven package and cleaned and prepared for analysis
# in final_project_plsc_clean.R file

load("./data/03_clean/soep_plsc.RData")

# Overview ----------------------------------------------------------------

# Visualization

# Grouping and summarizing the data
grouped_data <- soep  |> 
  group_by(syear, treatment)  |> 
  summarise(count = n()) |> 
  ungroup()

# Creating a bar plot to visualize treatment status
ggplot(grouped_data, aes(x = as.factor(syear), y = count, fill = as.factor(treatment))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("lightgray", "steelblue"), 
                    labels = c("Not treated", "Treated"),
                    name = "Treatment Status") +
  labs(title = "Treatment status distribution over survey period 2016-2021",
       x = "Survey year",
       y = "Number of distinct individuals") +
  theme_minimal()

# How many average responses in the data?
responses_per_pid <- soep |> 
  group_by(pid)  |> 
  summarise(number_of_responses = n())

# Calculate the average number of responses
mean(responses_per_pid$number_of_responses)
# How often people respond
responses_per_pid |> group_by(number_of_responses) |> count()

# How many people are in each treatment group?
  soep |> 
    group_by(group) |> 
    distinct(pid) |> 
    count()
  
# How many people completed each of the 6 different courses?
  
  # Vector of course variable names
  course_vars <- c("course_1_end", "course_2_end", "course_3_end", "course_4_end", "course_5_end", "course_6_end")
  
  # Function to count distinct individuals with non-missing values for each course
  count_distinct_individuals <- function(variable, data) {
    non_missing_data <- data[!is.na(data[[variable]]), ]
    length(unique(non_missing_data$pid))
  }
  
  # Apply the function to each course variable
  distinct_individual_counts <- sapply(course_vars, count_distinct_individuals, data = soep)
  
  # Descriptive names for each course
  course_names <- c("BAMF Integration", "ESF-BAMF", "Entry Course German", 
                    "Other German Language Course", "Perspectives Refugees", 
                    "Perspectives Young Refugees")
  
  # Convert the counts to a data frame for plotting
  course_data <- data.frame(
    course = course_names,
    count = distinct_individual_counts
  )
  
  # Calculate fraction of total for each course
  course_data <- course_data %>%
    mutate(fraction = count / sum(count)) %>%
    arrange(desc(fraction))
  
  # Create the bar plot
  ggplot(course_data, aes(x = reorder(course, fraction), y = fraction)) +
    geom_bar(stat = "identity") +
    labs(x = "Course", y = "Fraction of Individuals", title = "Refugees' Participation in Language Courses") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 55, hjust = 1)) # Rotate x-axis labels for readability

# Understanding unemployment and missing wages patterns
  # each individual who is unemployed has been coded NA for wages in original data, 0 for wages_gro_new
  soep |> 
    group_by(emplyd) |> 
    summarise(na = sum(is.na(wages_gro_new)))

# Summary Tables ----------------------------------------------------------

# Reorder factor levels for group:
soep <- soep |> 
  mutate(treatment_type = factor(treatment_type, levels = c("No Treatment", "Treatment pre-survey", "pre treatment", "post treatment")))

# Create summary table
soep |> 
  sumtable(
    vars = c("sex", "age", "married", "regtyp", "years_since_immig", "years_of_educ", "emplyd", "wages_gro", "hghinc", "oral_abil_ger", "written_abil_ger", "read_abil_ger"),
    labels = c("Gender", "Age", "Married", "Live in urban area", "Years since immigration", "Years of Education", "Employed", "Gross Labor Income", "Household Income", "Oral Ability German", "Written Ability German", "Reading Ability German"),
    summ = c("mean(x)", "sd(x)", "notNA(x)"),
    summ.names = c("Mean", "Std. Dev", "n"),
    title = "Descriptive Statistics",
    group = "treatment_type",
    out = "latex"
    )

# Two Way Fixed Effects Regression ------------------------------------------------------
# Using feols

# Outcome: Wages (Continuous)
  
  # Regress log gross wages on treatment only, no other covariates, using full sample
  fit0 <- feols(ln_wages_gro ~ treatment | pid + syear, data = soep)
  summary(fit0)
  
  # Using different group comparisons:
  fit0_1 <- feols(ln_wages_gro ~ treatment | pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment during survey"))
  summary(fit0_1)
  #fit0_2 <- feols(ln_wages_gro ~ treatment | pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment pre-survey"))
  # This has very high standard errors because treatment is correlated with individual fixed effects?
  # fit0_2 <- feols(ln_wages_gro ~ treatment | syear, data = subset(soep, group == "No Treatment" | group == "Treatment pre-survey"))
  # I don't need to report the comparison never - pre, because that is an apples with oranges comparison
  
  fit0_3 <- feols(ln_wages_gro ~ treatment | pid + syear, data = subset(soep, group == "Treatment during survey" | group == "Treatment pre-survey"))
  # the last specification is where we are least concerned about selection bias
  etable(fit0, fit0_1, fit0_3, tex = T)
  
  
  # Regress log gross wages on treatment, and covariate age as factor
  model1 <- feols(ln_wages_gro ~ treatment + as.factor(age) | pid + syear, data = soep)
  model1_1 <- feols(ln_wages_gro ~ treatment + as.factor(age) | pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment during survey"))
  model1_3 <-  feols(ln_wages_gro ~ treatment + as.factor(age) | pid + syear, data = subset(soep, group == "Treatment during survey" | group == "Treatment pre-survey"))
  
  # LateX Table
  etable(model1, model1_1, model1_3, tex = T)

# Outcome: Employment (Binary)
  
  # Regress employment dummy on treatment (without age as covariate)
  model0 <- feols(emplyd ~ treatment | pid + syear, data = soep)
  # Iterating through all the different group comparisons:
  model0_1 <- feols(emplyd ~ treatment | pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment during survey"))
  # model0_2 <- feols(emplyd ~ treatment | pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment pre-survey"))
  # The model above yields high standard errors due to pid being collinear with treatment
  # model0_2 <- feols(emplyd ~ treatment | syear, data = subset(soep, group == "No Treatment" | group == "Treatment pre-survey"))
  model0_3 <- feols(emplyd ~ treatment | pid + syear, data = subset(soep, group == "Treatment during survey" | group == "Treatment pre-survey"))
  
  
  # Regress employment dummy on treatment and age as factor
  model2 <- feols(emplyd ~ treatment + as.factor(age) | pid + syear, data = soep)
  model2_1 <- feols(emplyd ~ treatment + as.factor(age)| pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment during survey"))
  model2_3 <- feols(emplyd ~ treatment + as.factor(age)| pid + syear, data = subset(soep, group == "Treatment during survey" | group == "Treatment pre-survey"))
  
  # LateX Table
  etable(model2, model2_1, model2_3, tex = T)

# For the Appendix:
  # Regress new absolute wage variable (which codes NA values of wages as zeroes)
  # This uses the entire sample and is an "in between" of the previous two
  model3 <- feols(wages_gro_new ~ treatment + as.factor(age) | pid + syear, data = soep)
  model3_1 <- feols(wages_gro_new ~ treatment + as.factor(age)| pid + syear, data = subset(soep, group == "No Treatment" | group == "Treatment during survey"))
  model3_3 <- feols(wages_gro_new ~ treatment + as.factor(age)| pid + syear, data = subset(soep, group == "Treatment during survey" | group == "Treatment pre-survey"))

  # LaTeX Table
  etable(model3, model3_1, model3_3, tex = T)

# Check for Pretrends -----------------------------------------------------

# Compare the No Treatment and pre-treatment group
relevant_data <- soep %>%
  filter(syear %in% c(2016, 2017), 
         treatment_type %in% c("No Treatment", "pre treatment"))

# Calculate the average gross wages and employment rate
average_metrics <- relevant_data %>%
  group_by(syear, treatment_type) %>%
  summarise(
    avg_wages = mean(wages_gro, na.rm = TRUE),
    avg_employment_rate = mean(emplyd, na.rm = TRUE)
  )

# View the results
print(average_metrics)

# Adjusting the legend labels
legend_labels <- c("Never Treated (Control)", "Treated during survey:\n Pre-training")

# Plot for Average Gross Wages
wages_plot <- ggplot(average_metrics, aes(x = syear, y = avg_wages, group = treatment_type, color = treatment_type)) +
  geom_line() +
  scale_x_continuous(breaks = c(2016, 2017)) +
  scale_color_manual(values = c("blue", "red"), labels = legend_labels) +
  labs(x = "Survey Year", y = "Average Gross Wages", title = "Pre-Trend Comparison: Gross Wages", color = "Group") +
  theme_minimal()

# Plot for Average Employment Rate
employment_plot <- ggplot(average_metrics, aes(x = syear, y = avg_employment_rate, group = treatment_type, color = treatment_type)) +
  geom_line() +
  scale_x_continuous(breaks = c(2016, 2017)) +
  scale_color_manual(values = c("blue", "red"), labels = legend_labels) +
  labs(x = "Survey Year", y = "Average Employment Rate", title = "Pre-Trend Comparison: Employment Rate", color = "Group") +
  theme_minimal()

# Combine the plots in one row
combined_plot <- wages_plot + employment_plot + plot_layout(ncol = 2)
print(combined_plot)

# Heterogeneity Effects (not reported) ---------------------------------------------------

# Dimension: Gender
fit0 <- feols(ln_wages_gro ~ treatment*sex | pid + syear, data = soep)
summary(fit0)

# Dimension: Education level
fit_educ_1 <- feols(emplyd ~ treatment + as.factor(age)| pid + syear, data = subset(soep, (group == "No Treatment" | group == "Treatment during survey") & low_educ == 1))
fit_educ_2 <- feols(emplyd ~ treatment  + as.factor(age)| pid + syear, data = subset(soep, (group == "No Treatment" | group == "Treatment during survey") & med_educ == 1))
fit_educ_3 <- feols(emplyd ~ treatment  + as.factor(age)| pid + syear, data = subset(soep, (group == "No Treatment" | group == "Treatment during survey") & high_educ == 1))
etable(fit_educ_1, fit_educ_2, fit_educ_3, tex = T)

# Dimension: Age
fit0 <- feols(ln_wages_gro ~ treatment | pid + syear, data = subset(soep, age <= 30))
summary(fit0)


# Regressions for only Integration Course Group ---------------------------
model0 <- feols(emplyd ~ treatment_integ | pid + syear, data = subset(soep, sex == 1))
summary(model0)

