This repository contains the relevant data and code to replicate my submission for the course PLSC 536 "Applied Quantitative Research Design", which I took in fall semester of 2023 at Yale University.

# Data
* **Raw Data**: Requested and accessed from the German Institute for Economic Research, DIW Berlin. Not made available due to data privacy requirements.
* **soep_plsc.RData**: Cleaned data file used to perform analysis

# Code
* **clean_soep.R**: Takes as input "long.dta", produces as output "soep_clean.RData". General cleaning of data, renaming and recoding of variables 
* **final_project_plsc_clean.R**: Takes as input "soep_clean.RData", produces as output "soep_plsc.RData". Cleaning of data and creation of new variables specific to the PLSC project
* **final_project_plsc_analysis.R**: Takes as input "soep_plsc.RData". Contains analysis, produces tables and figures from the paper
