## Data Analysis for CPS Predicting Student Enrollment ##

This folder contains our work on top-down school-level modeling and exploratory data analysis. 

### Top-down school-level model ###

The code `CPS_gradebygrade_dataprep.R` will create a data file with all of the features we considered in the top-down model.
The code `CPS_gradebygrade_script.R` creates functions that we used to try out different regression models. 
The code `CPS_gradebygrade_modelingexploration.R` uses the above two scripts and goes through many different models. 

The code `topdown_FINAL.R` is the final code where we focused on modeling catchment high school enrollment. 

### Exploratory data analysis ###

The code `prepare_data_and_shapefile_for_tableau.R` prepares the data for the Tableau workbook that shows where students from one catchment area go to school in 2013. 

The code `map_neighborhood_catchment.R` plots the percent of students who go to their catchment area. 

The code `grade9_transition_matrix.R` produces our plots of where 9th graders live versus where they go to high school. 

`investigate_9thgrade.R` and `investigate_juarez_change.R` are files we used to explore where 9th grade 
students go to school and where they live. In particular we look at how many students at certain schools
come from within their catchment areas or come from outside CPS. 

The codes called `DeepDive1EDA_Counts.Rmd`, `exploratory_data_analysis.R`, `explore_8_to_9_transition.R`,
`explore_enroll_totals_from_website.Rmd`
are some of our initial exploratory files. 

The code `CPS_JAGS.R` documents our exploratory hierarchical modeling of cohort survival ratios. 
Through R we use the program called JAGS, which stands for Just Another Gibbs Sampler. 

`dataprep_geocodeschools.R` is where we find that latitude and longitude of each CPS school using the ggmap package in R. 

`history_evaluation.R` is the visualization of historical error of CPS. 

