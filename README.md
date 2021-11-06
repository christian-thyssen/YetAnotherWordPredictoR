# Yet Another Word PredictoR

This repository contains "Yet Another Word PredictoR" written in R.
The following sections explain the purpose of the different files.

## Code for Preliminary Analyses

These files contain some code for preliminary analyses:

- 0_understanding_the_problem.R
- 1_getting_and_cleaning_the_data.R
- 2_exploratory_data_analysis.R

## Code for Building and Testing the Prediction Model

This file is rather important, since it contains the code for building and testing the model.

- 4_prediction_model.R

## Milestone Report

This is the milestone report:

- MilestoneReport.Rmd
- MilestoneReport.html

## Slide Deck

This is the slide deck:

- index.Rmd
- index.html

## Shiny App

This is the app:

- YetAnotherWordPredictoR/ui.R
- YetAnotherWordPredictoR/server.R

To run it locally, you have to generate the model files ("final_vocabulary.rds", "final_ngmats.rds", "final_svecs.rds") first and place them in the app's directory. To achieve this, run "0_understanding_the_problem.R" for downloading the data and the needed parts of "4_prediction_model.R" for creating the files (or the whole file if you have the time).

## Miscellaneous Files

Additionally to the aforementioned files there a the following "standard" files:

- .gitignore
- README.md
- YetAnotherWordPredictoR.Rproj
