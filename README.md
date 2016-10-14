# projectCAGD

This is the course project for the Getting and Cleaning Data Coursera course.



### Overview
This project serves to demonstrate the collection and cleaning of a tidy data set that can be used for subsequent
analysis. A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)


###The R script
The R script, `run_analysis.R`, does the following:

1. Download the dataset if it does not already exist in the working directory
2. Load the feature info
3. Loads both the training and test datasets, keeping only those columns which
   reflect a mean or standard deviation
4. Loads the activity and subject data for each dataset, and merges those
   columns with the dataset
5. Merges the two datasets
6. Load the activity info
7. maps the activity to the tidy dataset
8. Creates a dataset that consists of the average (mean) value of each
   variable for each subject and activity pair.

The result after step 7 is shown in the file `tidy.csv`.
The result after step 8 is shown in the file `mean.csv`.
