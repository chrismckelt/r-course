# Coursera - GettingAndCleaningData - Assignment results
###based off dataset http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 

**run_analysis.R performs the following operations**

1. gets packages ("data.table", "reshape2", "plyr", "assertthat", "knitr", "markdown")
2. create local folders (results, UCI, HAR Dataset) after pulling down and extracting the zip
3. combines the provided subject, activity and training data in 1 main data set 'dt'
4. filters the feature data for measurements on the mean and standard deviation 
5. on the main data set applies select expression using the filtered features column names
6. appends the activities to the main data set
7. melts the main data set category names (subject, activityId, activityName) by measurements
8. outputs a tidy dataset containing the count, average, for each variable for each activity and each subject



![image](https://cloud.githubusercontent.com/assets/662868/15468419/c18fe40a-2116-11e6-9e0f-3287602d37de.png)
