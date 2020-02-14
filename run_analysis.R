#___LOAD FEATURES, TRAINING AND TEST DATA INTO R___
run_analysis <- function() {
    
    #read in all the needed data: features, x and y datasets from test and training datasets as well as subject data from both datasets
    features_data <<- read_table2("Dataset/features.txt",col_names = FALSE)
    x_test <<- read_table2("Dataset/test/X_test.txt",col_names = FALSE)
    y_test <<- read_table2("Dataset/test/y_test.txt",col_names = FALSE)
    x_train <<- read_table2("Dataset/train/X_train.txt",col_names = FALSE)
    y_train <<- read_table2("Dataset/train/y_train.txt",col_names = FALSE)
    subjects_test <<- read_table2("Dataset/test/subject_test.txt", col_names = FALSE)
    subjects_train <<- read_table2("Dataset/train/subject_train.txt", col_names = FALSE)
    #add activity code to features data so that we can use that to rename column that contains activity codes for each observation
    features_data <<- features_data %>% add_row(X1=562,X2="activity_code")
    
    #store variable names so that we can use that in renaming the columns
    variable_names <<- features_data$X2
    
    #read activity codes for both data sets
    activity_code_data_train <<- y_train$X1
    activity_code_data_test <<- y_test$X1
    
    #add activity codes and rename column names (=variable names) to more descriptive names
    x_train <<- x_train %>% mutate(activity_code = activity_code_data_train) %>% set_names(variable_names)
    x_test <<- x_test %>% mutate(activity_code = activity_code_data_test) %>% set_names(variable_names)
    
    #add subject id values to both datasets
    x_train <<- x_train %>% add_column(subject=subjects_train$X1)
    x_test <<- x_test %>% add_column(subject=subjects_test$X1)
    
    
    #make the variable names unique in both datasets so that we can apply dplyr mutate functions over those
    x_train <<- x_train %>% setNames(make.unique(names(.))) 
    x_test <<- x_test %>% setNames(make.unique(names(.))) 
    
    #keep only the columns that include the mean, standard deviation (=std), activity codes (=code) and subject ids (=subject)
    xtrain_stdmean <<- x_train %>% select(contains("mean") | contains("std") | contains("code")| contains("subject"))
    xtest_stdmean <<- x_test %>% select(contains("mean") | contains("std") | contains("code")| contains("subject"))
    
    #make new variable activity and convert activity codes to human-readable value and put the into the new variable
    xtest_stdmean <<-  xtest_stdmean %>% mutate(activity = case_when(activity_code==1 ~ "Walking", activity_code==2 ~ "Walking upstairs", activity_code==3 ~ "Walking downstairs",activity_code==4 ~ "Sitting",activity_code==5 ~ "Standing", activity_code==6 ~ "Laying"))
    xtrain_stdmean <<-  xtest_stdmean %>% mutate(activity = case_when(activity_code==1 ~ "Walking", activity_code==2 ~ "Walking upstairs", activity_code==3 ~ "Walking downstairs",activity_code==4 ~ "Sitting",activity_code==5 ~ "Standing", activity_code==6 ~ "Laying"))
    
    #store variable names in its own variable so that we can use those when merging the two datasets
    variable_names <<- names(xtest_stdmean)
    
    #Merge the two datasets on all columns on all variables that have the same name
    x_joined <<- left_join(xtrain_stdmean,xtest_stdmean,by=variable_names)
    
    #drop activity code variable since we already have human-readable activities
    x_joined <<- x_joined %>% select(-one_of("activity_code"))
    
    #Group the merged dataset based on activity and subjet
    x_joined <- x_joined %>% group_by(activity,subject)
    
    #create a new tidy dataset that includes caculated arithmetic mean (=average) of each variable for each group
    x_joined_mean_by_group <<- x_joined %>% summarise_all("mean")
}

