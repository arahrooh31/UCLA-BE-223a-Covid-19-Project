# Let's try some BNN
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")


set.seed(1)
percent_training = 0.7
num_folds = 3

local_path = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/"
community_today = read.csv(paste0(local_path, "community_todayUpdated.csv")) # N = 334 obs
cv_cases_longitudinal = read.csv(paste0(local_path, "community_TimeCases_long.csv")) 
community_demo = read.csv(paste0(local_path, "community_demographic_cleaned.csv")) # N = 265 obs

# Merged demographic variables
merged_community = 
  cv_cases_longitudinal %>% 
  filter(date == max(as.Date(cv_cases_longitudinal$date)) - 10) %>% 
  # Here I used 10 days before just to make sure they have values for new_7d_lead
  inner_join(community_demo, by = c("place_ID" = "place_ID"))
# N - 212 obs


#================================================
# The preparation of sample dataset starts here #
#================================================
test_model_data =
  merged_community %>% 
  mutate(outbreak_7d_lead = as.factor(case_when(
    new_7d_lead >= 3 ~ "Yes",
    !is.na(new_7d_lead) ~ "No"
  ))) %>% 
  mutate(outbreak_3d_lead = as.factor(case_when(
    new_3d_lead >= 3 ~ "Yes",
    !is.na(new_3d_lead) ~ "No"
  ))) %>% 
  mutate(outbreak_MA_3d_incremental = as.factor(case_when(
    case_incremental_3d_MA >= 50 ~ "more_than_50",
    case_incremental_3d_MA >= 10 ~ "10-50",
    case_incremental_3d_MA >= 3 ~ "3-10",
    case_incremental_3d_MA >= 0 ~ "less_than_3"
  ))) %>%
  mutate(cumulative_cases_cat = as.factor(case_when(
    cumulative_cases_today > 1000 ~ "1000_and_more",
    cumulative_cases_today > 500 ~ "500_to_1000",
    cumulative_cases_today > 0 ~ "Less_than_500"
  ))) %>% 
  mutate(place_ID = as.factor(place_ID)) %>% 
  mutate(age10_less_cat = as.factor(case_when(
    AGE.10.OR.LESS < 0.05 ~ "less_than_5%",
    AGE.10.OR.LESS < 0.1 ~ "5-10%",
    AGE.10.OR.LESS < 0.2 ~ "10-20%",
    TRUE ~ "larger_than_20%"
  ))) %>% 
  mutate(age65_more_cat = as.factor(case_when(
    AGE.65.AND.UP < 0.05 ~ "less_than_5%",
    AGE.65.AND.UP < 0.1 ~ "5-10%",
    AGE.65.AND.UP < 0.2 ~ "10-20%",
    TRUE ~ "larger_than_20%"
  ))) %>% 
  mutate(high_school_orless_cat = as.factor(case_when(
    LESS.THAN.HIGH.SCHOOL < 0.05 ~ "less_than_5%",
    LESS.THAN.HIGH.SCHOOL < 0.25 ~ "5-25%",
    LESS.THAN.HIGH.SCHOOL < 0.50 ~ "25-55%",
    TRUE ~ "larger than 50%"
  ))) %>% 
  mutate(masters_degree_orhigher_cat = as.factor(case_when(
    MASTERS.DEGREE.OR.HIGHER < 0.05 ~ "less_than_5%",
    MASTERS.DEGREE.OR.HIGHER < 0.15 ~ "5-15%",
    MASTERS.DEGREE.OR.HIGHER < 0.25 ~ "15-25%",
    TRUE ~ "larger than 25%"
  ))) %>% 
  mutate(non_white_cat = as.factor(case_when(
    NON.WHITE.POPULATION < 0.15 ~ "less_than_15%",
    NON.WHITE.POPULATION < 0.30 ~ "15-30%",
    NON.WHITE.POPULATION < 0.50 ~ "30-50%",
    NON.WHITE.POPULATION < 0.75 ~ "50-75%",
    TRUE ~ "larger than 75%"
  ))) %>% 
  mutate(pop_per_sqmi_cat = as.factor(case_when(
    POPULATION.PER.SQMI < 500 ~ "less_than_500",
    POPULATION.PER.SQMI < 5000 ~ "500-5000",
    POPULATION.PER.SQMI < 10000 ~ "5000-10000",
    TRUE ~ "larger_than_10000"
  ))) %>% 
  mutate(ave_household_size_cat = as.factor(case_when(
    AVERAGE.HOUSEHOLD.SIZE < 2 ~ "less_than_2",
    AVERAGE.HOUSEHOLD.SIZE < 3 ~ "2-3",
    AVERAGE.HOUSEHOLD.SIZE < 4 ~ "3-4",
    TRUE ~ "larger_than_4"
  ))) %>% 
  mutate(perc_renters_cat = as.factor(case_when(
    RENTERS < 0.3 ~ "less_than_0.3",
    RENTERS < 0.5 ~ "0.3-0.5",
    RENTERS < 0.7 ~ "0.5-0.7",
    TRUE ~ "larger_than_0.7"
  ))) %>% 
  mutate(perc_income_20000_less_cat = as.factor(case_when(
    X.20.000.or.less < 0.1 ~ "less_than_0.1",
    X.20.000.or.less < 0.2 ~ "0.1-0.2",
    X.20.000.or.less < 0.3 ~ "0.2-0.3",
    TRUE ~ "larger_than_0.3"
  )))
# Distribution of outbreadk -- Yes/No = 102/110
#======================= Joy's edits end here ===============================

# Final preprocessing
cleaned_data = 
  test_model_data %>% 
  select(outbreak_3d_lead, cumulative_cases_cat, non_white_cat,  age10_less_cat, high_school_orless_cat,  pop_per_sqmi_cat) 
cleaned_data = as.data.frame(cleaned_data)
# Delete the communities/rows that have nan values
cleaned_data_no_nan = na.omit(cleaned_data)

target_variable_index = 1
# plot network func
# using the visNetwork package to plot the network because it looks very nice.
plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}

#==========================
# Build the BNN model automatically using an algorithm

# Randomly shuffle before doing train-test splitting
cleaned_data_no_nan = cleaned_data_no_nan[sample(nrow(cleaned_data_no_nan)), ]

# First we set aside an independent testing set from the rest of the set which will be used for cross validation
cross_validation_index = createDataPartition(cleaned_data_no_nan$outbreak, p = percent_training, list = FALSE)

cross_validation_data = cleaned_data_no_nan[cross_validation_index, ]
independent_testing_data = cleaned_data_no_nan[-cross_validation_index, ]

# Perform cross validation
folds = cut(seq(1, nrow(cross_validation_data)), breaks = num_folds, labels = FALSE)
acc_folds = c()
sensitivity_folds = c()
specificity_folds = c()

for (fold_index in 1:num_folds){
  print("-----------------")
  print(paste("Cross validation on fold", fold_index))
  fold_testing_index = which(folds == fold_index, arr.ind = TRUE)
  fold_testing_data = cross_validation_data[fold_testing_index, ]
  fold_training_data = cross_validation_data[-fold_testing_index, ]
  fold_structure = hc(fold_training_data, score = 'bde')
  print(plot.network(fold_structure))
  bn.mod_fold = bn.fit(fold_structure, data = cleaned_data_no_nan)
  
  pred_classes = c()
  for (test_index in 1:nrow(fold_testing_data)){
    testing_sample = fold_testing_data[test_index, ]
    
    # Evidence is all the features of this sample.
    column_names = names(cleaned_data_no_nan)
    event_yes = paste("(", column_names[target_variable_index], " == 'Yes')", sep = "")
    event_no = paste("(", column_names[target_variable_index], " == 'No')", sep = "")
    
    evidence = paste("(", column_names[-target_variable_index], " == '",sapply(testing_sample[-target_variable_index], as.character), "')",sep = "", collapse = " & ") 
    
    # Make prediction at both values that the target variable can take on, 
    cmd_yes = paste("cpquery(bn.mod_fold, ", event_yes, ", ", evidence, ")", sep = "")
    cmd_no = paste("cpquery(bn.mod_fold, ", event_no, ", ", evidence, ")", sep = "")
    prediction_yes = eval(parse(text = cmd_yes))
    prediction_no = eval(parse(text = cmd_no))
    
    # and choose the value that gives maximum probability as the predicted class
    if (prediction_yes > prediction_no){
      pred_classes[test_index] = "Yes"
    }
    else{
      pred_classes[test_index] = "No"
    }
  }
  
  target_classes = as.numeric(fold_testing_data$outbreak)
  pred_classes_numeric = as.numeric(factor(pred_classes))
  
  # Calculate the evaluation metrics for this fold and save the values across folds
  acc_fold = mean(target_classes == pred_classes_numeric)
  sens_spec_fold = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="sens_spec", dnn = c("Outbreak", "No_outbreak"))

  acc_folds[fold_index] = acc_fold
  sensitivity_folds[fold_index] = sens_spec_fold$byClass["Sensitivity"]
  specificity_folds[fold_index] = sens_spec_fold$byClass["Specificity"]
  
  print(paste("The accuracy of the fold is", acc_fold))
  print(paste("The sensitivity of the fold is", sens_spec_fold$byClass["Sensitivity"]))
  print(paste("The specificity of the fold is", sens_spec_fold$byClass["Specificity"]))
  
  
  # sens_spec_fold = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="sens_spec", dnn = c("Outbreak", "No_outbreak"))
  # print(sens_spec_fold)
  
}
average_acc = mean(acc_folds)
average_sensitivity = mean(sensitivity_folds)
average_specificity = mean(specificity_folds)

print(paste("Average accuracy across folds", average_acc))
print(paste("Average sensitivity across folds", average_sensitivity))
print(paste("Average specificity across folds", average_specificity))

#==========================
# Now for the independent testing set

independent_test_structure = hc(independent_testing_data, score = 'bde')
plot.network(independent_test_structure)
bn.mod_indep_testing = bn.fit(independent_test_structure, data = independent_testing_data)

# Just like what we did in the testing phase of CV, we take each testing sample one by one and make a query on it.
indep_pred_classes = c()
for (test_index in 1:nrow(independent_testing_data)){
  indep_testing_sample = independent_testing_data[test_index, ]
  
  # Evidence is all the features of this sample.
  column_names = names(cleaned_data_no_nan)
  event_yes = paste("(", column_names[target_variable_index], " == 'Yes')", sep = "")
  event_no = paste("(", column_names[target_variable_index], " == 'No')", sep = "")
  
  indep_evidence = paste("(", column_names[-target_variable_index], " == '",sapply(indep_testing_sample[-target_variable_index], as.character), "')",sep = "", collapse = " & ") 
  
  # Make prediction at both values that the target variable can take on, 
  indep_cmd_yes = paste("cpquery(bn.mod_indep_testing, ", event_yes, ", ", indep_evidence, ")", sep = "")
  indep_cmd_no = paste("cpquery(bn.mod_indep_testing, ", event_no, ", ", indep_evidence, ")", sep = "")
  indep_prediction_yes = eval(parse(text = indep_cmd_yes))
  indep_prediction_no = eval(parse(text = indep_cmd_no))
  
  # and choose the value that gives maximum probability as the predicted class
  if (indep_prediction_yes > indep_prediction_no){
    indep_pred_classes[test_index] = "Yes"
  }
  else{
    indep_pred_classes[test_index] = "No"
  }
}

indep_target_classes = as.numeric(independent_testing_data$outbreak)
indep_pred_classes_numeric = as.numeric(factor(indep_pred_classes))

# Calculate the evaluation metrics for this fold and save the values across folds
indep_acc = mean(indep_target_classes == indep_pred_classes_numeric)
sens_spec_indep = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="sens_spec", dnn = c("Outbreak", "No_outbreak"))

print(paste("The accuracy on independent test set is", indep_acc))
print(paste("The sensitivity on independent test setis", sens_spec_indep$byClass["Sensitivity"]))
print(paste("The specificity on independent test set is", sens_spec_indep$byClass["Specificity"]))


# Example: outbreak tomorrow in North Hollywood
# cat("P(outbreak tomorrow | live at North Hollywood =", 
#     cpquery(bn.mod, (outbreak == "Yes"), (place_ID == "North Hollywood" & cumulative_cases_cat == "Less than 500" &
#                                             pop_per_sqmi_cat == "5000-10000")), "\n")
# 
# cat("P(outbreak tomorrow | sudo community =",
#     cpquery(bn.mod, (outbreak == "Yes"), (cumulative_cases_cat == "Less than 500" ), "\n"))


