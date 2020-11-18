# Let's try some BNN
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")


set.seed(1)
num_folds = 5

local_path = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/BBN_modeling/"
community_today = read.csv(paste0(local_path, "community_todayUpdated.csv")) # N = 334 obs
cv_cases_longitudinal = read.csv(paste0(local_path, "community_TimeCases_long.csv")) 
community_demo = read.csv(paste0(local_path, "community_demographic_cleaned.csv")) # N = 265 obs
load(paste0(local_path, "merged_community_new.rda"))
# Merged demographic variables
merged_community = 
  cv_cases_longitudinal %>% 
  filter(date == max(as.Date(cv_cases_longitudinal$date)) - 10) %>% 
  # Here I used 10 days before just to make sure they have values for new_7d_lead
  inner_join(community_demo, by = c("place_ID" = "place_ID"))

# Calculate the proportion of age 0 to 34. This group is considered a young age group.
age_34_less_column = merged_community_new$AGE.10.OR.LESS + merged_community_new$AGE.11.TO.18 + merged_community_new$AGE.19.TO.34 
#================================================
# The preparation of sample dataset starts here #
#================================================
data =
  merged_community_new %>% 
  mutate(outbreak_7d_lead = as.factor(case_when(
    new_7d_lead >= 3 ~ "Yes",
    !is.na(new_7d_lead) ~ "No"
  ))) %>% 
  mutate(cumulative_cases = as.factor(case_when(
    cumulative_cases_today < 50 ~ "less_than_50",
    cumulative_cases_today < 100 ~ "50_to_100",
    cumulative_cases_today < 200 ~ "100_to_200",
    cumulative_cases_today < 300 ~ "200_to_300",
    cumulative_cases_today < 400 ~ "300_to_400",
    cumulative_cases_today < 500 ~ "400_to_500",
    TRUE ~ "larger_than_500"
  ))) %>% 
  mutate(new_cases_today = as.factor(case_when(
    new_cases_today < 2  ~ "0-2",
    new_cases_today < 5  ~ "2-5",
    new_cases_today < 10  ~ "5-10",    
    new_cases_today < 20  ~ "10-20",
    TRUE ~ "larger_than_20"
  ))) %>% 
  mutate(place_ID = as.factor(place_ID)) %>% 
  mutate(age34_less = as.factor(case_when(
    age_34_less_column < 0.4 ~ "less_than_40%",
    age_34_less_column < 0.5 ~ "40-50%",
    age_34_less_column < 0.6 ~ "50-60%",
    TRUE ~ "larger_than_60%"
  ))) %>% 
  mutate(high_school_orless = as.factor(case_when(
    LESS.THAN.HIGH.SCHOOL < 0.05 ~ "less_than_5%",
    LESS.THAN.HIGH.SCHOOL < 0.15 ~ "5-15%",
    LESS.THAN.HIGH.SCHOOL < 0.25 ~ "15-25%",
    LESS.THAN.HIGH.SCHOOL < 0.35 ~ "25-35%",
    TRUE ~ "larger than 35%"
  ))) %>% 
  mutate(non_white = as.factor(case_when(
    NON.WHITE.POPULATION < 0.15 ~ "less_than_15%",
    NON.WHITE.POPULATION < 0.30 ~ "15-30%",
    NON.WHITE.POPULATION < 0.50 ~ "30-50%",
    NON.WHITE.POPULATION < 0.75 ~ "50-75%",
    TRUE ~ "larger than 75%"
  ))) 

# Delete the communities/rows that have nan values
data_no_nan = na.omit(data)
# Count the frequency of community available on each day
date_community_freq = data.frame(table(data_no_nan$date))
date_community_freq_sufficient_n = data.frame(date_community_freq[-(1:2), ]) # Too few cases for the first 2 weeks. Delete them.


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

#================================================
# Cross validation function #
#================================================
run_cross_validation = function(structure, data, num_folds){
  target_variable_index = 1
  beta = 2
  folds = cut(seq(1, nrow(data)), breaks = num_folds, labels = FALSE)
  F1_folds = c()
  F2_folds = c()
  sensitivity_folds = c()
  specificity_folds = c()
  current_best_sensitivity = 0
  
  pred_positive_class_prob = c() # This variable stores all the predicted positive class probability of each sample. Each sample will be a testing sample exactly once throughout cross validation.
  for (fold_index in 1:num_folds){
    # print(paste("Cross validation on fold", fold_index))
    fold_testing_index = which(folds == fold_index, arr.ind = TRUE)
    fold_testing_data = data[fold_testing_index, ]
    
    fold_training_data = data[-fold_testing_index, ]
    
    bn.mod_fold = bn.fit(structure, data = fold_training_data)
    
    pred_classes = c()
    pred_test_samples_prob = c()
    for (test_index in 1:nrow(fold_testing_data)){
      testing_sample = fold_testing_data[test_index, ]
      
      # Evidence is all the features of this sample.
      column_names = names(fold_testing_data)
      event_yes = paste("(", column_names[target_variable_index], " == 'Yes')", sep = "")
      event_no = paste("(", column_names[target_variable_index], " == 'No')", sep = "")
      
      evidence = paste("(", column_names[-target_variable_index], " == '",sapply(testing_sample[-target_variable_index], as.character), "')",sep = "", collapse = " & ") 
      
      # Make prediction at both values that the target variable can take on, 
      cmd_yes = paste("cpquery(bn.mod_fold, ", event_yes, ", ", evidence, ")", sep = "")
      cmd_no = paste("cpquery(bn.mod_fold, ", event_no, ", ", evidence, ")", sep = "")
      prediction_yes = eval(parse(text = cmd_yes))
      prediction_no = eval(parse(text = cmd_no))
      pred_test_samples_prob[test_index] = prediction_yes
      
      # and choose the value that gives maximum probability as the predicted class
      if (prediction_yes > prediction_no){
        pred_classes[test_index] = "Yes"
      }
      else{
        pred_classes[test_index] = "No"
      }
    }
    
    pred_positive_class_prob = append(pred_positive_class_prob, pred_test_samples_prob, after = length(pred_positive_class_prob))
    target_classes = as.numeric(fold_testing_data$outbreak)
    pred_classes_numeric = as.numeric(factor(pred_classes))
    
    # Calculate the evaluation metrics for this fold and save the values across folds
    sens_spec_fold = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="everything", dnn = c("Outbreak", "No_outbreak"))
    
    sensitivity_folds[fold_index] = sens_spec_fold$byClass["Sensitivity"]
    specificity_folds[fold_index] = sens_spec_fold$byClass["Specificity"]
    F1_folds[fold_index] = sens_spec_fold$byClass["F1"]
    F2_folds[fold_index] = ((sens_spec_fold$byClass["Sensitivity"] * sens_spec_fold$byClass["Precision"]) * (1 + beta**2)) / ((beta**2 * sens_spec_fold$byClass["Precision"]) + sens_spec_fold$byClass["Sensitivity"])
    
    # Find the fold with the best sensitivity and save the model from that fold
    if (sensitivity_folds[fold_index] > current_best_sensitivity){
      current_best_sensitivity = sensitivity_folds[fold_index]
      current_best_fold = fold_index
      saved_best_model = bn.mod_fold
    }
    
  }
  average_F1 = mean(F1_folds)
  average_F2 = mean(F2_folds)
  average_sensitivity = mean(sensitivity_folds)
  average_specificity = mean(specificity_folds)
  
  # print(paste("Average sensitivity across folds", round(average_sensitivity, 3)))
  # print(paste("Average specificity across folds", round(average_specificity, 3)))
  # print(paste("Average F1 score across folds", round(average_F1, 3)))
  # save(saved_best_model, file = (paste0(local_path, "model_with_best_fold_performance_time_series.rda")))
  return (list(average_sensitivity, average_specificity, average_F1, average_F2))
}

#==============================================================
# Choose data from each week and do cross validation using a BBN #
#==============================================================
time_series_selected_features = c()
time_series_sensitivity = c()
time_series_specificity = c()
time_series_F1 = c()
time_series_F2 = c()
model_results_each_week = data.frame(model_features = character(), sensitivity = numeric(), specificity = numeric(), F1_score = numeric(), F2_score = numeric())
for (week_index in 1:nrow(date_community_freq_sufficient_n)){
  print("-------------------------------")
  print(paste("Working on week of", date_community_freq_sufficient_n$Var1[week_index]))
  
  # Select the data from a week
  data_no_nan_week = as.data.frame(subset(data_no_nan, date == as.character(date_community_freq_sufficient_n$Var1[week_index])))
  data_no_nan_week$date = NULL
  # Checking number of instances in each target class
  check_classes = table(data_no_nan_week$outbreak_7d_lead)
  print("Target class distribution")
  print(table(data_no_nan_week$outbreak_7d_lead))
  
  #================================================
  # Preparation of models with different features #
  
  # Set up 7 different combinations of features to be used in BBN. Cumulative_Case and new_cases_today will exist in every combo, but age, education, race will permute.
  data_covidCases_education = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, high_school_orless)
  data_covidCases_education = as.data.frame(data_covidCases_education)
  
  data_covidCases_age = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, age34_less)
  data_covidCases_age = as.data.frame(data_covidCases_age)
  
  data_covidCases_race = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, non_white)
  data_covidCases_race = as.data.frame(data_covidCases_race)
  
  data_covidCases_education_race = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, high_school_orless, non_white)
  data_covidCases_education_race = as.data.frame(data_covidCases_education_race)
  
  data_covidCases_education_age = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, high_school_orless, age34_less)
  data_covidCases_education_age = as.data.frame(data_covidCases_education_age)
  
  data_covidCases_age_race = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, age34_less, non_white)
  data_covidCases_age_race = as.data.frame(data_covidCases_age_race)
  
  data_covidCases_education_age_race = 
    data_no_nan_week %>% 
    select(outbreak_7d_lead, cumulative_cases, new_cases_today, high_school_orless, age34_less, non_white)
  data_covidCases_education_age_race = as.data.frame(data_covidCases_education_age_race)
  
  # Randomly shuffle before doing train-test splitting
  data_covidCases_education = data_covidCases_education[sample(nrow(data_covidCases_education)), ]
  data_covidCases_age = data_covidCases_age[sample(nrow(data_covidCases_age)), ]
  data_covidCases_race = data_covidCases_race[sample(nrow(data_covidCases_race)), ]
  data_covidCases_education_race = data_covidCases_education_race[sample(nrow(data_covidCases_education_race)), ]
  data_covidCases_education_age = data_covidCases_education_age[sample(nrow(data_covidCases_education_age)), ]
  data_covidCases_age_race = data_covidCases_age_race[sample(nrow(data_covidCases_age_race)), ]
  data_covidCases_education_age_race = data_covidCases_education_age_race[sample(nrow(data_covidCases_education_age_race)), ]
  
  
  structure_covidCases_education = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "high_school_orless"))
  modelstring(structure_covidCases_education) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|high_school_orless][high_school_orless]")
  
  structure_covidCases_age = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "age34_less"))
  modelstring(structure_covidCases_age) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|age34_less][age34_less]")
  
  structure_covidCases_race = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "non_white"))
  modelstring(structure_covidCases_race) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|non_white][non_white]")
  
  structure_covidCases_education_age = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "high_school_orless", "age34_less"))
  modelstring(structure_covidCases_education_age) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|high_school_orless:age34_less][high_school_orless][age34_less]")
  
  structure_covidCases_education_race = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "high_school_orless", "non_white"))
  modelstring(structure_covidCases_education_race) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|high_school_orless:non_white][high_school_orless|non_white][non_white]")
  
  structure_covidCases_age_race = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "age34_less", "non_white"))
  modelstring(structure_covidCases_age_race) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|age34_less:non_white][age34_less][non_white]")
  
  structure_covidCases_education_age_race = empty.graph(c("outbreak_7d_lead", "cumulative_cases", "new_cases_today", "high_school_orless", "age34_less", "non_white"))
  modelstring(structure_covidCases_education_age_race) = paste0("[outbreak_7d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases][cumulative_cases|high_school_orless:non_white:age34_less][high_school_orless|non_white][non_white][age34_less]")

    # Preparation of models with different features ends here#
  #======================================================
  
  # print(plot.network(structure))
  # Run cross validation on each model with different combinations of features
  cv_results_education = run_cross_validation(structure_covidCases_education, data_covidCases_education, num_folds)
  cv_results_age = run_cross_validation(structure_covidCases_age, data_covidCases_age, num_folds)
  cv_results_race = run_cross_validation(structure_covidCases_race, data_covidCases_race, num_folds)
  cv_results_education_age = run_cross_validation(structure_covidCases_education_age, data_covidCases_education_age, num_folds)
  cv_results_education_race = run_cross_validation(structure_covidCases_education_race, data_covidCases_education_race, num_folds)
  cv_results_age_race = run_cross_validation(structure_covidCases_age_race, data_covidCases_age_race, num_folds)
  cv_results_education_age_race = run_cross_validation(structure_covidCases_education_age_race, data_covidCases_education_age_race, num_folds)
  
  # Save the results produced by model with different features.
  model_results_each_week[1, ] = c("education", cv_results_education[1], cv_results_education[2], cv_results_education[3], cv_results_education[4])
  model_results_each_week[2, ] = c("age", cv_results_age[1], cv_results_age[2], cv_results_age[3], cv_results_age[4])
  model_results_each_week[3, ] = c("race", cv_results_race[1], cv_results_race[2], cv_results_race[3], cv_results_race[4])
  model_results_each_week[4, ] = c("education_age", cv_results_education_age[1], cv_results_education_age[2], cv_results_education_age[3], cv_results_education_age[4])
  model_results_each_week[5, ] = c("education_race", cv_results_education_race[1], cv_results_education_race[2], cv_results_education_race[3], cv_results_education_race[4])
  model_results_each_week[6, ] = c("age_race", cv_results_age_race[1], cv_results_age_race[2], cv_results_age_race[3], cv_results_age_race[4])
  model_results_each_week[7, ] = c("education_age_race", cv_results_education_age_race[1], cv_results_education_age_race[2], cv_results_education_age_race[3], cv_results_education_age_race[4])
  
  # Find the feature combo that gives the best sensitivity, and then save the sensitivity, specificity, f1 score of that combo
  max_sensi_index = which.max(model_results_each_week$sensitivity)
  time_series_selected_features[week_index] = model_results_each_week$model_features[max_sensi_index]
  time_series_sensitivity[week_index] = model_results_each_week$sensitivity[max_sensi_index]
  time_series_specificity[week_index] = model_results_each_week$specificity[max_sensi_index]
  time_series_F1[week_index] = model_results_each_week$F1_score[max_sensi_index]
  time_series_F2[week_index] = model_results_each_week$F2_score[max_sensi_index]
  
  print(paste("The feature(s) that gives the best sensitivity:", model_results_each_week$model_features[max_sensi_index]))
  print(paste("Sensitivity of this week's model is", round(model_results_each_week$sensitivity[max_sensi_index], 3)))

}

save_time_series_model_performance = data.frame(date_community_freq_sufficient_n$Var1, time_series_selected_features, time_series_sensitivity, time_series_specificity, time_series_F1, time_series_F2)
save_time_series_model_performance = 
  save_time_series_model_performance %>%
  rename(date = date_community_freq_sufficient_n.Var1)
save(save_time_series_model_performance, file = (paste0(local_path, "save_time_series_model_performance.rda")))

#================================================
# Some small calculations #
#================================================
# corr_20000_less = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.20.000.or.less, method = 'spearman')
# corr_20000_40000 = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.20.000.to..40.000, method = 'spearman')
# corr_40000_60000 = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.40.000.to..60.000, method = 'spearman')
# corr_60000_125000 = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.60.000.to..125.000, method = 'spearman')
# corr_125000_up = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.125.000.and.up, method = 'spearman')

# x = c(0.556, 0.882, 0.9, 1, 0.857)
# y = c(0.593, 0.882, 0.9, 1, 0.857) # high school
# z = c(0.63, 0.882, 0.85, 1, 0.81) # age
# wilcoxon_result_1 = wilcox.test(y, z)  # Comparing high school vs age


