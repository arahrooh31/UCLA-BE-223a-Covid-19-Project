# Let's try some BNN
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages('funModeling', repos = "http://cran.us.r-project.org")

set.seed(1)
num_folds = 5
local_path = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/BBN_modeling/"
# Merged demographic variables
load("/Users/rinading/Desktop/new/data/modeling/merged_community_expand.rda")

# Also read in the interpolated data for individual risk prediction later
estimated_infection_race = read.csv("/Users/rinading/Desktop/new/data/dynamic/locations_demographics/2020-12-07/race-ethnicity_interpolated.csv") # For each race category, the proportion of infected people in that category
estimated_infection_age = read.csv("/Users/rinading/Desktop/new/data/dynamic/locations_demographics/2020-12-07/AgeInterpolationFinalDF.csv") # For each age category, the proportion of infected people in that category

# For each community, exclude the first report of cumulative cases that is not zero. Essentially delete that row of community. The first non-zero COVID cases report doesn't necessarily mean that there was no case before that day.
# We do this by sorting the data by community names, and then loop through each set of community and record the index needed to be deleted.
# merged_community_sort_community = merged_community_no_nan[with(merged_community_no_nan, order(place_ID)), ]
# unique_community = unique(unlist(merged_community_sort_community$place_ID))
# to_be_deleted_community_rows = c()
# for (i in 1:length(unique_community)){
#   community_subset = subset(merged_community_sort_community, place_ID == unique_community[i])
#   for (week_index in 1:nrow(community_subset)){
#     if (community_subset$cumulative_cases_today[week_index] != 0){
#       to_be_deleted_community_rows = c(to_be_deleted_community_rows, rownames(community_subset)[week_index + 1])
#       break
#     }
#   }
# }
# to_be_deleted_community_rows = na.omit(to_be_deleted_community_rows)
# 
# # The data after excluding the first non-zero case report.
# merged_community_no_nan = merged_community_no_nan[-c(as.numeric(to_be_deleted_community_rows)), ]


# Calculate the proportion of age 0 to 34. This group is considered a young age group.
age_34_less_column = merged_community_expand$AGE.10.OR.LESS + merged_community_expand$AGE.11.TO.18 + merged_community_expand$AGE.19.TO.34 
merged_community_expand = cbind(merged_community_expand, age_34_less_column)

#================================================
# The preparation of sample dataset starts here #
#================================================
# Choose the features we want to use in the model.
data = 
  merged_community_expand %>% 
  mutate(outbreak_7d_lead = as.factor(case_when(
    new_7d_lead >= 3 ~ "Yes",
    !is.na(new_7d_lead ) ~ "No"
  ))) 
data = as.data.frame(data)

d_bins_community_no_interpolation = discretize_get_bins(data = data, n_bins=3)
save(d_bins_community_no_interpolation, file = (paste0(local_path, "discretization_params_community_no_interpolation.rda")))
data = discretize_df(data = data, data_bins = d_bins_community_no_interpolation, stringsAsFactors=T)
data = as.data.frame(data)

# Delete the communities/rows that have nan values
data_no_nan = na.omit(data)
# Count the frequency of community available on each day
date_community_freq = data.frame(table(data_no_nan $date))
date_community_freq_sufficient_n = data.frame(date_community_freq) 

data_no_nan = 
  data_no_nan  %>%
  rename(past_cases_3d = new_3d_MA_pop_adj, age34_less = age_34_less_column, high_school_orless = LESS.THAN.HIGH.SCHOOL, non_white = NON.WHITE.POPULATION)

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
    fold_testing_index = which(folds == fold_index, arr.ind = TRUE)
    fold_testing_data = data[fold_testing_index, ]
    
    fold_training_data = data[-fold_testing_index, ]
    
    bn.mod_fold = bn.fit(structure, data = fold_training_data, method = "bayes")
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
    target_classes = as.numeric(fold_testing_data[, target_variable_index])
    pred_classes_numeric = as.numeric(factor(pred_classes))
    
    # Calculate the evaluation metrics for this fold and save the values across folds
    sens_spec_fold = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="everything", dnn = c("Outbreak", "No_outbreak"))
    # print(sens_spec_fold)
    sensitivity_folds[fold_index] = sens_spec_fold$byClass["Sensitivity"]
    specificity_folds[fold_index] = sens_spec_fold$byClass["Specificity"]
    F1_folds[fold_index] = sens_spec_fold$byClass["F1"]
    F2_folds[fold_index] = ((sens_spec_fold$byClass["Sensitivity"] * sens_spec_fold$byClass["Precision"]) * (1 + beta**2)) / ((beta**2 * sens_spec_fold$byClass["Precision"]) + sens_spec_fold$byClass["Sensitivity"])
    
    #Find the fold with the best sensitivity and save the model from that fold
    if (sensitivity_folds[fold_index] >= current_best_sensitivity){
      current_best_sensitivity = sensitivity_folds[fold_index]
      current_best_fold = fold_index
      saved_best_model = bn.mod_fold
    }
    
  }
  average_F1 = round(mean(na.omit(F1_folds)), 3)
  average_F2 = round(mean(na.omit(F2_folds)), 3)
  average_sensitivity = round(mean(na.omit(sensitivity_folds)), 3)
  average_specificity = round(mean(na.omit(specificity_folds)), 3)
  
  # print(paste("Average sensitivity across folds", round(average_sensitivity, 3)))
  # print(paste("Average specificity across folds", round(average_specificity, 3)))
  # print(paste("Average F1 score across folds", round(average_F1, 3)))
  return (list(average_sensitivity, average_specificity, average_F1, average_F2, pred_positive_class_prob, saved_best_model))
}

#==============================================================
# Choose data from each time window and do cross validation using a BBN #
#==============================================================
time_series_sensitivity = data.frame(Sensitivity = numeric())
time_series_specificity = data.frame(Specificity = numeric())
time_series_F1 = data.frame(F1_score = numeric())
time_series_F2 = data.frame(F2_score = numeric())

for (week_index in 1:nrow(date_community_freq_sufficient_n)){
  print("-------------------------------")
  print(paste("Working on time window of", date_community_freq_sufficient_n$Var1[week_index]))
  
  # Select the data from a time window
  data_no_nan_week = as.data.frame(subset(data_no_nan, date == as.character(date_community_freq_sufficient_n$Var1[week_index])))
  
  data_no_nan_week$date = NULL
  # Checking number of instances in each target class
  check_classes = table(data_no_nan_week$outbreak_7d_lead)
  print("Target class distribution")
  print(table(data_no_nan_week$outbreak_7d_lead))
  
  data_covidCases_education_age_race = 
    data_no_nan_week %>%
    select(outbreak_7d_lead, cumulative_3w_pop_adj, past_cases_3d, high_school_orless, age34_less, non_white) 
    
  
  # Randomly shuffle before doing train-test splitting
  data_covidCases_education_age_race = data_covidCases_education_age_race[sample(nrow(data_covidCases_education_age_race)), ]
  
  structure_covidCases_education_age_race = empty.graph(c("outbreak_7d_lead", "cumulative_3w_pop_adj", "past_cases_3d", "high_school_orless", "age34_less", "non_white"))
  modelstring(structure_covidCases_education_age_race) = paste0("[outbreak_7d_lead|cumulative_3w_pop_adj:past_cases_3d][past_cases_3d|cumulative_3w_pop_adj][cumulative_3w_pop_adj|high_school_orless:non_white:age34_less][high_school_orless|non_white][non_white][age34_less]")
  
  # Run cross validation using the given features
  cv_results_education_age_race = run_cross_validation(structure_covidCases_education_age_race, data_covidCases_education_age_race, num_folds)
  
  # Save the performance
  time_series_sensitivity[week_index, ] = cv_results_education_age_race[[1]]
  time_series_specificity[week_index, ] = cv_results_education_age_race[[2]]
  time_series_F1[week_index, ] = cv_results_education_age_race[[3]]
  time_series_F2[week_index, ] = cv_results_education_age_race[[4]]
  
  # print(paste("Sensitivity of this week's model is", cv_results_education_age_race[1]))
  
}
community_best_model = cv_results_education_age_race[[6]]
save(community_best_model, file = (paste0(local_path, "community_model_best_fold_sensitivity_latest_time_window.rda")))

save_time_series_model_performance = data.frame(date_community_freq_sufficient_n$Var1, time_series_sensitivity, time_series_specificity, time_series_F1, time_series_F2)
save_time_series_model_performance = 
  save_time_series_model_performance %>%
  rename(date = date_community_freq_sufficient_n.Var1)
save(save_time_series_model_performance, file = (paste0(local_path, "save_time_series_model_performance.rda")))

#===========================================================================================================
# Incorporating interpolated age and race data with the latest community data to predict outbreak risk #
#===========================================================================================================
# Delete the communities/rows that have nan values
merged_community_no_nan = na.omit(merged_community_expand, cols = "population")
estimated_infection_age = na.omit(estimated_infection_age)
estimated_infection_race = na.omit(estimated_infection_race)

# First select the community data from the latest time window
community_latest = as.data.frame(subset(data_no_nan, date == max(as.Date(merged_community_expand$date))))
# Sort the interpolated data by community names
estimated_infection_race = estimated_infection_race[with(estimated_infection_race, order(CITY)), ]
estimated_infection_age = estimated_infection_age[with(estimated_infection_age, order(Place)), ]

# In case we want to reduce the number of interpolated age or race related variables
age_49_less_infected = estimated_infection_age$rscore_18OrLess + estimated_infection_age$rscore_19to49
age_50_more_infected = estimated_infection_age$rscore_50to65 + estimated_infection_age$rscore_65andUp
estimated_infection_age = cbind(estimated_infection_age, age_49_less_infected, age_50_more_infected)

merge_interpolated_age_race = 
  estimated_infection_age %>% 
  inner_join(estimated_infection_race, by = c("Place" = "CITY"))%>% 
  rename(Asian_infected = ASIAN, Black_infected = BLACK, Latino_infected = LATINO, White_infected = WHITE, Non_white_infected = NON.WHITE, age_18_less_infected = rscore_18OrLess, age_19_to_49_infected = rscore_19to49, age_50_to_65_infected = rscore_50to65, age_65_up_infected = rscore_65andUp)

save(merge_interpolated_age_race, file = (paste0(local_path, "merge_interoplated_age_race.rda")))

# Merge the interpolated data with the community level data from the latest time window
merge_community_interpolated =  
  community_latest %>% 
  inner_join(merge_interpolated_age_race, by = c("place_ID" = "Place")) %>% # Then take the intersection between interpolated data and community data
  # select(outbreak_7d_lead, cumulative_3w_pop_adj, past_cases_3d, high_school_orless, age34_less, non_white, Asian_infected, Black_infected, Latino_infected, White_infected, Non_white_infected, age_18_less_infected, age_19_to_49_infected, age_50_to_65_infected, age_65_up_infected)
  select(outbreak_7d_lead, cumulative_3w_pop_adj, past_cases_3d, high_school_orless, age34_less, non_white, age_49_less_infected, age_50_more_infected, Non_white_infected, White_infected)

d_bins_community_with_interpolation = discretize_get_bins(data = merge_community_interpolated, n_bins=3)
save(d_bins_community_with_interpolation, file = (paste0(local_path, "discretization_params_community_with_interpolation.rda")))
merge_community_interpolated_discrete = discretize_df(data = merge_community_interpolated, data_bins = d_bins_community_with_interpolation, stringsAsFactors=T)
merge_community_interpolated_discrete = as.data.frame(merge_community_interpolated_discrete)

# Randomly shuffle before doing train-test splitting
merge_community_interpolated_discrete = merge_community_interpolated_discrete[sample(nrow(merge_community_interpolated_discrete)), ]

structure_community_interpolated = empty.graph(c("outbreak_7d_lead", "cumulative_3w_pop_adj", "past_cases_3d", "high_school_orless", "age34_less", "non_white", "age_49_less_infected", "age_50_more_infected", "Non_white_infected", "White_infected"))
modelstring(structure_community_interpolated) = paste0("[outbreak_7d_lead|cumulative_3w_pop_adj:past_cases_3d:age_49_less_infected:Non_white_infected:age_50_more_infected:White_infected]",
                                                       "[past_cases_3d|cumulative_3w_pop_adj][cumulative_3w_pop_adj|high_school_orless:non_white:age34_less][high_school_orless|non_white][non_white][age34_less]",
                                                       "[age_49_less_infected][Non_white_infected][age_50_more_infected][White_infected]")

# structure_community_interpolated = empty.graph(c("outbreak_7d_lead", "cumulative_3w_pop_adj", "past_cases_3d", "high_school_orless", "age34_less", "non_white", "Asian_infected", "Black_infected", "Latino_infected", "White_infected", "Non_white_infected", "age_18_less_infected", "age_19_to_49_infected", "age_50_to_65_infected", "age_65_up_infected"))
# modelstring(structure_community_interpolated) = paste0("[outbreak_7d_lead|cumulative_3w_pop_adj:past_cases_3d:Asian_infected:Black_infected:Latino_infected:White_infected:Non_white_infected:age_18_less_infected:age_19_to_49_infected:age_50_to_65_infected:age_65_up_infected]",
#                                                        "[past_cases_3d|cumulative_3w_pop_adj][cumulative_3w_pop_adj|high_school_orless:non_white:age34_less][high_school_orless|non_white][non_white][age34_less]",
#                                                        "[Asian_infected][Black_infected][Latino_infected][White_infected][Non_white_infected][age_18_less_infected][age_19_to_49_infected][age_50_to_65_infected][age_65_up_infected]")

plot.network(structure_community_interpolated)

# Run cross validation using the given features
cv_results_merge_community_interpolated = run_cross_validation(structure_community_interpolated, merge_community_interpolated_discrete, num_folds)

# Save the performance
sensitivity_latest_time_window_with_interpolation = cv_results_merge_community_interpolated[[1]]
specificity_latest_time_window_with_interpolation= cv_results_merge_community_interpolated[[2]]
F1_latest_time_window_with_interpolation = cv_results_merge_community_interpolated[[3]]
F2_latest_time_window_with_interpolation = cv_results_merge_community_interpolated[[4]]

community_interpolation_best_model = cv_results_merge_community_interpolated[[6]]
save(community_interpolation_best_model, file = (paste0(local_path, "community_model_best_fold_sensitivity_latest_with_interpolation.rda")))

