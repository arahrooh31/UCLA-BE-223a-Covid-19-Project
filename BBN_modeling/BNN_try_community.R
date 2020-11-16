# Let's try some BNN
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies=TRUE)

library(tidyverse)
library(caret)

set.seed(1)
percent_training = 0.7
num_folds = 3
local_path = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/"
# app_path = paste0(local_path, "COVID_shinyLA/")
# cleaned_data_path = paste0(app_path, 'output_clean/')
community_today = read.csv(paste0(local_path, "community_todayUpdated.csv"))
cv_cases_longitudinal = read.csv(paste0(local_path, "community_TimeCases_long.csv"))
# N = 335 obs
community_demo = read.csv(paste0(local_path, "neighborhood_data_latimes.csv"))
# N = 265 obs

# Clean place_ID
community_demo_CleanID =
  community_demo %>% 
  mutate(place_ID_1 = gsub(pattern = "City of ", replacement = '', x = community_demo$Neighborhood)) 
community_demo_CleanID =
  community_demo_CleanID %>% 
  mutate(place_ID_2 = gsub(pattern = "Los Angeles - ", replacement = '', x = community_demo_CleanID$place_ID_1)) 
community_demo_CleanID =
  community_demo_CleanID %>% 
  mutate(place_ID_3 = gsub(pattern = "Unincorporated - ", replacement = '', x = community_demo_CleanID$place_ID_2))
community_demo_CleanID =
  community_demo_CleanID %>% 
  mutate(place_ID_4 = gsub(pattern = "[[:punct:] ]+", replacement = ' ', x = community_demo_CleanID$place_ID_3))
community_demo_CleanID =
  community_demo_CleanID %>% 
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = community_demo_CleanID$place_ID_4)) %>% 
  select(-c(place_ID_1, place_ID_2, place_ID_3, place_ID_4))


community_demo_CleanID %>% 
  group_by(place_ID) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)
# No duplicate place_ID

cv_cases_longitudinal_nodup =
  cv_cases_longitudinal %>% 
    filter(date == max(as.Date(cv_cases_longitudinal$date))) %>% 
    group_by(place_ID) %>% 
    summarise(n = n(), place_ID, cumulative_cases = sum(cases), new_cases = sum(new_cases)) %>% 
    distinct() %>% 
    ungroup()

# Merged demographic variables, data cleaning
merged_community = 
  cv_cases_longitudinal_nodup %>% 
  inner_join(community_demo_CleanID, by = c("place_ID" = "place_ID")) %>% 
  mutate(age10_less = as.numeric(gsub(pattern = "%", replacement = '', x = AGE.10.OR.LESS))/100) %>% 
  mutate(square_miles = as.numeric(SQUARE.MILES)) %>% 
  mutate(high_school_orless = as.numeric(gsub(pattern = "%", replacement = '', x = LESS.THAN.HIGH.SCHOOL))/100) %>% 
  mutate(high_school_diploma = as.numeric(gsub(pattern = "%", replacement = '', x = HIGH.SCHOOL.DIPLOMA))/100) %>% 
  mutate(diversity_index = as.numeric(DIVERSITY.INDEX)) %>% 
  mutate(non_white = as.numeric(gsub(pattern = "%", replacement = '', x = NON.WHITE.POPULATION))/100) %>% 
  mutate(avg_hh_size = as.numeric(AVERAGE.HOUSEHOLD.SIZE)) %>% 
  mutate(median_income = as.numeric(gsub(pattern = "[[:punct:] ]+", replacement = '', x = Median.Income))) %>% 
  mutate(pop_total = as.numeric(gsub(pattern = "[[:punct:] ]+", replacement = '', x = POPULATION.TOTAL))) %>% 
  mutate(pop_per_sqmi = as.numeric(gsub(pattern = "[[:punct:] ]+", replacement = '', x = POPULATION.PER.SQMI))) %>% 
  select(place_ID, cumulative_cases, new_cases, age10_less, square_miles, high_school_orless, high_school_diploma, diversity_index,
         non_white, avg_hh_size, median_income, pop_total, pop_per_sqmi)
# n = 210 obs

# merged_community %>% 
#   group_by(place_ID) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 1)
# No duplicates

test_model_data =
  merged_community %>% 
  mutate(outbreak = as.factor(case_when(
    new_cases >= 3 ~ "Yes",
    !is.na(new_cases) ~ "No"
  ))) %>% 
  mutate(cumulative_cases_cat = as.factor(case_when(
    cumulative_cases > 1000 ~ "1000_and_more",
    cumulative_cases > 500 ~ "500_to_1000",
    cumulative_cases > 0 ~ "Less_than_500"
  ))) %>% 
  mutate(place_ID = as.factor(place_ID)) %>% 
  mutate(age10_less_cat = as.factor(case_when(
    age10_less < 0.05 ~ "less_than_5%",
    age10_less < 0.15 ~ "5-15%",
    age10_less < 0.25 ~ "15-25%",
    TRUE ~ "larger_than_25%"
  ))) %>% 
  mutate(high_school_orless_cat = as.factor(case_when(
    high_school_orless < 0.05 ~ "less_than_5%",
    high_school_orless < 0.25 ~ "5-25%",
    high_school_orless < 0.50 ~ "25-55%",
    TRUE ~ "larger than 50%"
  ))) %>% 
  mutate(non_white_cat = as.factor(case_when(
    non_white < 0.15 ~ "less_than_15%",
    high_school_orless < 0.30 ~ "15-30%",
    high_school_orless < 0.50 ~ "30-50%",
    high_school_orless < 0.75 ~ "50-75%",
    TRUE ~ "larger than 75%"
  ))) %>% 
  mutate(pop_per_sqmi_cat = as.factor(case_when(
    pop_per_sqmi < 500 ~ "less_than_500",
    pop_per_sqmi < 5000 ~ "500-5000",
    pop_per_sqmi < 10000 ~ "5000-10000",
    TRUE ~ "larger_than_10000"
  )))
# Distribution of outbreadk -- Yes/No = 127/83

# Final preprocessing
cleaned_data = 
  test_model_data %>% 
  select(cumulative_cases_cat, outbreak, non_white_cat, age10_less_cat, high_school_orless_cat, pop_per_sqmi_cat) 
cleaned_data = as.data.frame(cleaned_data)
# Delete the communities/rows that have nan values
cleaned_data_no_nan = na.omit(cleaned_data)


# # plot network func
# # using the visNetwork package to plot the network because it looks very nice.
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
  plot.network(fold_structure)
  bn.mod_fold = bn.fit(fold_structure, data = cleaned_data_no_nan)
  
  pred_classes = c()
  for (test_index in 1:nrow(fold_testing_data)){
    testing_sample = fold_testing_data[test_index, ]
    
    # Evidence is all the features of this sample.
    column_names = names(cleaned_data_no_nan)
    event_yes = paste("(", column_names[2], " == 'Yes')", sep = "")
    event_no = paste("(", column_names[2], " == 'No')", sep = "")
    
    evidence = paste("(", column_names[-2], " == '",sapply(testing_sample[-2], as.character), "')",sep = "", collapse = " & ") 
    
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
  sensitivity_fold = sensitivity(factor(pred_classes_numeric), factor(target_classes), positive="2")
  specificity_fold = specificity(factor(pred_classes_numeric), factor(target_classes), positive="2")
  
  acc_folds[fold_index] = acc_fold
  sensitivity_folds[fold_index] = sensitivity_fold
  specificity_folds[fold_index] = specificity_fold
  
  print(paste("The accuracy of the fold is", acc_fold))
  print(paste("The sensitivity of the fold is", sensitivity_fold))
  print(paste("The specificity of the fold is", specificity_fold))
  
  
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
bn.mod_indep_testing

# Just like what we did in the testing phase of CV, we take each testing sample one by one and make a query on it.
indep_pred_classes = c()
for (test_index in 1:nrow(independent_testing_data)){
  indep_testing_sample = independent_testing_data[test_index, ]
  
  # Evidence is all the features of this sample.
  column_names = names(cleaned_data_no_nan)
  event_yes = paste("(", column_names[2], " == 'Yes')", sep = "")
  event_no = paste("(", column_names[2], " == 'No')", sep = "")
  
  indep_evidence = paste("(", column_names[-2], " == '",sapply(indep_testing_sample[-2], as.character), "')",sep = "", collapse = " & ") 
  
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
indep_sensitivity = sensitivity(factor(indep_pred_classes_numeric), factor(indep_target_classes), positive="2")
indep_specificity = specificity(factor(indep_pred_classes_numeric), factor(indep_target_classes), positive="2")
print(paste("The accuracy on independent test set is", indep_acc))
print(paste("The sensitivity on independent test setis", indep_sensitivity))
print(paste("The specificity on independent test set is", indep_specificity))


# Example: outbreak tomorrow in North Hollywood
# cat("P(outbreak tomorrow | live at North Hollywood =", 
#     cpquery(bn.mod, (outbreak == "Yes"), (place_ID == "North Hollywood" & cumulative_cases_cat == "Less than 500" &
#                                             pop_per_sqmi_cat == "5000-10000")), "\n")
# 
# cat("P(outbreak tomorrow | sudo community =",
#     cpquery(bn.mod, (outbreak == "Yes"), (cumulative_cases_cat == "Less than 500" ), "\n"))

