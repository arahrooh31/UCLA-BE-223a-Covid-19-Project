# Let's try some BNN
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")


set.seed(1)
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

# Calculate the proportion of age 0 to 34. This group is considered a young age group.
age_34_less_column = merged_community$AGE.10.OR.LESS + merged_community$AGE.11.TO.18 + merged_community$AGE.19.TO.34 
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
  mutate(cumulative_cases = as.factor(case_when(
    cumulative_cases_today > 1000 ~ "1000_and_more",
    cumulative_cases_today > 500 ~ "500_to_1000",
    cumulative_cases_today > 0 ~ "Less_than_500"
  ))) %>% 
  mutate(new_cases_today = as.factor(case_when(
    new_cases_today < 5  ~ "0-5",
    new_cases_today < 10  ~ "5-10",    
    new_cases_today < 20  ~ "10-20",
    TRUE ~ "larger_than_20"
  ))) %>% 
  mutate(place_ID = as.factor(place_ID)) %>% 
  mutate(age10_less = as.factor(case_when(
    AGE.10.OR.LESS < 0.05 ~ "less_than_5%",
    AGE.10.OR.LESS < 0.1 ~ "5-10%",
    AGE.10.OR.LESS < 0.2 ~ "10-20%",
    TRUE ~ "larger_than_20%"
  ))) %>% 
  mutate(age19_to_34 = as.factor(case_when(
    AGE.19.TO.34 < 0.1 ~ "less_than_10%",
    AGE.19.TO.34 < 0.2 ~ "10-20%",
    AGE.19.TO.34 < 0.25 ~ "20-25%",
    AGE.19.TO.34 < 0.30 ~ "25-30%",
    TRUE ~ "larger_than_30%"
  ))) %>% 
  # mutate(age_34_less_proportion = as.factor(age_34_less))%>% 
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
  mutate(masters_degree_orhigher = as.factor(case_when(
    MASTERS.DEGREE.OR.HIGHER < 0.05 ~ "less_than_5%",
    MASTERS.DEGREE.OR.HIGHER < 0.15 ~ "5-15%",
    MASTERS.DEGREE.OR.HIGHER < 0.25 ~ "15-25%",
    TRUE ~ "larger than 25%"
  ))) %>% 
  mutate(non_white = as.factor(case_when(
    NON.WHITE.POPULATION < 0.15 ~ "less_than_15%",
    NON.WHITE.POPULATION < 0.30 ~ "15-30%",
    NON.WHITE.POPULATION < 0.50 ~ "30-50%",
    NON.WHITE.POPULATION < 0.75 ~ "50-75%",
    TRUE ~ "larger than 75%"
  ))) %>% 
  mutate(foreign_born = as.factor(case_when(
    FOREIGN.BORN.POPULATION < 0.10 ~ "less_than_10%",
    FOREIGN.BORN.POPULATION < 0.20 ~ "10-20%",
    FOREIGN.BORN.POPULATION < 0.30 ~ "20-30%",
    FOREIGN.BORN.POPULATION < 0.40 ~ "30-40%",
    TRUE ~ "larger than 40%"
  ))) %>% 
  mutate(pop_per_sqmi = as.factor(case_when(
    POPULATION.PER.SQMI < 500 ~ "less_than_500",
    POPULATION.PER.SQMI < 5000 ~ "500-5000",
    POPULATION.PER.SQMI < 10000 ~ "5000-10000",
    TRUE ~ "larger_than_10000"
  ))) %>% 
  mutate(ave_household_size = as.factor(case_when(
    AVERAGE.HOUSEHOLD.SIZE < 2 ~ "less_than_2",
    AVERAGE.HOUSEHOLD.SIZE < 3 ~ "2-3",
    AVERAGE.HOUSEHOLD.SIZE < 4 ~ "3-4",
    TRUE ~ "larger_than_4"
  ))) %>% 
  mutate(perc_renters = as.factor(case_when(
    RENTERS < 0.3 ~ "less_than_0.3",
    RENTERS < 0.5 ~ "0.3-0.5",
    RENTERS < 0.7 ~ "0.5-0.7",
    TRUE ~ "larger_than_0.7"
  ))) %>% 
  mutate(perc_income_20_to_40k = as.factor(case_when(
    X.20.000.to..40.000 < 0.1 ~ "less_than_0.1",
    X.20.000.to..40.000 < 0.2 ~ "0.1-0.2",
    X.20.000.to..40.000 < 0.3 ~ "0.2-0.3",
    TRUE ~ "larger_than_0.3"
  )))
# Distribution of outbreadk -- Yes/No = 102/110
#======================= Joy's edits end here ===============================

# Final preprocessing
cleaned_data = 
  test_model_data %>% 
  # select(outbreak_3d_lead, ave_household_size, cumulative_cases, new_cases_today, pop_per_sqmi, foreign_born, perc_renters, age34_less, high_school_orless, perc_income_20_to_40k, non_white) 
  select(outbreak_3d_lead, cumulative_cases, new_cases_today, age34_less)
cleaned_data = as.data.frame(cleaned_data)
# Delete the communities/rows that have nan values
cleaned_data_no_nan = na.omit(cleaned_data)

# Checking number of instances in each target class
check_classes = count(cleaned_data_no_nan, 'outbreak_3d_lead')
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

# Randomly shuffle before doing train-test splitting
cleaned_data_no_nan = cleaned_data_no_nan[sample(nrow(cleaned_data_no_nan)), ]

# Manually build the DAG of BBN and learn CPD tables in each fold
# structure = empty.graph(c("outbreak_3d_lead", "cumulative_cases", "new_cases_today", "ave_household_size", "pop_per_sqmi", "foreign_born", "perc_renters", "age34_less", "perc_income_20_to_40k", "high_school_orless", "non_white"))
# modelstring(structure) = paste0("[outbreak_3d_lead|cumulative_cases:new_cases_today][new_cases_today|cumulative_cases]",
#                                 "[cumulative_cases|ave_household_size:pop_per_sqmi:foreign_born:perc_renters:age34_less][ave_household_size][pop_per_sqmi][foreign_born]",
#                                 "[perc_renters|pop_per_sqmi:foreign_born:age34_less:perc_income_20_to_40k][age34_less]",
#                                 "[perc_income_20_to_40k|high_school_orless]",
#                                 "[high_school_orless|non_white][non_white]")

structure = empty.graph(c("outbreak_3d_lead", "cumulative_cases", "new_cases_today", "age34_less"))
modelstring(structure) = paste0("[outbreak_3d_lead|cumulative_cases:new_cases_today][cumulative_cases][new_cases_today|cumulative_cases][cumulative_cases|age34_less][age34_less]")
print(plot.network(structure))


# Perform cross validation
folds = cut(seq(1, nrow(cleaned_data_no_nan)), breaks = num_folds, labels = FALSE)
acc_folds = c()
sensitivity_folds = c()
specificity_folds = c()
pred_positive_class_prob = c() # This variable stores all the predicted positive class probability of each sample. Each sample will be a testing sample exactly once throughout cross validation.
for (fold_index in 1:num_folds){
  print("-----------------")
  print(paste("Cross validation on fold", fold_index))
  fold_testing_index = which(folds == fold_index, arr.ind = TRUE)
  fold_testing_data = cleaned_data_no_nan[fold_testing_index, ]
  fold_training_data = cleaned_data_no_nan[-fold_testing_index, ]
  # print(paste("Target class distribution on testing set", count(fold_testing_data, 'outbreak_3d_lead')))
  
  bn.mod_fold = bn.fit(structure, data = fold_training_data)
  
  pred_classes = c()
  pred_test_samples_prob = c()
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
  sens_spec_fold = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="sens_spec", dnn = c("Outbreak", "No_outbreak"))

  acc_folds[fold_index] = mean(target_classes == pred_classes_numeric)
  sensitivity_folds[fold_index] = sens_spec_fold$byClass["Sensitivity"]
  specificity_folds[fold_index] = sens_spec_fold$byClass["Specificity"]
  
  print(paste("The accuracy of the fold is", mean(target_classes == pred_classes_numeric)))
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


# corr_20000_less = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.20.000.or.less, method = 'spearman')
# corr_20000_40000 = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.20.000.to..40.000, method = 'spearman')
# corr_40000_60000 = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.40.000.to..60.000, method = 'spearman')
# corr_60000_125000 = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.60.000.to..125.000, method = 'spearman')
# corr_125000_up = cor.test(x=merged_community$new_3d_lead, y=merged_community$X.125.000.and.up, method = 'spearman')





