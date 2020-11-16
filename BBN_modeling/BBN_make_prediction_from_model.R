# This script makes predictions on the risk of an individual getting COVID. No evaluation or testing exists.
# First, this script predicts the risk of outbreak of a community in 3 days given the COVID stats on the current day using a saved BBN model. 
# Then, the predicted outbreak risk is combined with individual level demographics to predict a person's risk of getting COVID.

if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")

local_path = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/BBN_modeling/"
load(paste0(local_path, "merged_community_new.rda"))
load(paste0(local_path, "model_with_best_fold_performance.rda")) # In this saved model, the target variable is "outbreak_3d_lead".

#================================================
# Preprocessing of the data #
#================================================
# Get the latest day available
community_data_latest_day = 
  merged_community_new  %>% 
  filter(date == max(as.Date(merged_community_new$date)))

age_34_less_column = community_data_latest_day$AGE.10.OR.LESS + community_data_latest_day$AGE.11.TO.18 + community_data_latest_day$AGE.19.TO.34 

community_data_latest_day = 
  community_data_latest_day %>% 
  mutate(new_cases_today = as.factor(case_when(
    new_cases_today < 5  ~ "0-5",
    new_cases_today < 10  ~ "5-10",    
    new_cases_today < 20  ~ "10-20",
    TRUE ~ "larger_than_20"
  ))) %>% 
  mutate(cumulative_cases = as.factor(case_when(
    cumulative_cases_today > 1000 ~ "1000_and_more",
    cumulative_cases_today > 500 ~ "500_to_1000",
    cumulative_cases_today > 0 ~ "Less_than_500"
  ))) %>% 
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
  ))) 

community_data_latest_day_selected_variables = 
  community_data_latest_day %>% 
  select(cumulative_cases, new_cases_today,  high_school_orless)

# Delete the communities/rows that have nan values
community_data_latest_day_selected_variables_no_nan = na.omit(community_data_latest_day_selected_variables)

#================================================
# Make a prediction on each community's risk of outbreak #
#================================================
target_variable_index = 1 # Assuming the target variable is at the first index
pred_test_samples_prob = c()
pred_classes = c()

for (test_index in 1:nrow(community_data_latest_day_selected_variables_no_nan)){
  testing_sample = community_data_latest_day_selected_variables_no_nan[test_index, ]
  
  # Evidence is all the features of this sample.
  column_names = names(community_data_latest_day_selected_variables_no_nan)
  event_yes = paste("(", nodes(saved_best_model)[target_variable_index], " == 'Yes')", sep = "") 
  event_no = paste("(", nodes(saved_best_model)[target_variable_index], " == 'No')", sep = "") 
  
  evidence = paste("(", nodes(saved_best_model)[-target_variable_index], " == '",sapply(testing_sample, as.character), "')",sep = "", collapse = " & ") 

  # Make prediction at both values that the target variable can take on, 
  cmd_yes = paste("cpquery(saved_best_model, ", event_yes, ", ", evidence, ")", sep = "")
  cmd_no = paste("cpquery(saved_best_model, ", event_no, ", ", evidence, ")", sep = "")
  
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
# Save the place_ID and its according predicted probability or risk of outbreak
save_place_ID_and_outbreak_risk = data.frame(community_data_latest_day$place_ID, pred_test_samples_prob)

# Check the occurrences of predicted category
table(pred_classes)

#================================================
# Build the model for individual level risk prediction #
#================================================
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
# First, we automatically learn the conditional probability tables between the relationship outbreak-->cumulative_cases
pred_outbreak_risk = 
  data.frame(pred_test_samples_prob) %>% 
  transmute(pred_outbreak_risk = as.factor(case_when(
    pred_test_samples_prob < 0.25  ~ "0-25%",
    pred_test_samples_prob < 0.5  ~ "25-50%",
    pred_test_samples_prob < 0.75  ~ "50-75%",
    TRUE ~ "larger_than_75%"
  )))      

data_individual_prediction = data.frame(community_data_latest_day_selected_variables_no_nan$cumulative_cases, pred_outbreak_risk)
data_individual_prediction = 
data_individual_prediction  %>%
  rename(cumulative_cases = community_data_latest_day_selected_variables_no_nan.cumulative_cases)

structure_individual_prediction = empty.graph(c("cumulative_cases", "pred_outbreak_risk"))
modelstring(structure_individual_prediction) = paste0("[cumulative_cases|pred_outbreak_risk][pred_outbreak_risk]")
print(plot.network(structure_individual_prediction))
model_individual_prediction = bn.fit(structure_individual_prediction, data = data_individual_prediction)
print(model_individual_prediction)
                      

