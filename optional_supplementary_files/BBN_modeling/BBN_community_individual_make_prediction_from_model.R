# This script makes predictions on the risk of an individual getting COVID. No evaluation or testing exists.
# First, this script predicts the risk of outbreak of a community in 3 days given the COVID stats on the current day using a saved BBN model. 
# Then, the predicted outbreak risk is combined with individual level demographics to predict a person's risk of getting COVID.

if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")
# if(!require(gRbase)) install.packages('gRbase', dependencies = TRUE)
# if(!require(gRain)) install.packages('gRain', dependencies = TRUE)
set.seed(3)

#================================================
# Functions#
#================================================
# Function for making inference given a BBN structure and data
inference_on_new_samples = function(structure, data){
  target_variable_index = 1 # Assuming the target variable is at the first index
  pred_test_samples_prob = c()
  pred_classes = c()
  
  for (test_index in 1:nrow(data)){
    testing_sample = data[test_index, ]
    
    # Evidence is all the features of this sample.
    column_names = names(data)
    event_yes = paste("(", column_names[target_variable_index], " == 'Yes')", sep = "")
    event_no = paste("(", column_names[target_variable_index], " == 'No')", sep = "")
    
    evidence = paste("(", column_names[-target_variable_index], " == '",sapply(testing_sample[-target_variable_index], as.character), "')",sep = "", collapse = " & ") 
    # Make prediction at both values that the target variable can take on, 
    cmd_yes = paste("cpquery(structure, ", event_yes, ", ", evidence, ")", sep = "")
    cmd_no = paste("cpquery(structure, ", event_no, ", ", evidence, ")", sep = "")
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
  
  return (list(pred_test_samples_prob, pred_classes))
}

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

# Function for outbreak risk prediction on all communities
make_prediction_on_all_communities = function(filepath){
  filepath = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/BBN_modeling/"
  load("/Users/rinading/Desktop/new/data/modeling/merged_community_mst_recent.rda")
  load("/Users/rinading/Desktop/new/data/modeling/merged_community_expand.rda")
  load(paste0(filepath, "discretization_params_community_no_interpolation.rda"))
  load(paste0(filepath, "discretization_params_community_with_interpolation.rda"))
  load("/Users/rinading/Desktop/new/data/modeling/community_model_best_fold_sensitivity_latest_with_interpolation.rda") # In this saved model, the target variable is "outbreak_7d_lead". community_best_model
  load("/Users/rinading/Desktop/new/data/modeling/merge_interoplated_age_race.rda")
  
  # Preprocessing of the data for community level prediction#
  community_latest = 
    merged_community_mst_recent  %>% 
    filter(date == max(as.Date(merged_community_mst_recent$date)))   # Get the latest day available
  # Add interpolated race and age data
  community_latest = 
    community_latest %>%
    inner_join(merge_interpolated_age_race, by = c("place_ID" = "Place")) # Then take the intersection between interpolated data and community data
    
  
  age_34_less_column = community_latest$AGE.10.OR.LESS + community_latest$AGE.11.TO.18 + community_latest$AGE.19.TO.34 
  community_latest = cbind(community_latest, age_34_less_column)

  # Apply the same discretization parameters from the trained model
  community_latest = discretize_df(data = community_latest, data_bins = d_bins_community_no_interpolation, stringsAsFactors=T)
  community_latest = discretize_df(data = community_latest, data_bins = d_bins_community_with_interpolation, stringsAsFactors=T)
  
  community_latest_selected_variables_no_nan = 
    community_latest %>% 
    rename(outbreak_7d_lead = new_7d_lead, past_cases_3d = new_3d_MA_pop_adj, age34_less = age_34_less_column, high_school_orless = LESS.THAN.HIGH.SCHOOL, non_white = NON.WHITE.POPULATION) %>%
    select(outbreak_7d_lead, cumulative_3w_pop_adj, past_cases_3d, high_school_orless, age34_less, non_white, age_49_less_infected, Non_white_infected)
  
  community_predictions = inference_on_new_samples(community_interpolation_best_model, community_latest_selected_variables_no_nan)
  community_pred_test_samples_prob = community_predictions[[1]]
  community_pred_classes = community_predictions[[2]]
  # For visualization purpose. Save the place_ID and its according predicted probability or risk of outbreak.
  save_place_ID_and_outbreak_risk = data.frame(community_latest$place_ID, community_pred_test_samples_prob)
  save_place_ID_and_outbreak_risk = 
    save_place_ID_and_outbreak_risk %>%
    mutate(community_pred_test_samples_category = as.factor(case_when(
      community_pred_test_samples_prob < 0.33  ~ "Low_risk_outbreak",
      community_pred_test_samples_prob < 0.67  ~ "Intermediate_risk_outbreak",
      TRUE ~ "High_risk_outbreak"
  ))) %>%
    rename(place_ID = community_latest.place_ID)
  # Check the occurrences of predicted category
  table(community_pred_classes)
  
  # Individual level risk prediction automated CPT part#
  # First select the community data from the latest time window
  community_latest = 
    merged_community_expand %>%
    filter(date == max(as.Date(merged_community_expand$date))) 
  # Delete the communities that don't have age and race interpolation
  community_latest = 
    community_latest %>%
    inner_join(merge_interpolated_age_race, by = c("place_ID" = "Place")) # Then take the intersection between interpolated data and community data
  
  
  # Then add the predicted risk scores from the community modeling
  community_latest_outbreak_risk = cbind(community_latest, data.frame(community_pred_test_samples_prob))
  
  # Discretize the variables
  community_latest_outbreak_risk_discrete =
    community_latest_outbreak_risk %>%
    mutate(crude_rate_7d_lead = as.factor(case_when(
      crude_rate_7d_lead >= median(crude_rate_7d_lead) ~ "Yes_infection",
      !is.na(crude_rate_7d_lead) ~ "No_infection"
    ))) %>%
    mutate(pred_outbreak_risk = as.factor(case_when(
      community_pred_test_samples_prob < 0.33  ~ "Low_risk_outbreak",
      community_pred_test_samples_prob < 0.67  ~ "Intermediate_risk_outbreak",
      TRUE ~ "High_risk_outbreak"
    ))) %>%
    select(crude_rate_7d_lead, pred_outbreak_risk) 
  
  community_latest_outbreak_risk_discrete = as.data.frame(community_latest_outbreak_risk_discrete)
  # Automatically learn the conditional probability table on the relationship outbreak-->crude rate
  partial_structure_individual_prediction = empty.graph(c("crude_rate_7d_lead", "pred_outbreak_risk"))
  modelstring(partial_structure_individual_prediction) = paste0("[crude_rate_7d_lead|pred_outbreak_risk][pred_outbreak_risk]")
  individual_model_outbreak_infection = bn.fit(partial_structure_individual_prediction, data = community_latest_outbreak_risk_discrete, method = "bayes")
  
  return (list(save_place_ID_and_outbreak_risk, individual_model_outbreak_infection))
}


# Function for individual level risk prediction#
build_individual_BBN = function(){
  filepath = "/Users/rinading/Desktop/UCLA/courses/M223A_programming/UCLA-BE-223a-Covid-19-Project/BBN_modeling/"
  output_community = make_prediction_on_all_communities(filepath)
  age_infected_category_dynamic = read.csv(paste0(filepath, "Age_Group(excluding_LB_Pas).csv"))
  race_infected_category_dynamic = read.csv(paste0(filepath, "Race_Ethnicity(excluding_LB_Pas).csv"))
  
  save_place_ID_and_outbreak_risk = output_community[[1]]
  individual_model_outbreak_infection = output_community[[2]]
  
  p_covid_incidence_rate = 0.036
  # Static age and race data
  la_county_pop = 10039107
  age_18_less_percent_pop = 0.217
  age_19_49_percent_pop = 0.458
  age_50_64_percent_pop = 0.192
  age_65_up_percent_pop = 0.132
  Asian_percent_pop = 0.154
  Black_percent_pop = 0.09
  Latino_percent_pop = 0.486
  White_percent_pop = 0.261
  Other_percent_pop = 0.009
  population_age_category = c(la_county_pop * age_18_less_percent_pop, la_county_pop * age_19_49_percent_pop, la_county_pop * age_50_64_percent_pop, la_county_pop * age_65_up_percent_pop) # Static data. In sequence of age_18_less, 19-49, 50-64, 65_up. 
  infected_age_category = c(sum(age_infected_category_dynamic$Cases[1:3]), sum(age_infected_category_dynamic$Cases[4:5]), age_infected_category_dynamic$Cases[6], sum(age_infected_category_dynamic$Cases[7:8])) # Dynamic data. In sequence of age_18_less, 19-49, 50-64, 65_up. 
  infAge_conditional = infected_age_category / population_age_category # Proportion of people who got COVID within each specific age category using LA county data. P(infected | age)
  infAge = c(infAge_conditional[1] * age_18_less_percent_pop, infAge_conditional[2] * age_19_49_percent_pop, infAge_conditional[3] * age_50_64_percent_pop, infAge_conditional[4] * age_65_up_percent_pop) / p_covid_incidence_rate
  not_infAge = 1 - infAge # Proportion of people who didn't get COVID within each specific age category using LA county data
  
  population_in_race = c(la_county_pop * Asian_percent_pop, la_county_pop * Black_percent_pop, la_county_pop * Latino_percent_pop, la_county_pop * White_percent_pop, la_county_pop * Other_percent_pop) # Static data. In sequence of Asian, Black, Latino, White, Other
  infected_in_race = c(race_infected_category_dynamic$Cases[2], race_infected_category_dynamic$Cases[3], race_infected_category_dynamic$Cases[4], race_infected_category_dynamic$Cases[6], race_infected_category_dynamic$Cases[7]) # Dynamic data. In sequence of Asian, Black, Latino, White, Other
  infRace_conditional = infected_in_race / population_in_race # Proportion of people who got COVID within each specific race category using LA county data
  infRace = c(infRace_conditional[1] * Asian_percent_pop, infRace_conditional[2] * Black_percent_pop, infRace_conditional[3] * Latino_percent_pop, infRace_conditional[4] * White_percent_pop, infRace_conditional[5] * Other_percent_pop) / p_covid_incidence_rate
  not_infRace = 1 - infRace # Proportion of people who didn't get COVID within each specific race category using LA county data
  
  infOutbreak = c(individual_model_outbreak_infection$crude_rate_7d_lead$prob["Yes_infection", "High_risk_outbreak"] , individual_model_outbreak_infection$crude_rate_7d_lead$prob["Yes_infection", "Intermediate_risk_outbreak"], individual_model_outbreak_infection$crude_rate_7d_lead$prob["Yes_infection", "Low_risk_outbreak"]) # In sequence of high, intermediate, low risk. Numbers are from automatically learned CPT.
  not_infOutbreak = 1 - infOutbreak
  
  # Set up the network structure of (age, race, outbreak) --> Infection 
  net_age_race_outbreak_infection = model2network("[infection|age:race:outbreak][age][race][outbreak]")
  plot.network(net_age_race_outbreak_infection)
  # Priors. Static data.
  cpt_age = array(c(age_18_less_percent_pop, age_19_49_percent_pop, age_50_64_percent_pop, age_65_up_percent_pop), dim = 4, dimnames = list("age" = c("age_18_less", "age_19_o_49", "age_50_to_64", "age_65_up"))) # The probability of each age category
  cpt_race = array(c(Asian_percent_pop, Black_percent_pop, Latino_percent_pop, White_percent_pop, Other_percent_pop), dim = 5, dimnames = list("race" = c("Asian", "Black", "Latino", "White", "Other"))) # The probability of each race category
  cpt_outbreak = array(individual_model_outbreak_infection$pred_outbreak_risk$prob, dim = 3, dimnames = list("outbreak" = c("High_risk_outbreak", "Intermediate_risk_outbreak", "Low_risk_outbreak"))) # The probability of each outbreak risk category
  
  # CPT on the node "Infection". Since "Infection" has 3 parents, age which has 4 categories, race which has 4 categories, and outbreak which has 2 categories, "Infection" itself has 2 categories, there will be 4 x 4 x 2 x 2 = 64 numbers in this CPT.
  # Each number is derived based on conditional independence assumption. E.g., P(Asian, 18_years_less, Yes_outbreak| Infected) = P(Asian | Infected) x P(18_years_less | Infected) x P(Yes_outbreak | Infected)
  cpt_age_race_outbreak_infection = array(c(infOutbreak[1] * infAge[1] * infRace[1], 1 - (infOutbreak[1] * infAge[1] * infRace[1]), infOutbreak[1] *infAge[1] * infRace[2], 1 - (infOutbreak[1] *infAge[1] * infRace[2]), infOutbreak[1] *infAge[1] * infRace[3], 1 - (infOutbreak[1] *infAge[1] * infRace[3]), infOutbreak[1] *infAge[1] * infRace[4], 1 - (infOutbreak[1] *infAge[1] * infRace[4]), infOutbreak[1] *infAge[1] * infRace[5], 1 - (infOutbreak[1] *infAge[1] * infRace[5]), 
                                   infOutbreak[1] *infAge[2] * infRace[1], 1 - (infOutbreak[1] *infAge[2] * infRace[1]), infOutbreak[1] *infAge[2] * infRace[2], 1 - (infOutbreak[1] *infAge[2] * infRace[2]), infOutbreak[1] *infAge[2] * infRace[3], 1 - (infOutbreak[1] *infAge[2] * infRace[3]), infOutbreak[1] *infAge[2] * infRace[4], 1 - (infOutbreak[1] *infAge[2] * infRace[4]),infOutbreak[1] *infAge[2] * infRace[5], 1 - (infOutbreak[1] *infAge[2] * infRace[5]),
                                   infOutbreak[1] *infAge[3] * infRace[1], 1 - (infOutbreak[1] *infAge[3] * infRace[1]), infOutbreak[1] *infAge[3] * infRace[2], 1 - (infOutbreak[1] *infAge[3] * infRace[2]), infOutbreak[1] *infAge[3] * infRace[3], 1 - (infOutbreak[1] *infAge[3] * infRace[3]), infOutbreak[1] *infAge[3] * infRace[4], 1 - (infOutbreak[1] *infAge[3] * infRace[4]), infOutbreak[1] *infAge[3] * infRace[5], 1 - (infOutbreak[1] *infAge[3] * infRace[5]),
                                   infOutbreak[1] *infAge[4] * infRace[1], 1 - (infOutbreak[1] *infAge[4] * infRace[1]), infOutbreak[1] *infAge[4] * infRace[2], 1 - (infOutbreak[1] *infAge[4] * infRace[2]), infOutbreak[1] *infAge[4] * infRace[3], 1 - (infOutbreak[1] *infAge[4] * infRace[3]), infOutbreak[1] *infAge[4] * infRace[4],1 - (infOutbreak[1] *infAge[4] * infRace[4]), infOutbreak[1] *infAge[4] * infRace[5],1 - (infOutbreak[1] *infAge[4] * infRace[5]),
                                   
                                   infOutbreak[2] * infAge[1] * infRace[1], 1 - (infOutbreak[2] * infAge[1] * infRace[1]), infOutbreak[2] *infAge[1] * infRace[2], 1 - (infOutbreak[2] *infAge[1] * infRace[2]), infOutbreak[2] *infAge[1] * infRace[3], 1 - (infOutbreak[2] *infAge[1] * infRace[3]), infOutbreak[2] *infAge[1] * infRace[4], 1 - (infOutbreak[2] *infAge[1] * infRace[4]), infOutbreak[2] *infAge[1] * infRace[5], 1 - (infOutbreak[2] *infAge[1] * infRace[5]), 
                                   infOutbreak[2] *infAge[2] * infRace[1], 1 - (infOutbreak[2] *infAge[2] * infRace[1]), infOutbreak[2] *infAge[2] * infRace[2], 1 - (infOutbreak[2] *infAge[2] * infRace[2]), infOutbreak[2] *infAge[2] * infRace[3], 1 - (infOutbreak[2] *infAge[2] * infRace[3]), infOutbreak[2] *infAge[2] * infRace[4], 1 - (infOutbreak[2] *infAge[2] * infRace[4]), infOutbreak[2] *infAge[2] * infRace[5], 1 - (infOutbreak[2] *infAge[2] * infRace[5]),
                                   infOutbreak[2] *infAge[3] * infRace[1], 1 - (infOutbreak[2] *infAge[3] * infRace[1]), infOutbreak[2] *infAge[3] * infRace[2], 1 - (infOutbreak[2] *infAge[3] * infRace[2]), infOutbreak[2] *infAge[3] * infRace[3], 1 - (infOutbreak[2] *infAge[3] * infRace[3]), infOutbreak[2] *infAge[3] * infRace[4], 1 - (infOutbreak[2] *infAge[3] * infRace[4]), infOutbreak[2] *infAge[3] * infRace[5], 1 - (infOutbreak[2] *infAge[3] * infRace[5]), 
                                   infOutbreak[2] *infAge[4] * infRace[1], 1 - (infOutbreak[2] *infAge[4] * infRace[1]), infOutbreak[2] *infAge[4] * infRace[2], 1 - (infOutbreak[2] *infAge[4] * infRace[2]), infOutbreak[2] *infAge[4] * infRace[3], 1 - (infOutbreak[2] *infAge[4] * infRace[3]), infOutbreak[2] *infAge[4] * infRace[4],1 - (infOutbreak[2] *infAge[4] * infRace[4]), infOutbreak[2] *infAge[4] * infRace[5],1 - (infOutbreak[2] *infAge[4] * infRace[5]),
                                   
                                   infOutbreak[3] * infAge[1] * infRace[1], 1 - (infOutbreak[3] * infAge[1] * infRace[1]), infOutbreak[3] *infAge[1] * infRace[2], 1 - (infOutbreak[3] *infAge[1] * infRace[2]), infOutbreak[3] *infAge[1] * infRace[3], 1 - (infOutbreak[3] *infAge[1] * infRace[3]), infOutbreak[3] *infAge[1] * infRace[4], 1 - (infOutbreak[3] *infAge[1] * infRace[4]), infOutbreak[3] *infAge[1] * infRace[5], 1 - (infOutbreak[3] *infAge[1] * infRace[5]), 
                                   infOutbreak[3] *infAge[2] * infRace[1], 1 - (infOutbreak[3] *infAge[2] * infRace[1]), infOutbreak[3] *infAge[2] * infRace[2], 1 - (infOutbreak[3] *infAge[2] * infRace[2]), infOutbreak[3] *infAge[2] * infRace[3], 1 - (infOutbreak[3] *infAge[2] * infRace[3]), infOutbreak[3] *infAge[2] * infRace[4], 1 - (infOutbreak[3] *infAge[2] * infRace[4]), infOutbreak[3] *infAge[2] * infRace[5], 1 - (infOutbreak[3] *infAge[2] * infRace[5]),
                                   infOutbreak[3] *infAge[3] * infRace[1], 1 - (infOutbreak[3] *infAge[3] * infRace[1]), infOutbreak[3] *infAge[3] * infRace[2], 1 - (infOutbreak[3] *infAge[3] * infRace[2]), infOutbreak[3] *infAge[3] * infRace[3], 1 - (infOutbreak[3] *infAge[3] * infRace[3]), infOutbreak[3] *infAge[3] * infRace[4], 1 - (infOutbreak[3] *infAge[3] * infRace[4]), infOutbreak[3] *infAge[3] * infRace[5], 1 - (infOutbreak[3] *infAge[3] * infRace[5]), 
                                   infOutbreak[3] *infAge[4] * infRace[1], 1 - (infOutbreak[3] *infAge[4] * infRace[1]), infOutbreak[3] *infAge[4] * infRace[2], 1 - (infOutbreak[3] *infAge[4] * infRace[2]), infOutbreak[3] *infAge[4] * infRace[3], 1 - (infOutbreak[3] *infAge[4] * infRace[3]), infOutbreak[3] *infAge[4] * infRace[4],1 - (infOutbreak[3] *infAge[4] * infRace[4]), infOutbreak[3] *infAge[4] * infRace[5],1 - (infOutbreak[3] *infAge[4] * infRace[5])),
                                   dim = c(2, 5, 4, 3), dimnames = list("infection" = c("Infected", "Not_infected"), "race" = c("Asian", "Black", "Latino", "White", "Other"), "age" = c("age_18_less", "age_19_o_49", "age_50_to_64", "age_65_up"), "outbreak" = c("High_risk_outbreak", "Intermediate_risk_outbreak", "Low_risk_outbreak")))
  
  # Fit the BBN using the CPTs. The function automatically checks for the validity of all numbers. E.g. the probability distribution of a node summing to one.
  individual_model_manual = custom.fit(net_age_race_outbreak_infection, list("infection" = cpt_age_race_outbreak_infection, "race" = cpt_race, "age" = cpt_age, "outbreak" = cpt_outbreak))
  return (list(individual_model_manual, save_place_ID_and_outbreak_risk))
}

make_prediction_on_one_person = function(individual_model_manual, save_place_ID_and_outbreak_risk, community, age, race){
  # Get the parameters from users and from the calculation of user's community outbreak risk
  user_community_risk_category = save_place_ID_and_outbreak_risk[save_place_ID_and_outbreak_risk$place_ID == community, ]$community_pred_test_samples_category # Find the risk category of the user's community
  userinput = setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("outbreak", "age", "race"))
  userinput[1, ] = c(as.character(user_community_risk_category), age, race)
  # Make prediction using manual CPT on an individual#
  evidence = paste("(", names(userinput), " == '",sapply(userinput, as.character), "')",sep = "", collapse = " & ") 
  event = paste("(infection == 'Infected')", sep = "")
  cmd_yes = paste("cpquery(individual_model_manual, ", event, ", ", evidence, ")", sep = "")
  prediction_infected = eval(parse(text = cmd_yes))
  print(cpquery(individual_model_manual, (infection == 'Infected'), (outbreak == 'Yes_outbreak') & (age == "age_19_o_49") & (race == "Latino")))
  print(cpquery(individual_model_manual, (infection == 'Infected'), (outbreak == 'Yes_outbreak') & (age == "age_65_up") & (race == "Latino")))
  print(cpquery(individual_model_manual, (infection == "Infected"), (age == "age_19_o_49") & outbreak == 'No_outbreak'))
  print(cpquery(individual_model_manual, (infection == "Infected"), (age == "age_19_o_49") & (outbreak == 'Yes_outbreak')))
  print(cpquery(individual_model_manual, (infection == "Infected"), (race == "Black") & outbreak == 'No_outbreak'))
  print(cpquery(individual_model_manual, (infection == "Infected"), (race == "Black") & (outbreak == 'Yes_outbreak')))
  return (list(user_community_risk_category, prediction_infected, save_place_ID_and_outbreak_risk))
  
}
#================================================
# To be used in visualization#
#================================================
# Eligible names for community: the names in column "place_ID" in save_place_ID_and_outbreak_risk
# Eligible names for age: "age_18_less", "age_19_o_49", "age_50_to_64", "age_65_up"
# Eligible names for race: "Asian", "Black", "Latino", "White"
community = "Carson"
age = "age_19_o_49"
race = "Asian"
from_BBN = build_individual_BBN()
results = make_prediction_on_one_person(from_BBN[[1]], from_BBN[[2]], community, age, race)
save_place_ID_and_outbreak_risk = results[[3]]
print(paste("User's community outbreak category is", results[[1]]))
print(paste("User's chance of getting infected is", results[[2]]))

