
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', dependencies = TRUE)
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages('funModeling', repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")

# file paths and names for long-term use
local_path = "~/Downloads/new/"
data_path = paste0(local_path, "data/")
cleaned_data_path = paste0(data_path, 'output_clean/')
modeling_path = paste0(data_path, 'modeling/')
interpolation_path = paste0(data_path, 'dynamic/locations_demographics/', Sys.Date(), "/")

# import cleaned data file
LA_county = read.csv(paste0(cleaned_data_path, "LA_county.csv"))
community_today = read.csv(paste0(cleaned_data_path, "community_todayUpdated.csv"))
cv_cases_longitudinal = read.csv(paste0(cleaned_data_path, "community_TimeCases_long.csv"))
# import static data file
community_location = geojson_read(paste0(data_path, "static/la_county_geom.geojson"), what = "sp")
community_demographic = read.csv(paste0(data_path, "static/community_demographic_cleaned.csv"))

print("Success checkpoint 2: read in datafiles")


#=======================================
#     COVID-19 mapper tab Settings     #
#=======================================
# Define current date -- LA county
current_date = as.Date(max(LA_county$Date, na.rm = T), "%Y-%m-%d")
cv_min_date = as.Date(min(LA_county$Date, na.rm = T), "%Y-%m-%d")
cv_max_date_clean = format(current_date, "%d %B %Y")
# Define current date -- community level
current_date_community = as.Date(max(cv_cases_longitudinal$date, na.rm = T), "%Y-%m-%d")
cv_min_date_community = as.Date(min(cv_cases_longitudinal$date, na.rm = T), "%Y-%m-%d")

# set mapping colour for each outbreak
county_col = c("#E76F51", "#264653")
# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8, "Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8, "Set2"), 
            brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")), 5)
cls_names = as.character(unique(cv_cases_longitudinal$place_ID))
community_cols = cls[1:length(cls_names)]
names(community_cols) = cls_names

# GEO MAP functions
# create plotting parameters for map
bins = c(0, 50, 200, 500, 1000, 3000, Inf)
cv_pal = colorBin("Oranges", domain = community_today$cumulative_cases_today, bins = bins)
plot_map = community_location[community_location$label %in% community_today$geo_merge, ]
# creat cv base map 
basemap = leaflet(plot_map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-120, -116, ~36, 30) %>%
  setView(lng = -118.3, lat = 34.15,  zoom = 10.2) %>% 
  addLegend("bottomright", pal = cv_pal, values = ~community_today$cumulative_cases_today,
            title = "<small>Cumulative cases </small>")


#===================================
#     Region info tab Settings     #
#===================================
community_demographic_short = 
  community_demographic %>% 
  select(place_ID, POPULATION.TOTAL, SQUARE.MILES, MEDIAN.AGE, DIVERSITY.INDEX, AVERAGE.HOUSEHOLD.SIZE, Median.Income,
         AGE.65.AND.UP, AGE.50.TO.64, AGE.35.TO.49, AGE.19.TO.34, AGE.11.TO.18, AGE.10.OR.LESS,
         NEVER.MARRIED.MALES, NEVER.MARRIED.FEMALES, MARRIED.MALES, MARRIED.FEMALES, WIDOWED.MALES, WIDOWED.FEMALES, DIVORCED.MALES, DIVORCED.FEMALES
         ) %>% 
  rename(COMMUNITY = place_ID)


#=====================================
#     Risk modeling tab Settings     #
#=====================================
# Load the best model so far
load(paste0(modeling_path, "merged_community_expand.rda"))
# Also read in the interpolated data for individual risk prediction later
estimated_infection_race = read.csv(paste0(interpolation_path, "race-ethnicity_interpolated.csv")) # For each race category, the proportion of infected people in that category
estimated_infection_age = read.csv(paste0(interpolation_path, "AgeInterpolationFinalDF.csv")) # For each age category, the proportion of infected people in that category

set.seed(1)
num_folds = 5

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
save(d_bins_community_no_interpolation, file = (paste0(modeling_path, "discretization_params_community_no_interpolation.rda")))
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
  
  print(paste("Sensitivity of this week's model is", cv_results_education_age_race[1]))
}
community_best_model = cv_results_education_age_race[[6]]
save(community_best_model, file = (paste0(modeling_path, "community_model_best_fold_sensitivity_latest_time_window.rda")))

save_time_series_model_performance = data.frame(date_community_freq_sufficient_n$Var1, time_series_sensitivity, time_series_specificity, time_series_F1, time_series_F2)
save_time_series_model_performance = 
  save_time_series_model_performance %>%
  rename(date = date_community_freq_sufficient_n.Var1)
save(save_time_series_model_performance, file = (paste0(modeling_path, "save_time_series_model_performance.rda")))

# Incorporating interpolated age and race data with the latest community data to predict outbreak risk #
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

save(merge_interpolated_age_race, file = (paste0(modeling_path, "merge_interoplated_age_race.rda")))

# Merge the interpolated data with the community level data from the latest time window
merge_community_interpolated =  
  community_latest %>% 
  inner_join(merge_interpolated_age_race, by = c("place_ID" = "Place")) %>% # Then take the intersection between interpolated data and community data
  # select(outbreak_7d_lead, cumulative_3w_pop_adj, past_cases_3d, high_school_orless, age34_less, non_white, Asian_infected, Black_infected, Latino_infected, White_infected, Non_white_infected, age_18_less_infected, age_19_to_49_infected, age_50_to_65_infected, age_65_up_infected)
  select(outbreak_7d_lead, cumulative_3w_pop_adj, past_cases_3d, high_school_orless, age34_less, non_white, age_49_less_infected, Non_white_infected)

d_bins_community_with_interpolation = discretize_get_bins(data = merge_community_interpolated, n_bins=3)
save(d_bins_community_with_interpolation, file = (paste0(modeling_path, "discretization_params_community_with_interpolation.rda")))
merge_community_interpolated_discrete = discretize_df(data = merge_community_interpolated, data_bins = d_bins_community_with_interpolation, stringsAsFactors=T)
merge_community_interpolated_discrete = as.data.frame(merge_community_interpolated_discrete)

# Randomly shuffle before doing train-test splitting
merge_community_interpolated_discrete = merge_community_interpolated_discrete[sample(nrow(merge_community_interpolated_discrete)), ]

structure_community_interpolated = empty.graph(c("outbreak_7d_lead", "cumulative_3w_pop_adj", "past_cases_3d", "high_school_orless", "age34_less", "non_white", "age_49_less_infected", "Non_white_infected"))
modelstring(structure_community_interpolated) = paste0("[outbreak_7d_lead|cumulative_3w_pop_adj:past_cases_3d:age_49_less_infected:Non_white_infected]",
                                                       "[past_cases_3d|cumulative_3w_pop_adj][cumulative_3w_pop_adj|high_school_orless:non_white:age34_less][high_school_orless|non_white][non_white][age34_less]",
                                                       "[age_49_less_infected][Non_white_infected]")
plot.network(structure_community_interpolated)

# Run cross validation using the given features
cv_results_merge_community_interpolated = run_cross_validation(structure_community_interpolated, merge_community_interpolated_discrete, num_folds)

# Save the performance
sensitivity_latest_time_window_with_interpolation = cv_results_merge_community_interpolated[[1]]
specificity_latest_time_window_with_interpolation= cv_results_merge_community_interpolated[[2]]
F1_latest_time_window_with_interpolation = cv_results_merge_community_interpolated[[3]]
F2_latest_time_window_with_interpolation = cv_results_merge_community_interpolated[[4]]

community_interpolation_best_model = cv_results_merge_community_interpolated[[6]]
save(community_interpolation_best_model, file = (paste0(modeling_path, "community_model_best_fold_sensitivity_latest_with_interpolation.rda")))
save(cv_results_merge_community_interpolated, file = (paste0(modeling_path, "community_model_performance_with_interpolation.rda")))

# Function for outbreak risk prediction on all communities
make_prediction_on_all_communities = function(filepath){
  filepath = modeling_path
  load(paste0(filepath, "merged_community_mst_recent.rda"))
  load(paste0(filepath, "merged_community_expand.rda"))
  load(paste0(filepath, "merge_interoplated_age_race.rda"))
  load(paste0(filepath, "discretization_params_community_with_interpolation.rda"))
  load(paste0(filepath, "discretization_params_community_no_interpolation.rda"))
  load(paste0(filepath, "community_model_best_fold_sensitivity_latest_with_interpolation.rda")) # In this saved model, the target variable is "outbreak_7d_lead". community_best_model
  
  # Preprocessing of the data for community level prediction#
  
  # Add interpolated race and age data
  community_latest = 
    merged_community_mst_recent %>%
    inner_join(merge_interpolated_age_race, by = c("place_ID" = "Place")) # Then take the intersection between interpolated data and community data
  
  age_34_less_column = community_latest$AGE.10.OR.LESS + community_latest$AGE.11.TO.18 + community_latest$AGE.19.TO.34 
  community_latest = cbind(community_latest, age_34_less_column)
  
  # Apply the same discretization parameters from the trained model
  community_latest = discretize_df(data = community_latest, data_bins = d_bins_community_no_interpolation, stringsAsFactors=T)
  community_latest = discretize_df(data = community_latest, data_bins = d_bins_community_with_interpolation, stringsAsFactors=T)
  
  
  # Delete the communities/rows that have nan values
  community_latest = na.omit(community_latest)
  
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
  filepath = modeling_path
  output_community = make_prediction_on_all_communities(filepath)
  age_infected_category_dynamic = read.csv(paste0(interpolation_path, "Age_Group(excluding_LB_Pas).csv"))
  race_infected_category_dynamic = read.csv(paste0(interpolation_path, "Race_Ethnicity(excluding_LB_Pas).csv"))
  
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
  cpt_age = array(c(age_18_less_percent_pop, age_19_49_percent_pop, age_50_64_percent_pop, age_65_up_percent_pop), dim = 4, dimnames = list("age" = c("age_18_less", "age_19_to_49", "age_50_to_64", "age_65_up"))) # The probability of each age category
  cpt_race = array(c(Asian_percent_pop, Black_percent_pop, Latino_percent_pop, White_percent_pop, Other_percent_pop), dim = 5, dimnames = list("race" = c("Asian", "Black", "Latino", "White", "Other"))) # The probability of each race category
  cpt_outbreak = array(individual_model_outbreak_infection$pred_outbreak_risk$prob, dim = 3, dimnames = list("outbreak" = c("High_risk_outbreak", "Intermediate_risk_outbreak", "Low_risk_outbreak"))) # The probability of each outbreak risk category
  
  # CPT on the node "Infection". Since "Infection" has 3 parents, age which has 4 categories, race which has 4 categories, and outbreak which has 2 categories, "Infection" itself has 2 categories, there will be 4 x 4 x 2 x 2 = 64 numbers in this CPT.
  # Each number is derived based on conditional independence assumption. E.g., P(Asian, 18_years_less, High_risk_outbreak| Infected) = P(Asian | Infected) x P(18_years_less | Infected) x P(High_risk_outbreak | Infected)
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
                                          dim = c(2, 5, 4, 3), dimnames = list("infection" = c("Infected", "Not_infected"), "race" = c("Asian", "Black", "Latino", "White", "Other"), "age" = c("age_18_less", "age_19_to_49", "age_50_to_64", "age_65_up"), "outbreak" = c("High_risk_outbreak", "Intermediate_risk_outbreak", "Low_risk_outbreak")))
  
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
  # print(cpquery(individual_model_manual, (infection == 'Infected'), (outbreak == 'High_risk_outbreak') & (age == "age_19_to_49") & (race == "Latino")))
  # print(cpquery(individual_model_manual, (infection == 'Infected'), (outbreak == 'High_risk_outbreak') & (age == "age_65_up") & (race == "Latino")))
  # print(cpquery(individual_model_manual, (infection == "Infected"), (age == "age_19_to_49") & outbreak == 'No_outbreak'))
  # print(cpquery(individual_model_manual, (infection == "Infected"), (age == "age_19_to_49") & (outbreak == 'High_risk_outbreak')))
  # print(cpquery(individual_model_manual, (infection == "Infected"), (race == "Black") & outbreak == 'No_outbreak'))
  # print(cpquery(individual_model_manual, (infection == "Infected"), (race == "Black") & (outbreak == 'High_risk_outbreak')))
  return (list(user_community_risk_category, prediction_infected, save_place_ID_and_outbreak_risk))
  
}

output_community = make_prediction_on_all_communities(filepath)
save_place_ID_and_outbreak_risk = output_community[[1]]
community_risk_combined = 
  save_place_ID_and_outbreak_risk %>% 
  left_join(community_today, by = c("place_ID" = "place_ID")) %>% 
  mutate(pred_prob = round(community_pred_test_samples_prob, 4)) %>% 
  select(place_ID, population, cumulative_cases_today, adj_test_pos_rate,
         pred_prob, community_pred_test_samples_category) %>% 
  rename(Community = place_ID, Population = population, `Cumulative cases` = cumulative_cases_today,
         `Tested positive rate` = adj_test_pos_rate, `Predicted probability of outbreak` = pred_prob,
         `Predicted outbreak` = community_pred_test_samples_category)
community_risk_to_plot = 
  save_place_ID_and_outbreak_risk %>% 
  left_join(community_today, by = c("place_ID" = "place_ID")) %>% 
  mutate(pred_prob = round(community_pred_test_samples_prob, 4)) %>% 
  select(place_ID, longitude, latitude, pred_prob, geo_merge)

# Risk MAP functions
# create plotting parameters for map
bins = c(0, 0.3, 0.5, 0.7, 0.8, 0.9, 1)
cv_pal_risk = colorBin("Blues", domain = community_risk_to_plot$community_pred_test_samples_prob, bins = bins)
plot_map_risk = community_location[community_location$label %in% community_risk_to_plot$geo_merge, ]
# creat cv risk map 
riskmap = leaflet(plot_map_risk) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-120, -116, ~36, 30) %>%
  setView(lng = -118.3, lat = 34.15,  zoom = 10.2) %>% 
  addLegend("bottomright", pal = cv_pal_risk, values = ~community_risk_to_plot$community_pred_test_samples_prob,
            title = "<small>Predicted risk in 1 week </small>")

print("Success checkpoint 3: execute all settings")