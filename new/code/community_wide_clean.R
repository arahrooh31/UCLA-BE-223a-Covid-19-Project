## Covid-2019 interactive mapping tool: script to reformat New York Times LA community-level data

## data extracted from New York Times state-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data

## community-level population information obtained from US census data
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

options(warn = -1)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(reticulate)) install.packages("reticulate", repos = "http://cran.us.r-project.org")
if(!require(RcppRoll)) install.packages("RcppRoll", repos = "http://cran.us.r-project.org")


local_path = "/Users/Mingzhou/Desktop/2020_Fall/BE 223A/Group_project/new/"
code_path = paste0(local_path, "code/")
data_path = paste0(local_path, "data/")

# Output for cleaned files
cleaned_data_path = paste0(data_path, 'output_clean/')
modeling_path = paste0(data_path, "modeling/")

# Source in the LA_county_scraper -- update TODAY's COVID by community
setwd(paste0(data_path, "dynamic/"))
reticulate::source_python(paste0(code_path, "LA_county_scraper.py"))
reticulate::source_python(paste0(code_path, "LA_county_locations.py"))
county_data_path = paste0(data_path, 'dynamic/county/')
interpolation_path = paste0(data_path, 'dynamic/locations_demographics/', Sys.Date(), "/")


#===============================================
# Cleaning of community-level time series data #
#===============================================
case_github_file = "https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-place-totals.csv"

# Load in CA cases data by community
CA_cases_time_series = as.data.frame(data.table::fread(case_github_file))
LA_cases_time_series =
  CA_cases_time_series %>% 
  filter(county == "Los Angeles") %>% 
  select(date, place, confirmed_cases, x, y) %>% 
  rename(longitude = x, latitude = y, cumulative_confirmed_cases = confirmed_cases)
LA_cases_time_series$date = as.Date(LA_cases_time_series$date, format = "%Y-%m-%d")

# Transform data
CSA_cases_byDate =
  LA_cases_time_series %>% 
  spread(date, value = cumulative_confirmed_cases, fill = 0)

# Data validation : if N is smaller than previous data, assign the number from previous date (Unchanged from original function)
for (i in 5:ncol(CSA_cases_byDate)) {
  # The first three column are names, longitude, and latitude, so start with 3/17
  if (any(CSA_cases_byDate[,i] < CSA_cases_byDate[, (i - 1)])) {
    CSA_cases_byDate[CSA_cases_byDate[,i] < CSA_cases_byDate[, (i - 1)], i] = CSA_cases_byDate[CSA_cases_byDate[,i] < CSA_cases_byDate[, (i - 1)], (i - 1)]
  }
}

# function to update input data according to mapping base format
update_LA_community = function(input_df, tag) {
  dates = names(input_df)[which(names(input_df) == "2020-03-16"):ncol(input_df)]
  input_df$place = input_df$place %>% str_replace_all(., " ", "") 
  rownames(input_df) = paste0(input_df$place, "_", tag)
  input_df = input_df %>% select(-c(place, longitude, latitude)) %>% t()
  input_df = data.frame(input_df)
  input_df$date = dates
  rownames(input_df) = 1:nrow(input_df)
  input_df$date = format(as.Date(input_df$date, format = "%Y-%m-%d"))
  input_df
}

CSA_cases_updated = update_LA_community(CSA_cases_byDate, "cases")
CSA_cases_updated = 
  CSA_cases_updated %>% 
  select(date, everything())

collated_data = NULL
# loop to add new data for each new situation report
for (i in c(1:nrow(CSA_cases_updated))) {
  # extract subset of data for date in row i
  community_subset = CSA_cases_updated[i,]
  community_subset_cases = community_subset[, which(grepl("_cases", names(community_subset)))]
  community_subset_cases = community_subset[, colSums(community_subset_cases) > 0]
  # build new dataframe to add updated data
  new_data = data.frame(place_ID = names(community_subset_cases) %>% str_replace_all(., "_cases", ""),
                        date = format(as.Date(community_subset$date[1], "%Y-%m-%d")),
                        update = i,
                        cases = NA, new_cases = 0)
  # update column names in new_jhu dataframes to include country names only
  colnames(community_subset_cases) = colnames(community_subset_cases) %>% str_replace_all(., "_cases", "") 
  # loop to update cases
  for (j in 1:nrow(new_data)) {
    # update case numbers
    place_name = as.character(new_data$place_ID[j])
    new_data$cases[j] = as.numeric(community_subset_cases[, place_name])
  }
  # append new data to collated dataframe
  collated_data = rbind(collated_data, new_data)
  collated_data$place_ID = as.character(collated_data$place_ID)
  # calculate new cases, deaths and recoveries
  if (i == 1) {
    collated_data$new_cases = collated_data$cases
  }
  if (i > 1) {
    # split it into date i and date i-1
    today = subset(collated_data, update == i)
    yesterday = subset(collated_data, update == (i - 1))
    
    for (k in 1:nrow(today)) {
      place_name = today$place_ID[k]
      
      # if present in yesterday's data, calculate new cases by subtraction
      if (place_name %in% yesterday$place_ID) {
        collated_data$new_cases[collated_data$place_ID == place_name & collated_data$update == i] = as.numeric(today$cases[today$place_ID == place_name]) - as.numeric(yesterday$cases[yesterday$place_ID == place_name])  
      } else {
        # if absent from yesterday's data, new observations = total observations
        collated_data$new_cases[collated_data$place_ID == place_name & collated_data$update == i] = as.numeric(today$cases[today$place_ID == place_name]) 
      }
    }
  }
}

# re-order
collated_data = 
  collated_data %>% 
  filter(place_ID != "date")
collated_data = collated_data[order(as.Date(collated_data$date, format = "%Y-%m-%d"), decreasing = F),]
# update time stamp
collated_data$last_update = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))
# Clean the place_ID
collated_data_clean =
  collated_data %>%
  mutate(place_ID_1 = gsub(pattern = "[[:punct:]]+", replacement = '', x = collated_data$place_ID))
collated_data_clean =
  collated_data_clean %>%
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = collated_data_clean$place_ID_1)) %>% 
  select(-place_ID_1)

collated_data_clean$date = as.Date(collated_data_clean$date, format = "%Y-%m-%d")

# Adding 7-day moving average
collated_data_clean = 
  collated_data_clean %>% 
  group_by(place_ID) %>% 
  mutate(if_first_report = if_else(row_number() == 1, 1, 0)) %>% 
  mutate(case_incremental_3d_MA = rollmean(new_cases, 3, align = "right", fill = 0)) %>% 
  mutate(case_incremental_7d_MA = rollmean(new_cases, 7, align = "right", fill = 0)) %>% 
  mutate(new_3d_lead = lead(new_cases, 3)) %>% 
  mutate(new_7d_lead = lead(new_cases, 7)) %>% 
  rename("new_cases_today" = "new_cases") %>% 
  rename("cumulative_cases_today" = "cases") %>% 
  select(place_ID, date, new_cases_today, cumulative_cases_today, case_incremental_3d_MA, case_incremental_7d_MA,
         new_3d_lead, new_7d_lead, if_first_report, last_update)

# Add in population info for each community
CSA_test_table = read.csv(paste0(county_data_path, "communitytesting.csv"))
population_community_raw =
  CSA_test_table %>% 
  select(geo_merge, population)
population_community_raw =
  population_community_raw %>% 
  mutate(place_ID_1 = gsub(pattern = "City of ", replacement = '', x = population_community_raw$geo_merge)) 
population_community_raw =
  population_community_raw %>% 
  mutate(place_ID_2 = gsub(pattern = "Los Angeles - ", replacement = '', x = population_community_raw$place_ID_1)) 
population_community_raw =
  population_community_raw %>% 
  mutate(place_ID_3 = gsub(pattern = "[[:punct:]]+", replacement = '', x = population_community_raw$place_ID_2))
population_community =
  population_community_raw %>% 
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = population_community_raw$place_ID_3)) %>% 
  select(place_ID, population) %>% 
  distinct() %>% 
  drop_na()
# N = 329 (communities w/ population data)

# Adding cumulative window of 2 weeks and 3 weeks, also added crude_rate_7d_lead
collated_data_final =
  collated_data_clean %>% 
  mutate(cumulative_cases_2w = RcppRoll::roll_sum(new_cases_today, 14, fill = 0, align = "right")) %>% 
  mutate(cumulative_cases_3w = RcppRoll::roll_sum(new_cases_today, 21, fill = 0, align = "right")) %>% 
  left_join(population_community, by = c("place_ID" = "place_ID")) %>% 
  mutate(cumulative_2w_pop_adj = cumulative_cases_2w/population*100) %>% 
  mutate(cumulative_3w_pop_adj = cumulative_cases_3w/population*100) %>% 
  mutate(new_3d_MA_pop_adj = case_incremental_3d_MA/population*100) %>% 
  mutate(crude_rate = cumulative_cases_today/population) %>% 
  mutate(crude_rate_7d_lead = lead(crude_rate, 7)) %>% 
  select(place_ID, date, population, crude_rate, crude_rate_7d_lead, # outcome (predict variable for "individual")
         new_7d_lead, new_3d_lead, # outcome (predict variable for "outbreak")
         cumulative_3w_pop_adj, cumulative_2w_pop_adj, new_3d_MA_pop_adj, # adjusted variables by population size
         new_cases_today, cumulative_cases_today, case_incremental_3d_MA, case_incremental_7d_MA, 
         if_first_report, last_update)

# save file
collated_data_final = as.data.frame(collated_data_final)
write.csv(collated_data_final, paste0(cleaned_data_path, "community_TimeCases_long.csv"), row.names = F)
print("community_TimeCases_long.csv created.")


#=============================================================
# Cleaning of community-level case/test/death data -- TODAY! #
#=============================================================
CSA_case_death = read.csv(paste0(county_data_path, "deathsbycommunity.csv"))

# Merge Community level data together
community_merge = 
  CSA_test_table %>% 
  # select(-population) %>% 
  full_join(CSA_case_death, by = c("geo_merge" = "geo_merge")) %>% 
  select(-population.y) %>% 
  rename(population = population.x)
# Clean place_ID
community_merge_CleanID =
  community_merge %>% 
  mutate(place_ID_1 = gsub(pattern = "City of ", replacement = '', x = community_merge$geo_merge)) 
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID_2 = gsub(pattern = "Los Angeles - ", replacement = '', x = community_merge_CleanID$place_ID_1)) 
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID_3 = gsub(pattern = "[[:punct:]]+", replacement = '', x = community_merge_CleanID$place_ID_2))
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = community_merge_CleanID$place_ID_3)) %>% 
  select(-c(place_ID_1, place_ID_2, place_ID_3))
# Check for labels
# community_geo_label = 
#   community_merge_CleanID %>% 
#   select(place_ID, geo_merge) %>% 
#   distinct(place_ID, .keep_all = TRUE) 

# Clean the place_ID (LA_cases_time_series)
LA_cases_time_series_clean =
  LA_cases_time_series %>%
  mutate(place_ID_1 = gsub(pattern = "[[:punct:]]+", replacement = '', x = LA_cases_time_series$place))
LA_cases_time_series_clean =
  LA_cases_time_series_clean %>%
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = LA_cases_time_series_clean$place_ID_1)) %>% 
  select(place_ID, longitude, latitude) %>% 
  distinct()

# Select potential useful variables
community_merge_today =
  community_merge_CleanID %>% 
  left_join(LA_cases_time_series_clean, by = c("place_ID" = "place_ID")) %>% 
  rename("cumulative_cases_today" = "cases_final", "adj_case_rate" = "adj_case_rate_final", 
         "cumulative_deaths_today" = "deaths_final", "adj_death_rate" = "adj_death_rate_final",
         "cumulative_persons_tested_today" = "persons_tested_final", "adj_test_rate" = "adj_testing_rate_final",
         "cumulative_persons_pos_today" = "persons_tested_pos_final", "adj_test_pos_rate" = "adj_pos_testing_rate_final") %>% 
  filter(place_ID %in% unique(collated_data_clean$place_ID)) %>% 
  select(place_ID, longitude, latitude, population, cumulative_cases_today, adj_case_rate, cumulative_deaths_today, adj_death_rate,
         cumulative_persons_tested_today, adj_test_rate, cumulative_persons_pos_today, adj_test_pos_rate, geo_merge)

# save file
write.csv(community_merge_today, paste0(cleaned_data_path, "community_todayUpdated.csv"), row.names = F)
write.csv(community_merge_today, paste0(interpolation_path, "community_todayUpdated.csv"), row.names = F)
print("community_todayUpdated.csv created.")
  

#=============================================================
# Cleaning of county-level case/test/death data -- TODAY! #
#=============================================================
county_case_death = read.csv(paste0(county_data_path, "deathsbydate.csv"))
county_dailytesting = read.csv(paste0(county_data_path, "dailytesting.csv"))

county_summary = 
  county_case_death %>% 
  left_join(county_dailytesting, by = c("date_use" = "date_use")) %>% 
  mutate(percent_positive_avg_tests = as.numeric(str_replace(percent_positive_avg_tests, "%", ""))/100) %>% 
  select(date_use, total_cases, new_case, total_deaths, new_deaths, percent_positive_avg_tests) %>% 
  rename(Date = date_use, TotalCases = total_cases, DailyCases = new_case, TotalDeaths = total_deaths, 
         NewDeaths = new_deaths, PercentPosAvg = percent_positive_avg_tests)

# save file
write.csv(county_summary, paste0(cleaned_data_path, "LA_county.csv"), row.names = F)
print("LA_county.csv created.")


#===============================================
# Reproduce interpolation data #
#===============================================
demographic_file = read.csv(paste0(county_data_path, "demographics.csv"))
write.csv(demographic_file, paste0(interpolation_path, "demographics.csv"), row.names = F)
community_demographic_cleaned = read.csv(paste0(data_path, "static/community_demographic_cleaned.csv"))
write.csv(community_demographic_cleaned, paste0(interpolation_path, "community_demographic_cleaned.csv"), row.names = F)
setwd(interpolation_path)
reticulate::source_python(paste0(code_path, "interpolation.py"))

#======================================
# Create a dataset for BBN modeling #
#======================================
community_demo = read.csv(paste0(data_path, "static/community_demographic_cleaned.csv")) # N = 265 obs

all_date = unique(collated_data_final$date)
date_choose = all_date[seq(1, length(all_date), 21)]
merged_community_expand = 
  collated_data_final %>% 
  filter(date %in% date_choose) %>% 
  inner_join(community_demo, by = c("place_ID" = "place_ID")) %>% 
  drop_na() %>% 
  filter(if_first_report != 1)
# save file
save(merged_community_expand, file = paste0(modeling_path, "merged_community_expand.rda"))
print("merged_community_expand.rda created.")

most_recent = max(collated_data_final$date)
merged_community_mst_recent = 
  collated_data_final %>% 
  filter(date == most_recent) %>% 
  inner_join(community_demo, by = c("place_ID" = "place_ID")) %>% 
  filter(!is.na(population)) %>% 
  filter(if_first_report != 1)
save(merged_community_mst_recent, file = paste0(modeling_path, "merged_community_mst_recent.rda"))
print("merged_community_mst_recent.rda created.")

rm(list = ls())
