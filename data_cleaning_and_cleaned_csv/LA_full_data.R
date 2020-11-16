## Covid-2019 interactive mapping tool: script to reformat New York Times US state-level data
## Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine, April 2020

## data extracted from New York Times state-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data

## state-level population information obtained from US census data
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

library(tidyverse)
options(warn = -1)

local_data_path = "/Users/Mingzhou/Desktop/2020_Fall/BE 223A/Group_project/data/"
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
write.csv(CSA_cases_updated, paste0(local_data_path, "input_data/CSA_cases_updated.csv"))

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
# allow for repatriation or reassigned cases without negative new_cases
collated_data$new_cases[collated_data$new_cases < 0 | collated_data$new_cases > 10000] = 0

# re-order
collated_data = collated_data[order(as.Date(collated_data$date, format = "%Y-%m-%d"), -collated_data$cases),]

# update time stamp
collated_data$last_update = NA
collated_data$last_update[nrow(collated_data)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))

# Clean the place_ID
collated_data_clean =
  collated_data %>%
  mutate(place_ID_1 = gsub(pattern = "[[:punct:] ]+", replacement = ' ', x = collated_data$place_ID))
collated_data_clean =
  collated_data_clean %>%
  mutate(place_ID_2 = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = collated_data_clean$place_ID_1))
collated_data_clean =
  collated_data_clean %>%
  mutate(place_ID = gsub(pattern = "Unincorporated ", replacement = '', x = collated_data_clean$place_ID_2)) %>%
  select(-c(place_ID_1, place_ID_2))

collated_data_clean$date = as.Date(collated_data_clean$date, format = "%Y-%m-%d")

# save file
write.csv(collated_data_clean, paste0(local_data_path, "input_data/coronavirus.csv"), row.names = F)


#=============================================================================
# Other data downloaded from CSDPH
LA_case_death = read.csv(paste0(local_data_path, "LA_County_Covid19_cases_deaths_date_table.csv"), row.names = 1)
LA_test = read.csv(paste0(local_data_path, "LA_County_Covid19_tests_date_table.csv"), row.names = 1)
LA_person_test = read.csv(paste0(local_data_path, "LA_County_Covid19_persons_tested_date_table.csv"), row.names = 1)
CSA_case_death = read.csv(paste0(local_data_path, "LA_County_Covid19_CSA_case_death_table.csv"), row.names = 1)
CSA_test_table = read.csv(paste0(local_data_path, "LA_County_Covid19_CSA_testing_table.csv"), row.names = 1)

# Merge all LA county data together
LA_full_merge = 
  LA_case_death %>% 
  left_join(LA_test, by = c("date_use" = "date_use")) %>% 
  left_join(LA_person_test, by = c("date_use" = "date_use"))
# re-order
LA_full_merge$date_use = as.Date(LA_full_merge$date_use, format = "%Y-%m-%d")
LA_full_merge = LA_full_merge[order(LA_full_merge$date_use), ]

# update time stamp
LA_full_merge$last_update = NA
LA_full_merge$last_update[nrow(LA_full_merge)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))

# save file
write.csv(LA_full_merge, paste0(local_data_path, "input_data/LA_full_merge.csv"), row.names = F)

#=============================================================================
# Merge Community level data together
community_merge = 
  CSA_test_table %>% 
  select(-population) %>% 
  full_join(CSA_case_death, by = c("geo_merge" = "geo_merge"))

# Clean place_ID
community_merge_CleanID =
  community_merge %>% 
  mutate(place_ID_1 = gsub(pattern = "City of ", replacement = '', x = community_merge$geo_merge)) 
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID_2 = gsub(pattern = "Los Angeles - ", replacement = '', x = community_merge_CleanID$place_ID_1)) 
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID_3 = gsub(pattern = "Unincorporated - ", replacement = '', x = community_merge_CleanID$place_ID_2))
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID_4 = gsub(pattern = "[[:punct:] ]+", replacement = ' ', x = community_merge_CleanID$place_ID_3))
community_merge_CleanID =
  community_merge_CleanID %>% 
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = community_merge_CleanID$place_ID_4)) %>% 
  select(-c(place_ID_1, place_ID_2, place_ID_3, place_ID_4))

community_geo_label = 
  community_merge_CleanID %>% 
  select(place_ID, geo_merge) %>% 
  distinct(place_ID, .keep_all = TRUE) 

location_community =
  LA_cases_time_series %>%
  mutate(place_ID_1 = gsub(pattern = "[[:punct:] ]+", replacement = ' ', x = LA_cases_time_series$place))
location_community =
  location_community %>%
  mutate(place_ID_2 = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = location_community$place_ID_1))
location_community =
  location_community %>%
  mutate(place_ID = gsub(pattern = "Unincorporated ", replacement = '', x = location_community$place_ID_2)) %>%
  select(-c(place_ID_1, place_ID_2))

location_community =
  location_community %>% 
  select(place_ID, longitude, latitude) %>% 
  distinct(place_ID, .keep_all = TRUE) 

# Combine data from the same area
community_merge_final = 
  community_merge_CleanID %>% 
  group_by(place_ID) %>% 
  summarise(n = n(), place_ID, population = sum(population), cases_final = sum(cases_final), case_rate_final = sum(case_rate_final)/n, adj_case_rate_final = sum(adj_case_rate_final)/n,
            deaths_final = sum(deaths_final), death_rate_final = sum(death_rate_final)/n, adj_death_rate_final = sum(adj_death_rate_final)/n,
            persons_tested_final = sum(persons_tested_final), testing_rate_final = sum(testing_rate_final)/n, adj_testing_rate_final = sum(adj_testing_rate_final)/n, 
            persons_tested_pos_final = sum(persons_tested_pos_final), pos_testing_rate_final = sum(pos_testing_rate_final)/n, adj_pos_testing_rate_final = sum(adj_pos_testing_rate_final)/n) %>% 
  ungroup() %>% 
  distinct() %>% 
  left_join(community_geo_label) %>% 
  left_join(location_community)

community_merge_final =
  community_merge_final %>% 
  mutate(if_combined = if_else(n == 1, 0, 1)) %>% 
  select(-n)

# save file
write.csv(community_merge_final, paste0(local_data_path, "input_data/community_merge.csv"), row.names = F)
rm(list = ls())
