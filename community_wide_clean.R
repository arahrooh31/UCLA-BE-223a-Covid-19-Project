## Covid-2019 interactive mapping tool: script to reformat New York Times LA community-level data

## data extracted from New York Times state-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data

## community-level population information obtained from US census data
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
seismicRoll
options(warn = -1)

local_path = "/Users/Mingzhou/Desktop/2020_Fall/BE 223A/Group_project/UCLA-BE-223a-Covid-19-Project/"
app_path = paste0(local_path, "COVID_shinyLA/")
cleaned_data_path = paste0(app_path, 'output_clean/')
county_data_path = paste0(app_path, 'county/')
latimes_data_path = paste0(app_path, 'csv/')
demographic_data_path = paste0(app_path, 'neighborhoods/')

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
  mutate(case_incremental_3d_MA = rollmean(new_cases, 3, align = "right", fill = 0)) %>% 
  mutate(case_incremental_7d_MA = rollmean(new_cases, 7, align = "right", fill = 0)) %>% 
  mutate(new_3d_lead = lead(new_cases, 3)) %>% 
  mutate(new_7d_lead = lead(new_cases, 7)) %>% 
  rename("new_cases_today" = "new_cases") %>% 
  rename("cumulative_cases_today" = "cases") %>% 
  select(place_ID, date, new_cases_today, cumulative_cases_today, case_incremental_3d_MA, case_incremental_7d_MA,
         new_3d_lead, new_7d_lead, last_update)

# save file
write.csv(collated_data_clean, paste0(cleaned_data_path, "community_TimeCases_long.csv"), row.names = F)
print("community_TimeCases_long.csv created.")


#=============================================================
# Cleaning of community-level case/test/death data -- TODAY! #
#=============================================================
CSA_case_death = read.csv(paste0(county_data_path, "deathsbycommunity.csv"))
CSA_test_table = read.csv(paste0(county_data_path, "communitytesting.csv"))

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

# Select potential useful variables
community_merge_today =
  community_merge_CleanID %>% 
  rename("cumulative_cases_today" = "cases_final", "adj_case_rate" = "adj_case_rate_final", 
         "cumulative_deaths_today" = "deaths_final", "adj_death_rate" = "adj_death_rate_final",
         "cumulative_persons_tested_today" = "persons_tested_final", "adj_test_rate" = "adj_testing_rate_final",
         "cumulative_persons_pos_today" = "persons_tested_pos_final", "adj_test_pos_rate" = "adj_pos_testing_rate_final") %>% 
  select(place_ID, cumulative_cases_today, adj_case_rate, cumulative_deaths_today, adj_death_rate,
         cumulative_persons_tested_today, adj_test_rate, cumulative_persons_pos_today, adj_test_pos_rate)

# save file
write.csv(community_merge_today, paste0(cleaned_data_path, "community_todayUpdated.csv"), row.names = F)
print("community_todayUpdated.csv created.")


#===============================================
# Cleaning of community-level demographic data #
#===============================================
community_demo = read.csv(paste0(demographic_data_path, "neighborhood_data_latimes.csv"))
# N = 265 obs
# Clean place_ID
community_demo_CleanID =
  community_demo %>% 
  mutate(place_ID_1 = gsub(pattern = "[[:punct:]]+", replacement = '', x = community_demo$Neighborhood))
community_demo_CleanID =
  community_demo_CleanID %>% 
  mutate(place_ID = gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = community_demo_CleanID$place_ID_1)) %>% 
  select(-place_ID_1)
# Check for duplicate IDs -- No duplicate place_ID
# community_demo_CleanID %>%
#   group_by(place_ID) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)

community_demo_numeric =
  community_demo_CleanID %>% 
  mutate_at(vars(AGE.65.AND.UP:FOREIGN.BORN.POPULATION, FOUR.YEAR.DEGREES:MASTERS.DEGREE.OR.HIGHER, ASIAN.POPULATION:SINGLE.PARENTS,
                 HOMEOWNERS:RENTERS, X.20.000.or.less:VETERANS), ~ as.numeric(str_replace(., "%", ""))/100) %>% 
  mutate(Median.Income = as.numeric(gsub(pattern = "[[:punct:]]+", replacement = '', x = Median.Income))) %>% 
  mutate(POPULATION.TOTAL = as.numeric(gsub(pattern = "[[:punct:]]+", replacement = '', x = POPULATION.TOTAL))) %>% 
  mutate(POPULATION.PER.SQMI = as.numeric(gsub(pattern = "[[:punct:]]+", replacement = '', x = POPULATION.PER.SQMI)))

# save file
write.csv(community_demo_numeric, paste0(cleaned_data_path, "community_demographic_cleaned.csv"), row.names = F)
print("community_demographic_cleaned.csv created.")
  

rm(list = ls())
