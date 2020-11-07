# Let's try some BNN
if(!require(bnlearn)) install.packages("bnlearn", repos = "http://cran.us.r-project.org")
if(!require(visNetwork)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("visNetwork", repos = "http://cran.us.r-project.org")

local_path = "/Users/Mingzhou/Desktop/2020_Fall/BE 223A/Group_project/UCLA-BE-223a-Covid-19-Project/"
app_path = paste0(local_path, "COVID_shinyLA/")
cleaned_data_path = paste0(app_path, 'output_clean/')
community_today = read.csv(paste0(cleaned_data_path, "community_todayUpdated.csv"))
cv_cases_longitudinal = read.csv(paste0(cleaned_data_path, "community_TimeCases_long.csv"))
# N = 335 obs
community_demo = read.csv(paste0(paste0(local_path, "neighborhoods/"), "neighborhood_data_latimes.csv"))
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

merged_community %>% 
  group_by(place_ID) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)
# No duplicates

test_model_data =
  merged_community %>% 
  mutate(outbreak = as.factor(case_when(
    new_cases >= 3 ~ "Yes",
    !is.na(new_cases) ~ "No"
  ))) %>% 
  mutate(cumulative_cases_cat = as.factor(case_when(
    cumulative_cases > 1000 ~ "1000 and more",
    cumulative_cases > 500 ~ "500 to 1000",
    cumulative_cases > 0 ~ "Less than 500"
  ))) %>% 
  mutate(place_ID = as.factor(place_ID)) %>% 
  mutate(age10_less_cat = as.factor(case_when(
    age10_less < 0.05 ~ "less than 5%",
    age10_less < 0.15 ~ "5-15%",
    age10_less < 0.25 ~ "15-25%",
    TRUE ~ "larger than 25%"
  ))) %>% 
  mutate(high_school_orless_cat = as.factor(case_when(
    high_school_orless < 0.05 ~ "less than 5%",
    high_school_orless < 0.25 ~ "5-25%",
    high_school_orless < 0.50 ~ "25-55%",
    TRUE ~ "larger than 50%"
  ))) %>% 
  mutate(non_white_cat = as.factor(case_when(
    non_white < 0.15 ~ "less than 15%",
    high_school_orless < 0.30 ~ "15-30%",
    high_school_orless < 0.50 ~ "30-50%",
    high_school_orless < 0.75 ~ "50-75%",
    TRUE ~ "larger than 75%"
  ))) %>% 
  mutate(pop_per_sqmi_cat = as.factor(case_when(
    pop_per_sqmi < 500 ~ "less than 500",
    pop_per_sqmi < 5000 ~ "500-5000",
    pop_per_sqmi < 10000 ~ "5000-10000",
    TRUE ~ "larger than 10000"
  )))
# Distribution of outbreadk -- Yes/No = 127/83


#==========================
# Build the BNN model

# create an empty graph
structure = empty.graph(c("place_ID", "cumulative_cases_cat", "outbreak", "age10_less_cat", "high_school_orless_cat", "non_white_cat", "pop_per_sqmi_cat"))

# set relationships manually
modelstring(structure) = "[place_ID][cumulative_cases_cat][age10_less_cat][high_school_orless_cat][non_white_cat][pop_per_sqmi_cat][outbreak|place_ID:cumulative_cases_cat:age10_less_cat:high_school_orless_cat:non_white_cat:pop_per_sqmi_cat]"

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


# observe structure
plot.network(structure)

# subset and fit 
test_subset = 
  test_model_data %>% 
  select(place_ID, cumulative_cases_cat, outbreak, non_white_cat, age10_less_cat, high_school_orless_cat, pop_per_sqmi_cat) 
test_subset = as.data.frame(test_subset)
bn.mod = bn.fit(structure, data = test_subset)
bn.mod


# Example: outbreak tomorrow in North Hollywood
cat("P(outbreak tomorrow | live at North Hollywood =", 
    cpquery(bn.mod, (outbreak == "Yes"), (place_ID == "North Hollywood" & cumulative_cases_cat == "Less than 500" &
                                            pop_per_sqmi_cat == "5000-10000")), "\n")

cat("P(outbreak tomorrow | sudo community =",
    cpquery(bn.mod, (outbreak == "Yes"), (cumulative_cases_cat == "Less than 500" ), "\n"))

