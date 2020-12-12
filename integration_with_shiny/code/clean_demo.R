#===============================================
# Cleaning of community-level demographic data #
#===============================================
# local_path = "/Users/Mingzhou/Desktop/2020_Fall/BE 223A/Group_project/new/"
working_dir = getwd()
data_path = paste0(working_dir, "/data/")
output_data_path = paste0(data_path, 'static/')

community_demo = read.csv(paste0(data_path, "static/neighborhood_data_latimes.csv"))
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
write.csv(community_demo_numeric, paste0(output_data_path, "community_demographic_cleaned.csv"), row.names = F)
print("community_demographic_cleaned.csv created.")
