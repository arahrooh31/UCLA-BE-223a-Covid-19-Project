##########
# Import libraries
##########
import pandas as pd
import numpy as np

#####################################################################################################
# PREPROCESSING
#####################################################################################################
##########
# Read in city demographics csv file
##########
demographics_df = pd.read_csv(r'community_demographic_cleaned.csv')

# Get only race/ethnicity data from demographics_df
city_race_percent_df = demographics_df[["Neighborhood", "ASIAN.POPULATION", "BLACK.POPULATION",
                                "LATINO.POPULATION", "WHITE.POPULATION", "NON.WHITE.POPULATION",
                               "POPULATION.TOTAL"]]
# Sory by alphabetical city name
city_race_percent_df = city_race_percent_df.sort_values(by='Neighborhood', ignore_index=True)


##########
# Calculate the total population of each race in each city
# (# people of given race) = (% of city that is given race) * (# people in city)
##########

# This stores column name and corresponding number from city_race_percent_df
column_codes =  [('ASIAN', 1), ('BLACK', 2), ('LATINO', 3), ('WHITE', 4), ('NON.WHITE', 5)]

# Initialize our empty dictionary
city_race_info = {}
city_race_info['CITY'] = []
for race, j in column_codes:
    city_race_info[race] = []

# Fill in dictionary
for i, row in city_race_percent_df.iterrows():
    # Total population of a given city
    total_pop = row[6]
    # Store name of city in city_race_info
    city_name = row[0]
    city_race_info['CITY'].append(city_name)
    # Loop through all races in city_race_percent_df
    for race, j in column_codes:
        city_race_info[race].append(row[j] * total_pop)
            
# Convert to DataFrame and sort by alphabetical city-name
city_race_df = pd.DataFrame(city_race_info)
city_race_df = city_race_df.sort_values(by='CITY', ignore_index=True)


###########
# Read in city covid data
##########
covid_df = pd.read_csv(r'community_todayUpdated.csv')

# Get only city name, city population, and cumulative case numbers from covid_df
city_covid_df = covid_df[["place_ID", "population", "cumulative_cases_today"]]
city_covid_df = city_covid_df.rename(columns={"place_ID":"CITY", 
                                              "population":"POPULATION", 
                                              "cumulative_cases_today":"POSITIVES"})
city_covid_df = city_covid_df.sort_values(by='CITY', ignore_index=True)


##########
# Concatenate all data for each city into one dataframe
##########
# This stores column name and corresponding number from city_race_percent_df
column_codes =  [('ASIAN', 1), ('BLACK', 2), ('LATINO', 3), ('WHITE', 4), ('NON.WHITE', 5)]

# Initialize empty dictionary
city_info = {}
city_info['CITY'] = []
for race, j in column_codes:
    city_info[race] = []
city_info['POSITIVES'] = []
city_info['POPULATION'] = []

# Fill in dictionary
for i, city_race_row in city_race_df.iterrows():
    # Get city name
    city = city_race_row[0]
    
    # Find city in city covid data
    city_covid_data = city_covid_df.loc[city_covid_df['CITY'] == city]
    # if city does not exist in city covid data, then continue to next city
    if city_covid_data.empty:
        continue
    # if city exists in both city demographic data and city covid data, then add it to city info 
    else:
        # add demographic data
        city_info['CITY'].append(city)
        for race, j in column_codes:
            city_info[race].append(city_race_row[j])

        # add covid data
        index = city_covid_df.loc[city_covid_df['CITY'] == city].index.to_numpy().item()
        city_info['POPULATION'].append(city_covid_data['POPULATION'].loc[index])
        city_info['POSITIVES'].append(city_covid_data['POSITIVES'].loc[index])

city_df = pd.DataFrame(city_info)


##########
# Read in county-level covid data
##########
county_race_covid = {'Race/Ethnicity':['American Indian/Alaska Native','Asian', 
                                       'Black','Hispanic/Latino', 
                                      'Native Hawaiian/Pacific Islander','White'],
                    'Cases':[316, 10473, 10113, 129999, 967, 28990]}
county_race_covid_df = pd.DataFrame(county_race_covid)


# Clean up county_race_covid_df
county_race_covid_df.drop(index=0, inplace=True)
county_race_covid_df.drop(index=4, inplace=True)

non_white = 0
for i, row in county_race_covid_df.iterrows():
    upper = row[0].upper()
    if upper == 'HISPANIC/LATINO':
        upper = 'LATINO'
    county_race_covid_df.at[i, 'Race/Ethnicity'] = upper
    if upper != 'WHITE':
        non_white += county_race_covid_df.at[i, 'Cases']

non_white_row = {'Race/Ethnicity':'NON.WHITE', 
                 'Cases' : non_white}
county_race_covid_df = county_race_covid_df.append(non_white_row, ignore_index=True)


##########
# Read in county-level demographics data
# Demographics data for LA County from U.S. Census Bureau
# https://www.census.gov/quickfacts/losangelescountycalifornia
##########
county_race = {'Race/Ethnicity':['American Indian/Alaska Native',
                                    'Asian', 'Black','Hispanic/Latino', 
                                    'Native Hawaiian/Pacific Islander', 'White',],
                'Percent':[0.004, 0.154, 0.09, 0.486, 0.004, 0.261],
              }
total_pop = 10039107
pop_num = np.zeros_like(county_race['Percent'])
for i in np.arange(len(county_race['Percent'])):
    pop_num[i] = int(county_race['Percent'][i] * total_pop)
county_race['Population'] = pop_num

county_race_df = pd.DataFrame(county_race)
county_race_df.drop(columns='Percent', inplace=True)

# Clean up county_race_df
county_race_df.drop(index=0, inplace=True)
county_race_df.drop(index=4, inplace=True)

non_white = 0
for i, row in county_race_df.iterrows():
    upper = row[0].upper()
    if upper == 'HISPANIC/LATINO':
        upper = 'LATINO'
    county_race_df.at[i, 'Race/Ethnicity'] = upper
    if upper != 'WHITE':
        non_white += county_race_df.at[i, 'Population']

non_white_row = {'Race/Ethnicity':'NON.WHITE', 
                 'Population' : non_white}
county_race_df = county_race_df.append(non_white_row, ignore_index=True)


##########
# Merge county_race_df with county_race_covid_df
##########
county_info = {'RACE':[], 'POPULATION':[], 'POSITIVES':[]}
for i, row in county_race_df.iterrows():
    county_info['RACE'].append(row[0])
    county_info['POPULATION'].append(row[1])
    county_info['POSITIVES'].append(county_race_covid_df.at[i, 'Cases'])
        
county_df = pd.DataFrame(county_info)



#####################################################################################################
# DATA INTERPOLATION
#####################################################################################################
'''
PROPORTION

# interpolated positive cases per race in city      # positive cases per race in county
-------------------------------                  =   ------------------------------------
# people of race in city                            # people of race in county

SCALING

# true positive cases per race in city =   # interpolated positive cases per race in city
                                             ----------------------------------------
                                            total interpolated cases for all races in city
'''

# empty dataframe that will store percentage of each city's positive cases that come from each race
city_positive_race_df = pd.DataFrame(columns=['CITY', 'ASIAN', 'BLACK', 'LATINO', 'WHITE', 'NON.WHITE',
                                             'TOTAL.POP', 'TOTAL.CASES'])
# this stores the column names and corresponding number in city_df
column_codes = [('ASIAN', 1), ('BLACK', 2), ('LATINO', 3), ('WHITE', 4), ('NON.WHITE', 5)]

# iterate through all the rows in city_race_df
for i, city_row in city_df.iterrows():
    # empty dictionary to story the data from a given city
    city_info = {}
    
    # get neighborhood name from city_race_df and store it in city_info
    city_name = city_row[0]
    city_info['CITY'] = city_name
    
    total_interpolated = 0
    raw_race = {}
    for race, j in column_codes:        
        # get population of given race in the city
        pop_city_race = city_row[j]
    
        county_race_data = county_df.loc[county_df['RACE'] == race]
        # get population of given race in the county
        pop_county_race = county_race_data['POPULATION'].loc[j-1]
        # get number of positive cases in the county for given race
        num_county_positive_race = county_race_data['POSITIVES'].loc[j-1]

        #calculate number of positives for given race in city 
        raw_race[race] = int((num_county_positive_race / pop_county_race) * pop_city_race)
        total_interpolated += raw_race[race]
    
    # scale number of cases by total cases in city
    total_cases_in_city = city_row[6]
    for race, j in column_codes[0:5]:
        scaled_num_positive = int((raw_race[race] / total_interpolated) * total_cases_in_city)
        city_info[race] = scaled_num_positive
    
    city_info['TOTAL.CASES'] = total_cases_in_city
    city_info['TOTAL.POP'] = city_row[7]
    
    # append city_info to city_positive_race_df
    city_positive_race_df = city_positive_race_df.append(city_info, ignore_index=True)
    
    
##########
# Save output to csv
##########
city_positive_race_df.to_csv('race-ethnicity_interpolated.csv')
