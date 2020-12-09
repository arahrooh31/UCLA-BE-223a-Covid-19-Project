import pandas as pd
import numpy as np 

##Dataframes: Place/City/Neighborhood Demographics
#Makes a dataframe of the age columns from the LA Times neighborhood data
hoodDemoDF = pd.read_csv('community_demographic_cleaned.csv', encoding = "ISO-8859-1")
neighborhood_DemographicsDF = hoodDemoDF[['Neighborhood','AGE.10.OR.LESS', 'AGE.11.TO.18','AGE.19.TO.34','AGE.35.TO.49','AGE.50.TO.64','AGE.65.AND.UP','POPULATION.TOTAL']].loc[1:len(hoodDemoDF)]

#Combines age columns to make them cohere with the LA County data
hood_DemographicsDF = pd.DataFrame()
hood_DemographicsDF['NEIGHBORHOOD'] = neighborhood_DemographicsDF['Neighborhood']
hood_DemographicsDF['%AGE_18_OR_LESS'] = neighborhood_DemographicsDF['AGE.10.OR.LESS'] + neighborhood_DemographicsDF['AGE.11.TO.18']
hood_DemographicsDF['%AGE_19_TO_49'] = neighborhood_DemographicsDF['AGE.19.TO.34'] + neighborhood_DemographicsDF['AGE.35.TO.49']
hood_DemographicsDF['%AGE_50_TO_64'] = neighborhood_DemographicsDF['AGE.50.TO.64']
hood_DemographicsDF['%AGE_65_AND_UP'] = neighborhood_DemographicsDF['AGE.65.AND.UP']
hood_DemographicsDF['POPULATION_TOTAL'] = neighborhood_DemographicsDF['POPULATION.TOTAL']
hood_DemographicsDF['SUM_SHOULD_BE_1'] = hood_DemographicsDF['%AGE_18_OR_LESS'] + hood_DemographicsDF['%AGE_19_TO_49'] + hood_DemographicsDF['%AGE_50_TO_64'] + hood_DemographicsDF['%AGE_65_AND_UP']
age18orLessHood = []
age19to49Hood = []
age50to64Hood = []
age65andUp = []
for i in range(1,len(neighborhood_DemographicsDF)+1):
		age18orLessHood.append((neighborhood_DemographicsDF['AGE.10.OR.LESS'].loc[i] + neighborhood_DemographicsDF['AGE.11.TO.18'].loc[i])*neighborhood_DemographicsDF['POPULATION.TOTAL'].loc[i])
		age19to49Hood.append((neighborhood_DemographicsDF['AGE.19.TO.34'].loc[i] + neighborhood_DemographicsDF['AGE.35.TO.49'].loc[i])*neighborhood_DemographicsDF['POPULATION.TOTAL'].loc[i])
		age50to64Hood.append(neighborhood_DemographicsDF['AGE.50.TO.64'].loc[i]*neighborhood_DemographicsDF['POPULATION.TOTAL'].loc[i])
		age65andUp.append(neighborhood_DemographicsDF['AGE.65.AND.UP'].loc[i]*neighborhood_DemographicsDF['POPULATION.TOTAL'].loc[i])
hood_DemographicsDF['#18_or_Less'] = age18orLessHood
hood_DemographicsDF['#19_to_49'] = age19to49Hood
hood_DemographicsDF['#50_to_64'] = age50to64Hood
hood_DemographicsDF['#65_and_Up'] = age65andUp

##Dataframes: LA County Demographics
LADemoDF = pd.read_csv('demographics.csv')
LAAP = LADemoDF[['POP']].loc[0:7]  #Defining the countywide population breakdown as a variable for readability purposes

tempLADemographicsDF = pd.DataFrame()
tempLADemographicsDF['AgeCategories'] = LADemoDF['characteristic']
tempLADemographicsDF['PopulationInAgeGroups'] = LAAP
LA_CountyPopulation = LAAP.sum()

#Combines the age categories for the LA County data into comparable categories with the Place data because the Place/neighborhood
#data doesn't have the same categories. Note: LA County's '18 or less' only has 5-17. Otherwise, categories line up
LA_DemographicsDF = pd.DataFrame()
LA_DemographicsDF['AgeCategories'] = ['AGE_18_OR_LESS','AGE_19_TO_49', 'AGE_50_TO_64', 'AGE_65_AND_UP']
LA_DemographicsDF['PopulationByAge'] = [tempLADemographicsDF['PopulationInAgeGroups'].loc[0:2].sum(), tempLADemographicsDF['PopulationInAgeGroups'].loc[3:4].sum(), tempLADemographicsDF['PopulationInAgeGroups'].loc[5].sum(), tempLADemographicsDF['PopulationInAgeGroups'].loc[6:7].sum()]

##Dataframes: LA County COVID
tempLA_COVIDDF = pd.read_csv('demographics.csv')
tempLA_COVIDDF['%OfTotalKnownCases'] = tempLA_COVIDDF['total']/tempLA_COVIDDF['total'][0:8].sum()
# tempLA_COVIDDF['Age'][9] = "Sum"

LA_COVIDDF = pd.DataFrame()
LA_COVIDDF['AgeCategories'] = ['18_Or_Less','19_To_49', '50_To_64', '65_And_Up', 'Unknown_Age','Sum_of_KnownAgeCases', 'SumOfAllCases']
LA_COVIDDF['%OfTotalKnownCases'] = [(tempLA_COVIDDF['%OfTotalKnownCases'].loc[0:2]).sum(), (tempLA_COVIDDF['%OfTotalKnownCases'].loc[3:4]).sum(), (tempLA_COVIDDF['%OfTotalKnownCases'].loc[5]), (tempLA_COVIDDF['%OfTotalKnownCases'].loc[6:7]).sum(), (tempLA_COVIDDF['%OfTotalKnownCases'].loc[8]), (tempLA_COVIDDF['%OfTotalKnownCases'].loc[0:7]).sum(),(tempLA_COVIDDF['%OfTotalKnownCases'].loc[0:8]).sum()]
LA_COVIDDF['Cases'] = [(tempLA_COVIDDF['total'].loc[0:2]).sum(), (tempLA_COVIDDF['total'].loc[3:4]).sum(), (tempLA_COVIDDF['total'].loc[5]).sum(), (tempLA_COVIDDF['total'].loc[6:7]).sum(),  (tempLA_COVIDDF['total'].loc[8]).sum(), (tempLA_COVIDDF['total'].loc[0:7]).sum(), ((tempLA_COVIDDF['total'].loc[0:8]).sum())]
LA_COVIDDF['EstimatedCases'] = LA_COVIDDF['%OfTotalKnownCases']*LA_COVIDDF['Cases'][5] 
#Distributing ^ unknown cases into known cases categories

##Dataframes: Place/City/Neighborhood
hood_COVIDDF = pd.read_csv('community_todayUpdated.csv')

'''Cleaning City Names'''
#Removes "City of", "Unincorporated -", and "Los Angeles -" prefixes in hood_COVIDDF so the place_IDs can match 
#the NEIGHBORHOODS in nbhdAges
city_of = "City of "
unincorporated = "Unincorporated "
LosAngeles = "Los Angeles - "
for i, row in hood_COVIDDF.iterrows():
	if city_of in row[0]:
		new_name = row[0].replace(city_of, '')
		hood_COVIDDF.at[i, 'place_ID'] = new_name
	elif unincorporated in row[0]:
		new_name = row[0].replace(unincorporated, '')
		hood_COVIDDF.at[i, 'place_ID'] = new_name
	elif LosAngeles in row[0]:
		new_name = row[0].replace(LosAngeles, '')
		hood_COVIDDF.at[i, 'place_ID'] = new_name
#The number of cases whose locations are known. There are places which don't report COVID data.


##Calculation: Interpolation of Cities' Case Demographics
#Declaring variables
R = []
places = []
pop18OrLess = []
pop19To49 = []
pop50To64 = []
pop65AndUp = []
Totalpopulation = []
cityCase_18OrLess = []
cityCase_19To49 = []
cityCase_50To64 = []
cityCase_50To64 = []
cityCase_65AndUp = []
sumOfcityCase = []
totalR = 0
#Interpolating cities' cases for age categories based on LA County infected percents, LA County demographics,and the Places' demographics
#Note: This does not take into account the actual number of cases in a city. Thus, there will be estimated cases for cities whose actual cases are not known.
for i in range(1,len(hood_DemographicsDF)+1):
	places.append(hood_DemographicsDF['NEIGHBORHOOD'][i])
	Totalpopulation.append(hood_DemographicsDF['POPULATION_TOTAL'][i])
	pop18OrLess.append(hood_DemographicsDF['#18_or_Less'][i])
	pop19To49.append(hood_DemographicsDF['#19_to_49'][i])
	pop50To64.append(hood_DemographicsDF['#50_to_64'][i])
	pop65AndUp.append(hood_DemographicsDF['#65_and_Up'][i])
	totalR += 1
for i in range(0,totalR): 
	cityCase_18OrLess.append(pop18OrLess[i] * LA_COVIDDF['EstimatedCases'][0] / (LA_DemographicsDF['PopulationByAge'][0]))
	cityCase_19To49.append(pop19To49[i] * LA_COVIDDF['EstimatedCases'][1] / (LA_DemographicsDF['PopulationByAge'][1]))
	cityCase_50To64.append(pop50To64[i] * LA_COVIDDF['EstimatedCases'][2] / (LA_DemographicsDF['PopulationByAge'][2]))
	cityCase_65AndUp.append(pop65AndUp[i] * LA_COVIDDF['EstimatedCases'][3] / (LA_DemographicsDF['PopulationByAge'][3]))
	sumOfcityCase.append(cityCase_18OrLess[i] + cityCase_19To49[i] + cityCase_50To64[i] + cityCase_65AndUp[i]) 
hood_FinalDF = pd.DataFrame({'Place':places, 'TotalPopulation':Totalpopulation, 'pop18OrLess':pop18OrLess, 'pop19To49':pop19To49, 'pop50To64':pop50To64, 'pop65AndUp':pop65AndUp, 'cityCase_18OrLess':cityCase_18OrLess, 'cityCase_19To49':cityCase_19To49, 'cityCase_50To64':cityCase_50To64, 'cityCase_65AndUp':cityCase_65AndUp, 'sumOfcityCase':sumOfcityCase})

#Adds the 'Cases' for each place when it is known. Adds 'NA' if Cases are unknown
zeroes = []
for i in range(0,len(hood_FinalDF)):
	zeroes.append(0)
hood_FinalDF['Cases'] = zeroes
for i in range(0,len(hood_FinalDF)):
	#Finds the corresponding row in the COVID19 data for a place
	correspondingRow = (hood_COVIDDF[hood_COVIDDF['place_ID']==hood_FinalDF['Place'][i]].index.values)
	#If there is COVID data for a place, add the Cases to the combined DF
	if correspondingRow.size > 0: 
		hood_FinalDF['Cases'][i] = hood_COVIDDF['cumulative_cases_today'][correspondingRow[0]]
	#If there is no COVID data for a place, make a really big number for the Cases in the combined DF
	else: 
		hood_FinalDF['Cases'][i] = np.nan


'''Scaling the interpolation by the number of actual cases. Now the interpolated cases will sum to the actual number of cases.'''
hood_FinalDF['cityCase_18OrLess'] = (hood_FinalDF['cityCase_18OrLess']/hood_FinalDF['sumOfcityCase'])*hood_FinalDF['Cases']
hood_FinalDF['cityCase_19To49'] = (hood_FinalDF['cityCase_19To49']/hood_FinalDF['sumOfcityCase'])*hood_FinalDF['Cases']
hood_FinalDF['cityCase_50To64'] = (hood_FinalDF['cityCase_50To64']/hood_FinalDF['sumOfcityCase'])*hood_FinalDF['Cases']
hood_FinalDF['cityCase_65AndUp'] = (hood_FinalDF['cityCase_65AndUp']/hood_FinalDF['sumOfcityCase'])*hood_FinalDF['Cases']
hood_FinalDF['sumOfcityCase'] = hood_FinalDF['cityCase_18OrLess'] + hood_FinalDF['cityCase_19To49'] + hood_FinalDF['cityCase_50To64'] + hood_FinalDF['cityCase_65AndUp']

n = 0
null = np.nan
for i in range(0,len(hood_FinalDF)):
	if hood_FinalDF['cityCase_18OrLess'][i] >= 0:
		n += 1

##Risk Score
hood_FinalDF['rscore_18OrLess'] = hood_FinalDF['cityCase_18OrLess']/hood_FinalDF['pop18OrLess']
hood_FinalDF['rscore_19to49'] = hood_FinalDF['cityCase_19To49']/hood_FinalDF['pop19To49']
hood_FinalDF['rscore_50to65'] = hood_FinalDF['cityCase_50To64']/hood_FinalDF['pop50To64']
hood_FinalDF['rscore_65andUp'] = hood_FinalDF['cityCase_65AndUp']/hood_FinalDF['pop65AndUp']

hood_FinalDF.to_csv('AgeInterpolationFinalDF.csv')
print("AgeInterpolationFinalDF.csv created successfully")

###RACE_ETHNICITY###

#####################################################################################################
# PREPROCESSING
#####################################################################################################
# Read in city demographics csv file
##########
demographics_df = pd.read_csv(r'community_demographic_cleaned.csv')

# Get only race/ethnicity data from demographics_df
city_race_percent_df = demographics_df[["Neighborhood", "ASIAN.POPULATION", "BLACK.POPULATION","LATINO.POPULATION", "WHITE.POPULATION", "NON.WHITE.POPULATION","POPULATION.TOTAL"]]
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
city_covid_df = city_covid_df.rename(columns={"place_ID":"CITY", "population":"POPULATION", "cumulative_cases_today":"POSITIVES"})
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
county_race_covid_df = pd.read_csv(r'Race_Ethnicity(excluding_LB_Pas).csv')

city_covid_df = city_covid_df.rename(columns={"Race_Ethnicity(excluding_LB_Pas)":"Race/Ethnicity"})

# Clean up county_race_covid_df
county_race_covid_df.drop(index=0, inplace=True)
county_race_covid_df.drop(index=4, inplace=True)
county_race_covid_df.drop(index=6, inplace=True)
county_race_covid_df.drop(index=7, inplace=True)

non_white = 0
for i, row in county_race_covid_df.iterrows():
	upper = row[0].upper()
	if upper == 'HISPANIC/LATINO':
		upper = 'LATINO'
	county_race_covid_df.at[i, 'Race/Ethnicity'] = upper
	if upper != 'WHITE':
		non_white += county_race_covid_df.at[i, 'Cases']

non_white_row = {'Race/Ethnicity':'NON.WHITE','Cases' : non_white}
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

non_white_row = {'Race/Ethnicity':'NON.WHITE','Population' : non_white}
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
    # calculate raw interpolation
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
    
    # scale number of cases by total cases in city and divide by race population to get percent
    total_cases_in_city = city_row[6]
    for race, j in column_codes:
        # scale number of positives by number of total positive cases per city
        scaled_num_positive = int((raw_race[race] / total_interpolated) * total_cases_in_city)
        
        # get population of given race in the city
        pop_city_race = city_row[j]
        # divide scaled number by population of race in city to get final percentage
        if pop_city_race != 0:
            city_info[race] = scaled_num_positive / pop_city_race
        else:
            city_info[race] = 0.0
    
    city_info['TOTAL.CASES'] = total_cases_in_city
    city_info['TOTAL.POP'] = city_row[7]
    
    # append city_info to city_positive_race_df
    city_positive_race_df = city_positive_race_df.append(city_info, ignore_index=True)
##########
# Save output to csv
##########
city_positive_race_df.to_csv('race-ethnicity_interpolated.csv')
print("ace-ethnicity_interpolated.csv created successfully")