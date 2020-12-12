# -*- coding: utf-8 -*-
"""
Created on Thu Dec 10 15:21:43 2020

@author: admin
"""

def age_interpolation():
    
    import pandas as pd
    import numpy as np 
    
        
        ###AGE###
        
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
    tempLA_COVIDDF['Age'][9] = "Sum"
    
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
