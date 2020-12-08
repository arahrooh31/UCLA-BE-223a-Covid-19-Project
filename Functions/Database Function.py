# -*- coding: utf-8 -*-
"""
Created on Sat Dec  5 19:19:09 2020

@author: Al Rahrooh
"""



def create_database():
        
    import sqlite3
    import pandas as pd
    
    
    conn = sqlite3.connect('Covid_LA_Database_Updated.db')
    c = conn.cursor()
    
    df = pd.read_csv('data/neighborhood_data_latimes.csv')
    df.to_sql('Neighborhoods', conn, if_exists = "append", index = False)
    c.execute('''Select * From Neighborhoods''').fetchall()
    
    df1 = pd.read_csv('data/latimes-place-totals.csv')
    df1.to_sql('PlaceTotals',conn, if_exists = 'append', index = False)
    c.execute('''Select * From PlaceTotals''').fetchall()
    
    df2 = pd.read_csv('data/cdph-age.csv')
    df2.to_sql('Age',conn, if_exists = 'append', index = False)
    c.execute('''Select * From Age''').fetchall()
    
    df3 = pd.read_csv('data/cdcr-state-totals.csv')
    df3.to_sql('CDCRStateTotals',conn, if_exists = 'append', index = False)
    c.execute('''Select * From CDCRStateTotals''').fetchall()
    
    df4 = pd.read_csv('data/cdph-positive-test-rate.csv')
    df4.to_sql('PositiveTestRate',conn, if_exists = 'append', index = False)
    c.execute('''Select * From PositiveTestRate''').fetchall()
    
    df5 = pd.read_csv('data/cdph-race-ethnicity.csv')
    df5.to_sql('Ethnicity',conn, if_exists = 'append', index = False)
    c.execute('''Select * From Ethnicity''').fetchall()
    
    df6 = pd.read_csv('data/cdph-reopening-metrics.csv')
    df6.to_sql('ReopeningMetrics',conn, if_exists = 'append', index = False)
    c.execute('''Select * From ReopeningMetrics''').fetchall()
    
    df7 = pd.read_csv('data/cdph-reopening-tiers.csv')
    df7.to_sql('ReopeningTiers',conn, if_exists = 'append', index = False)
    c.execute('''Select * From ReopeningTiers''').fetchall()
    
    df8 = pd.read_csv('data/latimes-agency-totals.csv')
    df8.to_sql('AgencyTotals',conn, if_exists = 'append', index = False)
    c.execute('''Select * From AgencyTotals''').fetchall()
    
    df9 = pd.read_csv('data/latimes-state-totals.csv')
    df9.to_sql('LATimesStateTotal',conn, if_exists = 'append', index = False)
    c.execute('''Select * From LATimesStateTotal''').fetchall()
    
    df10 = pd.read_csv('data/latimes-county-totals.csv')
    df10.to_sql('LATimesCountyTotal',conn, if_exists = 'append', index = False)
    c.execute('''Select * From LATimesCountyTotal''').fetchall()
    
    df11 = pd.read_csv('data/cdph-state-totals.csv')
    df11.to_sql('CDPHStateTotals',conn, if_exists = 'append', index = False)
    c.execute('''Select * From CDPHStateTotals''').fetchall()
    
    df12 = pd.read_csv('data/cdph-hospital-patient-county-totals.csv')
    df12.to_sql('HospitalPatientCountyTotals',conn, if_exists = 'append', index = False)
    c.execute('''Select * From HospitalPatientCountyTotals''').fetchall()
    
    df13 = pd.read_csv('data/communitytesting.csv')
    df13.to_sql('CommunityTesting',conn, if_exists = 'append', index = False)
    c.execute('''Select * From CommunityTesting''').fetchall()
    
    df14 = pd.read_csv('data/deathsbydate.csv')
    df14.to_sql('DeathsbyDate',conn, if_exists = 'append', index = False)
    c.execute('''Select * From DeathsbyDate''').fetchall()
    
    df15 = pd.read_csv('data/deathsbycommunity.csv')
    df15.to_sql('DeathsbyCommunity',conn, if_exists = 'append', index = False)
    c.execute('''Select * From DeathsbyCommunity''').fetchall()
    
    df16 = pd.read_csv('data/personstested.csv')
    df16.to_sql('PersonsTested',conn, if_exists = 'append', index = False)
    c.execute('''Select * From PersonsTested''').fetchall()
    
    df17 = pd.read_csv('data/dailytesting.csv')
    df17.to_sql('DailyTesting',conn, if_exists = 'append', index = False)
    c.execute('''Select * From DailyTesting''').fetchall()
    
    df18 = pd.read_csv('data/demographics.csv')
    df18.to_sql('Demographics',conn, if_exists = 'append', index = False)
    c.execute('''Select * From Demographics''').fetchall()
    
    df19 = pd.read_csv('data/Age_Group(excluding_LB_Pas).csv')
    df19.to_sql('AgeGroup', conn, if_exists = "append", index = False)
    c.execute('''Select * From AgeGroup''').fetchall()
    
    df20 = pd.read_csv('data/Deaths.csv')
    df20.to_sql('TotalAge',conn, if_exists = 'append', index = False)
    c.execute('''Select * From TotalAge''').fetchall()
    
    df21 = pd.read_csv('data/Deaths_Race_Ethnicity(excluding_LB_Pas).csv')
    df21.to_sql('DeathsRaceEthnicity',conn, if_exists = 'append', index = False)
    c.execute('''Select * From DeathsRaceEthnicity''').fetchall()
    
    df22 = pd.read_csv('data/Gender(excluding_LB_Pas).csv')
    df22.to_sql('Gender',conn, if_exists = 'append', index = False)
    c.execute('''Select * From Gender''').fetchall()
    
    df23 = pd.read_csv('data/Hospitalization(excluding_LB_Pas).csv')
    df23.to_sql('Hospitilization',conn, if_exists = 'append', index = False)
    c.execute('''Select * From Hospitilization''').fetchall()
    
    df24 = pd.read_csv('data/Laboratory_Confirmed_Cases.csv')
    df24.to_sql('LaboratoryConfirmedCases',conn, if_exists = 'append', index = False)
    c.execute('''Select * From LaboratoryConfirmedCases''').fetchall()
    
    df25 = pd.read_csv('data/New_Daily_Counts.csv')
    df25.to_sql('NewDailyCounts',conn, if_exists = 'append', index = False)
    c.execute('''Select * From NewDailyCounts''').fetchall()
    
    df26 = pd.read_csv('data/Race_Ethnicity(excluding_LB_Pas).csv')
    df26.to_sql('RaceEthnicity',conn, if_exists = 'append', index = False)
    c.execute('''Select * From RaceEthnicity''').fetchall()
    
    df27 = pd.read_csv('data/community_demographic_cleaned.csv')
    df27.to_sql('CommunityDemographicCleaned',conn, if_exists = 'append', index = False)
    c.execute('''Select * From CommunityDemographicCleaned''').fetchall()
    
    df28 = pd.read_csv('data/community_todayUpdated.csv')
    df28.to_sql('CommunityTodayUpdated',conn, if_exists = 'append', index = False)
    c.execute('''Select * From CommunityTodayUpdated''').fetchall()
    
    df29 = pd.read_csv('data/AgeInterpolationFinalDF.csv')
    df29.to_sql('AgeInterpolationFinalDF',conn, if_exists = 'append', index = False)
    c.execute('''Select * From AgeInterpolationFinalDF''').fetchall()
    
    df30 = pd.read_csv('data/race-ethnicity_interpolated.csv')
    df30.to_sql('RaceEthnicityInterpolated',conn, if_exists = 'append', index = False)
    c.execute('''Select * From RaceEthnicityInterpolated''').fetchall()
    
    df31 = pd.read_csv('data/community_TimeCases_long.csv')
    df31.to_sql('CommunityTimeCasesLong',conn, if_exists = 'append', index = False)
    c.execute('''Select * From CommunityTimeCasesLong''').fetchall()
    
    df32 = pd.read_csv('data/LA_county.csv')
    df32.to_sql('LACountyCleaned',conn, if_exists = 'append', index = False)
    c.execute('''Select * From LACountyDeathsCleaned''').fetchall()
    
    
    
    conn.commit()
    c.close()
    conn.close()
