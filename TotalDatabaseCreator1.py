# -*- coding: utf-8 -*-
"""
Created on Sun Nov  8 22:12:22 2020

@author: admin
"""
import sqlite3
import pandas as pd


conn = sqlite3.connect('Covid_LA_Total_Database.db')
c = conn.cursor()

df = pd.read_csv('neighborhood_data_latimes.csv')
df.to_sql('neighborhoods', conn, if_exists="append", index = False)
c.execute('''Select * From neighborhoods''').fetchall()


def create_table():
    c.execute("""CREATE TABLE IF NOT EXISTS CDCRStateTotal(
        Date TEXT, 
        ConfirmedCases INT, 
        ActiveCases INT, 
        NewConfirmedCases INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS AgeData(
        Date Text, 
        Age INT, 
        ConfirmedCaseTotal INT, 
        ConfirmedCasePercent REAL, 
        TotalDeaths INT, 
        DeathsPercent REAL)""")
    c.execute("""CREATE TABLE IF NOT EXISTS PositiveTestRate(
        Date Text, 
        ConfirmedCases INT,
        TotalTests INT, 
        NewConfirmedCases INT,
        NewTests INT,
        NewConfirmedCasesSevenDayTotal INT,
        NewTestsSevenDayTotal INT, 
        PositiveTestRateSevenDayPercent REAL)""")
    c.execute("""CREATE TABLE IF NOT EXISTS Ethnicity(
        Date TEXT, 
        Race TEXT, 
        Age INT, 
        ConfirmedCasesTotal INT, 
        ConfirmedCasesPercent REAL, 
        DeathsTotal INT, 
        DeathsPercent REAL, 
        PopulationPercent REAL)""")
    c.execute("""CREATE TABLE IF NOT EXISTS ReopeningMetrics(
        Date TEXT, 
        County TEXT, 
        Fips INT, 
        PerCapitaCaseRate REAL, 
        AdjustedCaseRate REAL, 
        PositivityRate REAL, 
        EquityIndex REAL)""")
    c.execute("""CREATE TABLE IF NOT EXISTS ReopeningTiers(
        Date TEXT,
        County TEXT,
        Fips INT,
        Tier INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS AgencyTotals(
        Agency TEXT,
        County TEXT,
        Fips INT,
        Date TEXT,
        ConfirmedCases INT,
        Deaths INT,
        Recoveries INT,
        DidNotUpdate INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS CountyTotals(
        Date TEXT,
        County TEXT,
        Fips INT,
        ConfirmedCases INT,
        Deaths INT,
        NewConfirmedCases INT,
        NewDeaths INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS LATimesStateTotal(
        Date TEXT,
        ConfirmedCases INT,
        Deaths INT,
        NewConfirmedCases INT,
        NewDeaths INT)""")    
    c.execute("""CREATE TABLE IF NOT EXISTS CDPHStateTotal(
        Date TEXT,
        ConfirmedCases INT,
        Deaths INT,
        Travel INT,
        PersontoPerson INT,
        CommunitySpread INT,
        UnderInvestigation INT,
        OtherCauses INT,
        SelfMonitoring INT,
        Age0to17 INT,
        Age18to49 INT,
        Age50to64 INT,
        Age65andUp INT,
        Age18to64 INT,
        AgeUnknown INT,
        GenderMale INT,
        GenderFemale INT,
        GenderUnknown INT,
        TotalTests INT,
        RecievedTests INT,
        PendingTests INT,
        ConfirmedHospitlization INT,
        ConfirmedICU INT,
        SuspectedHospitlization INT,
        SuspectedICU INT,
        HealthcareWorkerInfections INT,
        HealthcareWorkerDeaths INT,
        Source TEXT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS HospitalPatientCountyTotals(
        Date TEXT,
        County TEXT,
        Fips INT,
        PositivePatients INT,
        SuspectedPatients INT,
        ICUPositivePatients INT,
        ICUSuspectedPatients INT,
        ICUAvailableBeds INT)""") 
    c.execute("""CREATE TABLE IF NOT EXISTS CommunityTesting(
        City TEXT, 
        PersonsTested INT, 
        TestingRate INT, 
        AdjustedTestingRate INT,  
        PersonsTestedPositive INT, 
        PositiveTestingRate INT, 
        AdjustedPositveTestingRate INT, 
        Population INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS DeathsByDate(
        Date Text, 
        TotalCases INT, 
        DailyCases INT, 
        AvgCases INT, 
        TotalDeaths INT, 
        NewDeaths INT, 
        AvgDeaths INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS DeathsByCommunity(
        City Text, 
        Cases INT, 
        CaseRate INT,
        AdjustedCaseRate INT, 
        Deaths INT, 
        DeathRate INT, 
        AdjustedDeathRate INT, 
        Population INT)""")
    c.execute("""CREATE TABLE IF NOT EXISTS PersonsTested(
        Date TEXT, 
        CummulativePersonsTested INT, 
        CummulativePositivePersons REAL, 
        CummulativePercentPositive REAL, 
        PersonsTested INT, 
        PositvePersonsAll INT, 
        PercentPositivePersons REAL, 
        AvgPersonsTested INT, 
        AvgPositivePersons INT, 
        CummulativeAvgPercentPositivePersons REAL)""")
    c.execute("""CREATE TABLE IF NOT EXISTS DailyTesting(
        Date TEXT, 
        CummulativeTests INT, 
        CummulativePositiveTests INT, 
        CummulativePercentPositiveTests REAL, 
        Tests INT, 
        PositveTests INT, 
        PercentPositiveTests REAl, 
        AvgTests INT, 
        AvgPositiveTests INT, 
        AvgPercentPositiveTests REAL)""")
    c.execute("""CREATE TABLE IF NOT EXISTS Demographics(
        Characteristic TEXT,
        Total INT,
        TotalPercent REAL,
        PercentTotalNonMissing REAL,
        PopulationPercent REAL,
        CrudeRate INT,
        AdjustedCaseRate INT,
        RateUnstable INT,
        Population INT)""")  

conn.commit()

def data_entry():
    with open('cdcr-state-totals.csv', 'r') as file1:
        no_records1 = 0
        for row in file1:
            c.execute("INSERT INTO CDCRStateTotal VALUES(?,?,?,?)", row.split(","))
            conn.commit()
            no_records1 += 1
    
    with open('cdph-age.csv', 'r') as file2:
        no_records2 = 0
        for row in file2:
             c.execute("INSERT INTO AgeData VALUES(?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records2 += 1
             
    with open('cdph-positive-test-rate.csv', 'r') as file3:
        no_records3 = 0
        for row in file3:
             c.execute("INSERT INTO PositiveTestRate VALUES(?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records3 += 1
    
    with open('cdph-race-ethnicity.csv', 'r') as file4:
        no_records4 = 0
        for row in file4:
             c.execute("INSERT INTO Ethnicity VALUES(?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records4 += 1    
             
    with open('cdph-reopening-metrics.csv', 'r') as file5:
        no_records5 = 0
        for row in file5:
             c.execute("INSERT INTO ReopeningMetrics VALUES(?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records5 += 1  
    
    with open('cdph-reopening-tiers.csv', 'r') as file6:
        no_records6 = 0
        for row in file6:
             c.execute("INSERT INTO ReopeningTiers VALUES(?,?,?,?)", row.split(","))
             conn.commit()
             no_records6 += 1   

    with open('latimes-agency-totals.csv', 'r') as file7:
        no_records7 = 0
        for row in file7:
             c.execute("INSERT INTO AgencyTotals VALUES(?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records7 += 1   
    
    with open('latimes-county-totals.csv', 'r') as file8:
        no_records8 = 0
        for row in file8:
             c.execute("INSERT INTO CountyTotals VALUES(?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records8 += 1   
    
    with open('latimes-state-totals.csv', 'r') as file9:
        no_records9 = 0
        for row in file9:
             c.execute("INSERT INTO LATimesStateTotal VALUES(?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records9 += 1   
 
    with open('cdph-state-totals.csv', 'r') as file10:
        no_records10 = 0
        for row in file10:
             c.execute("INSERT INTO CDPHStateTotal VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records10 += 1   

    with open('cdph-hospital-patient-county-totals.csv', 'r') as file11:
        no_records11 = 0
        for row in file11:
             c.execute("INSERT INTO HospitalPatientCountyTotals VALUES(?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records11 += 1   
    
    with open('communitytesting.csv', 'r') as file12:
        no_records12 = 0
        for row in file12:
            c.execute("INSERT INTO CommunityTesting VALUES(?,?,?,?,?,?,?,?)", row.split(","))
            conn.commit()
            no_records12 += 1
    
    with open('deathsbydate.csv', 'r') as file13:
        no_records13 = 0
        for row in file13:
             c.execute("INSERT INTO DeathsbyDate VALUES(?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records13 += 1
             
    with open('deathsbycommunity.csv', 'r') as file14:
        no_records14 = 0
        for row in file14:
             c.execute("INSERT INTO DeathsbyCommunity VALUES(?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records14 += 1    
    
    with open('personstested.csv', 'r') as file15:
        no_records15 = 0
        for row in file15:
             c.execute("INSERT INTO PersonsTested VALUES(?,?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records15 += 1    
             
    with open('dailytesting.csv', 'r') as file16:
        no_records16 = 0
        for row in file16:
             c.execute("INSERT INTO DailyTesting VALUES(?,?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records16 += 1  
    
    with open('demographics.csv', 'r') as file17:
        no_records17 = 0
        for row in file17:
             c.execute("INSERT INTO Demographics VALUES(?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records17 += 1   
    
    with open('neighborhood_data_latimes.csv', 'r') as file18:
        no_records18 = 0
        for row in file18:
            c.execute("INSERT INTO Neighborhoods VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)", row.split(','))
            conn.commit()
            no_records18 += 1      
            
    c.execute('DELETE FROM CDCRStateTotal WHERE rowid = 1')
    c.execute('DELETE FROM AgeData WHERE rowid = 1')
    c.execute('DELETE FROM PositiveTestRate WHERE rowid = 1')
    c.execute('DELETE FROM Ethnicity WHERE rowid = 1')
    c.execute('DELETE FROM ReopeningMetrics WHERE rowid = 1')
    c.execute('DELETE FROM ReopeningTiers WHERE rowid = 1')
    c.execute('DELETE FROM AgencyTotals WHERE rowid = 1')
    c.execute('DELETE FROM CountyTotals WHERE rowid = 1')
    c.execute('DELETE FROM LATimesStateTotal WHERE rowid = 1')
    c.execute('DELETE FROM CDPHStateTotal WHERE rowid = 1')
    c.execute('DELETE FROM HospitalPatientCountyTotals WHERE rowid = 1')

    
    conn.commit()
    c.close()
    conn.close()

create_table()
data_entry()    

