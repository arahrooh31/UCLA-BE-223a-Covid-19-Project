# -*- coding: utf-8 -*-
"""
Created on Sat Nov  7 18:15:52 2020

@author: Al Rahrooh
"""

import sqlite3


conn = sqlite3.connect('Covid_LA_Times_Database.db')
c = conn.cursor()

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