
import sqlite3


conn = sqlite3.connect('BE223_Covid_Database_Updated.db')
c = conn.cursor()


def create_table():
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
    with open('communitytesting.csv', 'r') as file1:
        no_records1 = 0
        for row in file1:
            c.execute("INSERT INTO CommunityTesting VALUES(?,?,?,?,?,?,?,?)", row.split(","))
            conn.commit()
            no_records1 += 1
    
    with open('deathsbydate.csv', 'r') as file2:
        no_records2 = 0
        for row in file2:
             c.execute("INSERT INTO DeathsbyDate VALUES(?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records2 += 1
             
    with open('deathsbycommunity.csv', 'r') as file3:
        no_records3 = 0
        for row in file3:
             c.execute("INSERT INTO DeathsbyCommunity VALUES(?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records3 += 1    
    
    with open('personstested.csv', 'r') as file4:
        no_records4 = 0
        for row in file4:
             c.execute("INSERT INTO PersonsTested VALUES(?,?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records4 += 1    
             
    with open('dailytesting.csv', 'r') as file5:
        no_records5 = 0
        for row in file5:
             c.execute("INSERT INTO DailyTesting VALUES(?,?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records5 += 1  
    
    with open('demographics.csv', 'r') as file6:
        no_records6 = 0
        for row in file6:
             c.execute("INSERT INTO Demographics VALUES(?,?,?,?,?,?,?,?,?)", row.split(","))
             conn.commit()
             no_records6 += 1   
             
    c.execute('DELETE FROM CommunityTesting WHERE rowid = 1')
    c.execute('DELETE FROM DeathsbyDate WHERE rowid = 1')
    c.execute('DELETE FROM DeathsbyCommunity WHERE rowid = 1')
    c.execute('DELETE FROM PersonsTested WHERE rowid = 1')
    c.execute('DELETE FROM DailyTesting WHERE rowid = 1')
    c.execute('DELETE FROM Demographics WHERE rowid = 1')
    
    conn.commit()
    c.close()
    conn.close()

    
create_table()
data_entry()


