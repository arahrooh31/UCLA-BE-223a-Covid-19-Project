# -*- coding: utf-8 -*-
"""
Created on Mon Nov  9 00:29:54 2020

@author: admin
"""


import sqlite3
import pandas as pd


conn = sqlite3.connect('Covid_LA_Total_Database_Updated1.db')
c = conn.cursor()

df = pd.read_csv('neighborhood_data_latimes.csv')
df.to_sql('Neighborhoods', conn, if_exists = "append", index = False)
c.execute('''Select * From Neighborhoods''').fetchall()

df1 = pd.read_csv('latimes-place-totals.csv')
df1.to_sql('PlaceTotals',conn, if_exists = 'append', index = False)
c.execute('''Select * From PlaceTotals''').fetchall()

df2 = pd.read_csv('cdph-age.csv')
df2.to_sql('Age',conn, if_exists = 'append', index = False)
c.execute('''Select * From Age''').fetchall()

df3 = pd.read_csv('cdcr-state-totals.csv')
df3.to_sql('CDCRStateTotals',conn, if_exists = 'append', index = False)
c.execute('''Select * From CDCRStateTotals''').fetchall()

df4 = pd.read_csv('cdph-positive-test-rate.csv')
df4.to_sql('PositiveTestRate',conn, if_exists = 'append', index = False)
c.execute('''Select * From PositiveTestRate''').fetchall()

df5 = pd.read_csv('cdph-race-ethnicity.csv')
df5.to_sql('Ethnicity',conn, if_exists = 'append', index = False)
c.execute('''Select * From Ethnicity''').fetchall()

df6 = pd.read_csv('cdph-reopening-metrics.csv')
df6.to_sql('ReopeningMetrics',conn, if_exists = 'append', index = False)
c.execute('''Select * From ReopeningMetrics''').fetchall()

df7 = pd.read_csv('cdph-reopening-tiers.csv')
df7.to_sql('ReopeningTiers',conn, if_exists = 'append', index = False)
c.execute('''Select * From ReopeningTiers''').fetchall()

df8 = pd.read_csv('latimes-agency-totals.csv')
df8.to_sql('AgencyTotals',conn, if_exists = 'append', index = False)
c.execute('''Select * From AgencyTotals''').fetchall()

df9 = pd.read_csv('latimes-state-totals.csv')
df9.to_sql('LATimesStateTotal',conn, if_exists = 'append', index = False)
c.execute('''Select * From LATimesStateTotal''').fetchall()

df10 = pd.read_csv('latimes-county-totals.csv')
df10.to_sql('LATimesCountyTotal',conn, if_exists = 'append', index = False)
c.execute('''Select * From LATimesCountyTotal''').fetchall()

df11 = pd.read_csv('cdph-state-totals.csv')
df11.to_sql('CDPHStateTotals',conn, if_exists = 'append', index = False)
c.execute('''Select * From CDPHStateTotals''').fetchall()

df12 = pd.read_csv('cdph-hospital-patient-county-totals.csv')
df12.to_sql('HospitalPatientCountyTotals',conn, if_exists = 'append', index = False)
c.execute('''Select * From HospitalPatientCountyTotals''').fetchall()

df13 = pd.read_csv('communitytesting.csv')
df13.to_sql('CommunityTesting',conn, if_exists = 'append', index = False)
c.execute('''Select * From CommunityTesting''').fetchall()

df14 = pd.read_csv('deathsbydate.csv')
df14.to_sql('DeathsbyDate',conn, if_exists = 'append', index = False)
c.execute('''Select * From DeathsbyDate''').fetchall()

df15 = pd.read_csv('deathsbycommunity.csv')
df15.to_sql('DeathsbyCommunity',conn, if_exists = 'append', index = False)
c.execute('''Select * From DeathsbyCommunity''').fetchall()

df16 = pd.read_csv('personstested.csv')
df16.to_sql('PersonsTested',conn, if_exists = 'append', index = False)
c.execute('''Select * From PersonsTested''').fetchall()

df17 = pd.read_csv('dailytesting.csv')
df17.to_sql('DailyTesting',conn, if_exists = 'append', index = False)
c.execute('''Select * From DailyTesting''').fetchall()

df18 = pd.read_csv('demographics.csv')
df18.to_sql('Demographics',conn, if_exists = 'append', index = False)
c.execute('''Select * From Demographics''').fetchall()

conn.commit()
c.close()
conn.close()
























