

import sqlite3
import pandas as pd
import requests
import re
import json
import os.path
from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options  
from webdriver_manager.chrome import ChromeDriverManager
from io import StringIO

#directory parameter
dir = 'data'

###CA_DATADESK_GITHUB_SCRAPER###

if not os.path.isdir(dir):
    os.mkdir(dir)

r = requests.get('https://api.github.com/repos/datadesk/california-coronavirus-data/contents/')
csvs = re.findall(r'https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/^\s*(?:\S\s*){10,100}$.csv', r.text)

contents = json.loads(r.text)

for i in contents:
	url_str = str(i['download_url'])
	if url_str.endswith('.csv'):
		file = os.path.join(dir, url_str.split('/')[-1])
		r = requests.get(url_str)
		with open(file, 'wb') as f:
			f.write(r.content)
		print(i['download_url'])


###LA_COUNTY_DASHBOARD_SCRAPER###

if not os.path.isdir(dir):
    os.mkdir(dir)

url = 'http://dashboard.publichealth.lacounty.gov/covid19_surveillance_dashboard/'

option = webdriver.ChromeOptions()
option.add_argument('headless') #make chrome run in the background
option.add_argument('log-level=3') #suppress logging
driver = webdriver.Chrome(ChromeDriverManager().install(), options = option)

print('Loading webpage....') 		

driver.get(url)
try:		
	#wait for shinyapp to appear
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.ID, 'shinyapps'))
    )
finally:
	#then switch to shinyapp frame
	frame = driver.find_elements_by_id('shinyapps') 
	driver.switch_to.frame(frame[0])
	
try:
	#wait for last element that loads on the page to finish loading
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="myplot"]/div/div'))
    )
finally:
	print('page loaded')
	print('retrieving links...')

#click tab
driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[3]/a')[0].click() #demographics

try:
	#wait for table to load
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_0"]/tbody'))
    )
finally:
	#then grab the link from the button element
	demographics_link = driver.find_elements_by_xpath('//*[@id="download1"]')[0].get_attribute('href')

#click on the next tab and repeat!
driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[5]/a')[0].click() #community case/death

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_1_wrapper"]'))
    )
finally:
	community_death_link = driver.find_elements_by_xpath('//*[@id="download2"]')[0].get_attribute('href')

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[6]/a')[0].click() #community testing

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_2_wrapper"]'))
    )
finally:
	community_testing_link = driver.find_elements_by_xpath('//*[@id="download4"]')[0].get_attribute('href')
	

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[7]/a')[0].click() #cases/death by date

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_3_wrapper"]'))
    )
finally:
	date_death_link = driver.find_elements_by_xpath('//*[@id="download3"]')[0].get_attribute('href')

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[8]/a')[0].click() #persons tested by date

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_4_wrapper"]'))
    )
finally:
	persons_date_link = driver.find_elements_by_xpath('//*[@id="download5"]')[0].get_attribute('href')
	

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[9]/a')[0].click() #tests by date

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_5_wrapper"]'))
    )
finally:
	test_date_link = driver.find_elements_by_xpath('//*[@id="download6"]')[0].get_attribute('href')
	
driver.quit()

print('links retrieved... downloading:')

#download and save files
	
demographics = requests.get(demographics_link)
file = os.path.join(dir, 'demographics.csv')
df = pd.read_csv(StringIO((demographics.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

community_death = requests.get(community_death_link)
file = os.path.join(dir, 'deathsbycommunity.csv')
df = pd.read_csv(StringIO((community_death.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)
	
community_testing = requests.get(community_testing_link)
file = os.path.join(dir, 'communitytesting.csv')
df = pd.read_csv(StringIO((community_testing.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

date_death = requests.get(date_death_link)
file = os.path.join(dir, 'deathsbydate.csv')
df = pd.read_csv(StringIO((date_death.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

persons_date = requests.get(persons_date_link)
file = os.path.join(dir, 'personstested.csv')
df = pd.read_csv(StringIO((persons_date.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

tests_date = requests.get(test_date_link)
file = os.path.join(dir, 'dailytesting.csv')
df = pd.read_csv(StringIO((tests_date.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)


 ###LA_COUNTY_NEIGHBORHOODS_SCRAPER###


#make dir and set filename
if not os.path.isdir(dir):
    os.mkdir(dir)
output_file = os.path.join(dir, 'neighborhood_data_latimes.csv')

#base url
url = 'http://maps.latimes.com/neighborhoods/'

#hardcoding endpoints
hrefs = [
'age/median/neighborhood/list/',
'age/65-and-up/neighborhood/list/',
'age/50-64/neighborhood/list/',
'age/35-49/neighborhood/list/',
'age/19-34/neighborhood/list/',
'age/11-18/neighborhood/list/',
'age/10-or-less/neighborhood/list/',
'foreign-born/neighborhood/list/',
'area/square-miles/neighborhood/list/',
'education/four-year-degree/neighborhood/list/',
'education/less-than-high-school/neighborhood/list/',
'education/high-school/neighborhood/list/',
'education/some-college/neighborhood/list/',
'education/bachelors/neighborhood/list/',
'education/masters-or-higher/neighborhood/list/',
'diversity/neighborhood/list/',
'ethnicity/asian/neighborhood/list/',
'ethnicity/black/neighborhood/list/',
'ethnicity/latino/neighborhood/list/',
'ethnicity/white/neighborhood/list/',
'ethnicity/non-white/neighborhood/list/',
'single-parents/neighborhood/list/',
'household-size/neighborhood/list/',
'owners/neighborhood/list/',
'renters/neighborhood/list/',
'income/median/neighborhood/list/',
'income/20-or-less/neighborhood/list/',
'income/20-to-40/neighborhood/list/',
'income/40-to-60/neighborhood/list/',
'income/60-to-125/neighborhood/list/',
'income/125-and-up/neighborhood/list/',
'marital-status/never-married-males/neighborhood/list/',
'marital-status/never-married-females/neighborhood/list/',
'marital-status/married-males/neighborhood/list/',
'marital-status/married-females/neighborhood/list/',
'marital-status/widowed-males/neighborhood/list/',
'marital-status/widowed-females/neighborhood/list/',
'marital-status/divorced-males/neighborhood/list/',
'marital-status/divorced-females/neighborhood/list/',
'veterans/neighborhood/list/',
'population/total/neighborhood/list/',
'population/density/neighborhood/list/'
]

option = webdriver.ChromeOptions()
option.add_argument('headless') #make chrome run in the background
option.add_argument('log-level=3') #suppress logging
driver = webdriver.Chrome(ChromeDriverManager().install(), options = option)

print('crawling webpages....') 		
print("NOTE: I would recommend against using chrome while this is running so the selenium driver doesn't hang")
#I am doing the first iteration out of the main loop because I couldn't pandas.merge 
#to work with an initialized empty dataframe. If someone is good with pandas and has a
#better solution please let me know
driver.get(url + hrefs[0])
try:		
	#wait for page to load
	element = WebDriverWait(driver, 10).until(
		EC.presence_of_element_located((By.ID, 'map-canvas'))
	)
finally:
	print(hrefs[0].split('/neighborhood',1)[0])
	#grab the name of the feature
	colname = driver.find_elements_by_xpath('//*[@id="sortable_table"]/thead/tr/th[3]')[0].text
	#grab the elements that contain neighborhood names
	neighborhood_elements = driver.find_elements_by_xpath('//*[@id="sortable_table"]/tbody/tr/td[2]/a')
	#grab the elements that contain the feature values
	feature_elements = driver.find_elements_by_xpath('//*[@id="sortable_table"]/tbody/tr/td[3]')
	#grab the text out of the HTML elements
	neighborhood = [element.text for element in neighborhood_elements] 
	feature = [element.text for element in feature_elements]
	
	#put them into a dataframe, and set the column names
	output = pd.DataFrame(
	[neighborhood, feature]
	).T
	output.columns = ['Neighborhood', colname]

for href in hrefs[1:(len(hrefs)+1)]:
	driver.get(url + href)
	try:		
		element = WebDriverWait(driver, 10).until(
			EC.presence_of_element_located((By.ID, 'map-canvas'))
		)
	finally:
		print(href.split('/neighborhood',1)[0])
		#same as above with exception for income categories
		if href.startswith('income'):
			colname = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[2]/h1')[0].text
		else:
			colname = driver.find_elements_by_xpath('//*[@id="sortable_table"]/thead/tr/th[3]')[0].text
		neighborhood_elements = driver.find_elements_by_xpath('//*[@id="sortable_table"]/tbody/tr/td[2]/a')
		feature_elements = driver.find_elements_by_xpath('//*[@id="sortable_table"]/tbody/tr/td[3]')
		neighborhood = [element.text for element in neighborhood_elements] 
		feature = [element.text for element in feature_elements]
		df = pd.DataFrame(
		[neighborhood, feature]
		).T
		df.columns = ['Neighborhood', colname]
		#merge new dataframe with output dataframe
		output = pd.merge(output, df, on = 'Neighborhood')

driver.close()
print(output)
output.to_csv(output_file, index=None)



###LA_COUNTY_LOCATIONS_SCRAPER###

#parameters
nan_value = float("NaN")

#make directory if it does not exist
if not os.path.isdir(dir):
    os.mkdir(dir)

#if not os.path.isdir(date_dir):
#	os.mkdir(date_dir)

#base url
url = 'http://publichealth.lacounty.gov/media/Coronavirus/locations.htm'


option = webdriver.ChromeOptions()
option.add_argument('headless') #make chrome run in the background
option.add_argument('log-level=3') #suppress logging
driver = webdriver.Chrome(ChromeDriverManager().install(), options = option)

print('Crawling webpage....') 		
driver.get(url)
try:		
	#wait for page to load
	element = WebDriverWait(driver, 10).until(
		EC.presence_of_element_located((By.XPATH, '//*[@id="citations"]/div/div/h2'))
	)
finally:
	#New Daily Counts
	tablename = 'New_Daily_Counts'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[1]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[1]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	cases = [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True) #convert empty string to NaN to drop rows with empty strings
	output.dropna().to_csv(output_file, index=None)
	
	#Laboratory Confirmed Cases
	
	tablename = 'Laboratory_Confirmed_Cases'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[2]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[2]/tr/td[2]')
	total = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/thead[2]/tr/td[2]')[0].text
	feature = ['Total'] + [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]   #strip non-alphanumeric from start
	cases = [total] + [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Deaths
	
	tablename = 'Deaths'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[3]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[3]/tr/td[2]')
	total = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/thead[3]/tr/td[2]')[0].text
	feature = ['Total'] + [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [total] + [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Age Group
	
	tablename = 'Age_Group(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[4]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[4]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]  
	cases = [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Gender
	
	tablename = 'Gender(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[5]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[5]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	
	#Race/Ethnicity
	
	tablename = 'Race_Ethnicity(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[6]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[6]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	
	#Hospitalization
	
	tablename = 'Hospitalization(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[7]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[7]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Deaths by race/ethnicity
	
	tablename = 'Deaths_Race_Ethnicity(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[8]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[8]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pd.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)

	print('Done!')
driver.close()






#CREATING DATABASE

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

conn.commit()
c.close()
conn.close()

















