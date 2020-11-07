import requests
import re
import json
import os.path
import pandas
from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options  
from webdriver_manager.chrome import ChromeDriverManager
from io import StringIO

#directory parameter
dir = 'neighborhoods'

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
	output = pandas.DataFrame(
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
		df = pandas.DataFrame(
		[neighborhood, feature]
		).T
		df.columns = ['Neighborhood', colname]
		#merge new dataframe with output dataframe
		output = pandas.merge(output, df, on = 'Neighborhood')

driver.close()
print(output)
output.to_csv(output_file, index=None)
