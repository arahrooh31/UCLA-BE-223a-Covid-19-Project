#scraper for http://publichealth.lacounty.gov/media/Coronavirus/locations.htm

import requests
import re
import json
import os.path
import pandas
import datetime
from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options  
from webdriver_manager.chrome import ChromeDriverManager
from io import StringIO

#parameters
dir = 'locations_demographics'
date_dir = os.path.join(dir, str(datetime.datetime.now().date()))
nan_value = float("NaN")

#make directory if it does not exist
if not os.path.isdir(dir):
    os.mkdir(dir)

if not os.path.isdir(date_dir):
	os.mkdir(date_dir)

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
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[1]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[1]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	cases = [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True) #convert empty string to NaN to drop rows with empty strings
	output.dropna().to_csv(output_file, index=None)
	
	#Laboratory Confirmed Cases
	
	tablename = 'Laboratory_Confirmed_Cases'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[2]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[2]/tr/td[2]')
	total = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/thead[2]/tr/td[2]')[0].text
	feature = ['Total'] + [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]   #strip non-alphanumeric from start
	cases = [total] + [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Deaths
	
	tablename = 'Deaths'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[3]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[3]/tr/td[2]')
	total = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/thead[3]/tr/td[2]')[0].text
	feature = ['Total'] + [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [total] + [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Age Group
	
	tablename = 'Age_Group(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[4]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[4]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]  
	cases = [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Gender
	
	tablename = 'Gender(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[5]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[5]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	
	#Race/Ethnicity
	
	tablename = 'Race_Ethnicity(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[6]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[6]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	
	#Hospitalization
	
	tablename = 'Hospitalization(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[7]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[7]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)
	
	#Deaths by race/ethnicity
	
	tablename = 'Deaths_Race_Ethnicity(excluding_LB_Pas)'
	colname = 'Cases'
	print(tablename)
	
	output_file = os.path.join(date_dir, tablename + '.csv')

	feature_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[8]/tr/td[1]')
	cases_elements = driver.find_elements_by_xpath('//*[@id="content"]/div[1]/div[3]/div/div/table/tbody[8]/tr/td[2]')
	feature = [element.text for element in feature_elements]
	feature = [re.sub('^[^a-zA-Z0-9]+', '', x) for x in feature]
	cases = [element.text for element in cases_elements]
	
	output = pandas.DataFrame(
	[feature, cases]
	).T
	output.columns = [tablename, colname]
	output.replace('', nan_value, inplace=True)
	output.dropna().to_csv(output_file, index=None)

	print('Done!')
driver.close()
