#scraper for .csv files found at http://dashboard.publichealth.lacounty.gov/covid19_surveillance_dashboard/
#requires requests, selenium, webdriver_manager packages -> pip install <...>
#requires selenium

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
dir = 'county'

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

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[8]/a')[0].click() #community testing

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_2_wrapper"]'))
    )
finally:
	community_testing_link = driver.find_elements_by_xpath('//*[@id="download4"]')[0].get_attribute('href')
	
driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[9]/a')[0].click() #cases/death by date

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_3_wrapper"]'))
    )
finally:
	date_death_link = driver.find_elements_by_xpath('//*[@id="download3"]')[0].get_attribute('href')

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[10]/a')[0].click() #persons tested by date

try:
	element = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="DataTables_Table_4_wrapper"]'))
    )
finally:
	persons_date_link = driver.find_elements_by_xpath('//*[@id="download5"]')[0].get_attribute('href')
	

driver.find_elements_by_xpath('//*[@id="sidebarItemExpanded"]/ul/li[11]/a')[0].click() #tests by date

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
df = pandas.read_csv(StringIO((demographics.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

community_death = requests.get(community_death_link)
file = os.path.join(dir, 'deathsbycommunity.csv')
df = pandas.read_csv(StringIO((community_death.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

community_testing = requests.get(community_testing_link)
file = os.path.join(dir, 'communitytesting.csv')
df = pandas.read_csv(StringIO((community_testing.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

date_death = requests.get(date_death_link)
file = os.path.join(dir, 'deathsbydate.csv')
df = pandas.read_csv(StringIO((date_death.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

persons_date = requests.get(persons_date_link)
file = os.path.join(dir, 'personstested.csv')
df = pandas.read_csv(StringIO((persons_date.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

tests_date = requests.get(test_date_link)
file = os.path.join(dir, 'dailytesting.csv')
df = pandas.read_csv(StringIO((tests_date.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)