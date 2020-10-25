#scraper for .csv files found at http://dashboard.publichealth.lacounty.gov/covid19_surveillance_dashboard/
#requires requests package -> pip install requests

import requests
import re
import json
import os.path
import pandas
from io import StringIO

#directory parameter
dir = 'county'

if not os.path.isdir(dir):
    os.mkdir(dir)

demographics = requests.get('https://lacdph.shinyapps.io/covid19_surveillance_dashboard/_w_9d504c84/session/9c3e5e708ae673c42b04153999c491ab/download/download1?w=9d504c84')
file = os.path.join(dir, 'demographics.csv')
df = pandas.read_csv(StringIO((demographics.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

community_death = requests.get('https://lacdph.shinyapps.io/covid19_surveillance_dashboard/_w_9d504c84/session/9c3e5e708ae673c42b04153999c491ab/download/download2?w=9d504c84')
file = os.path.join(dir, 'deathsbycommunity.csv')
df = pandas.read_csv(StringIO((community_death.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)
	
community_testing = requests.get('https://lacdph.shinyapps.io/covid19_surveillance_dashboard/_w_9d504c84/session/9c3e5e708ae673c42b04153999c491ab/download/download4?w=9d504c84')
file = os.path.join(dir, 'communitytesting.csv')
df = pandas.read_csv(StringIO((community_testing.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

date_death = requests.get('https://lacdph.shinyapps.io/covid19_surveillance_dashboard/_w_9d504c84/session/9c3e5e708ae673c42b04153999c491ab/download/download3?w=9d504c84')
file = os.path.join(dir, 'deathsbydate.csv')
df = pandas.read_csv(StringIO((date_death.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

persons_date = requests.get('https://lacdph.shinyapps.io/covid19_surveillance_dashboard/_w_9d504c84/session/9c3e5e708ae673c42b04153999c491ab/download/download5?w=9d504c84')
file = os.path.join(dir, 'personstested.csv')
df = pandas.read_csv(StringIO((persons_date.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)

tests_date = requests.get('https://lacdph.shinyapps.io/covid19_surveillance_dashboard/_w_9d504c84/session/9c3e5e708ae673c42b04153999c491ab/download/download6?w=9d504c84')
file = os.path.join(dir, 'dailytesting.csv')
df = pandas.read_csv(StringIO((tests_date.text)))
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]
df.to_csv(file, index = False, header = True)
print(file)