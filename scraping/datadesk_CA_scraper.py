#scraper for .csv files found at https://github.com/datadesk/california-coronavirus-data
#requires requests package -> pip install requests

import requests
import re
import json
import os.path

#directory parameter
dir = 'csv'

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