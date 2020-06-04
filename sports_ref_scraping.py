import urllib
from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd

# Read the HTML contents of a web page into Python
url = 'https://medium.com/@smehta/scrape-and-create-your-own-beautiful-dataset-from-sports-reference-com-using' \
      '-beautifulsoup-python-c26d6920684e '
req = urllib.request.Request(url, headers={'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'})
page = urllib.request.urlopen(req).read()
print(page)
