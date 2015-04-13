"""
googFinScaper.py: Downloads historical price data from Google Finance
Command Line Arguments: 
	1: source
	2: start date (optional), in m/d/y form. If included, must also include end date.
	3: end date (optional), in m/d/y form
	4: destination (optional), where to save output files
"""

from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium import webdriver
import time
import re
import sys

q = sys.argv[1]
start_date = None
end_date = None

profile = webdriver.FirefoxProfile()
profile.permissions_default_stylesheet = 2
profile.permissions_default_image = 2
wd = webdriver.Firefox(profile)
#wd = webdriver.Firefox()
wait = WebDriverWait(wd, 1000)
wd.get("https://www.google.com/finance/historical?q=" + q)
