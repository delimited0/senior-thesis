"""
factivaScaper.py: Downloads articles from Factiva in HTML form. Uses Princeton's Factiva subscription, 
				  need to be in the Princeton network to use this. Gets all articles from specified source,
				  or all artices between start and end dates provided by user.
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

source = sys.argv[1] # "rst=ferc"
start_date = None
end_date = None
dest = ''
if len(sys.argv) > 2:
	start_date = sys.argv[2].split("/")
	end_date = sys.argv[3].split("/")
	if len(sys.argv) > 4:
		dest = sys.argv[4]
	else:
		raise NameError("Incorrect number of arguments")

profile = webdriver.FirefoxProfile()
profile.permissions_default_stylesheet = 2
profile.permissions_default_image = 2
wd = webdriver.Firefox(profile)
#wd = webdriver.Firefox()
wait = WebDriverWait(wd, 1000)

# set up, enter search terms/params
wd.get("http://library.princeton.edu/resource/3791")
wd.find_element_by_link_text("Access Resource").click()
wd.switch_to_window(wd.window_handles[1])
#print 'choosing all dates'
wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, "select#dr > option[value='_Unspecified']")))
if start_date == None:
	wd.find_element_by_css_selector("select#dr > option[value='_Unspecified']").click()
else:
	wd.find_element_by_css_selector("#dr > option:nth-child(10)").click()
	wait.until(EC.visibility_of_element_located((By.CSS_SELECTOR, "div#datePnl")))
	wd.find_element_by_css_selector("#frm").send_keys(start_date[0])
	wd.find_element_by_css_selector("#frd").send_keys(start_date[1])
	wd.find_element_by_css_selector("#fry").send_keys(start_date[2])
	wd.find_element_by_css_selector("#tom").send_keys(end_date[0])
	wd.find_element_by_css_selector("#tod").send_keys(end_date[1])
	wd.find_element_by_css_selector("#toy").send_keys(end_date[2])

#print 'choosing source'
wait.until(EC.visibility_of_element_located((By.CSS_SELECTOR, "textarea#ftx")))
wd.find_element_by_css_selector("textarea#ftx").send_keys(source)
#print 'click search'
wd.find_element_by_css_selector("li#btnSBSearch").click()

doc_id = 1
while True:
	#if docs_got == 3: break
	# get the articles by checking headlines box, clicking save
	wd.find_element_by_css_selector("#selectAll > input:nth-child(1)").click()
	wd.find_element_by_css_selector(".ppssave > a:nth-child(1)").click()
	wait.until(EC.visibility_of_element_located((By.CSS_SELECTOR, "#listMenu-id-2 > li:nth-child(2) > a:nth-child(1)")))
	wd.find_element_by_css_selector("#listMenu-id-2 > li:nth-child(2) > a:nth-child(1)").click()

	# switch to article page, get html
	wd.switch_to_window(wd.window_handles[2])
	html = re.sub(r'<style.*<\/style>', '', unicode(wd.page_source).encode("utf-8"))
	html = re.sub(r'<input.*/>', '', html)
	document = open(str(dest) + '/data' + str(doc_id) + '.html', 'w')
	document.write(html)
	document.close()
	wd.close()

	# go back and click the next button
	doc_id += 1
	wd.switch_to_window(wd.window_handles[1])
	wd.find_element_by_css_selector("#clearAll > input:nth-child(1)").click()
	wait.until(EC.visibility_of_element_located((By.CSS_SELECTOR, "#selectAll > input:nth-child(1)")))
	wd.find_element_by_css_selector("a.nextItem").click()
	try:
		wait.until(EC.invisibility_of_element_located((By.CSS_SELECTOR, "#_ceprogressindicator > td:nth-child(1) > div:nth-child(1) > img:nth-child(1)")))
	except:
		time.sleep(2)		


