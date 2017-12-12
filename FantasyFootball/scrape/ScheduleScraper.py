'''
Utilities for scraping schedule data. This will be used to determine matchups for each team for each week, and whether the team was home or away. 
'''

from bs4 import BeautifulSoup
import urllib2
import csv
import os

class ScheduleScraper(object):
	
	def __init__(self, base_url = None, year = 2017):
		# Set base url for scraping schedule from espn.com
		self.base_url = "http://www.espn.com/nfl/schedulegrid/_/year/"

		self.year = year

	def get_url(self):
		return "%s%s" % (self.base_url, self.year)

	def write_data(self):


		'''
		Set the file name.
		File will will live in /data/schedules/[YEAR]
		'''
		PATH = "./data/schedules/%d.csv" % (self.year)
		# Create the subdirectories the file will live in if they do not already exist. 
		if not os.path.exists(os.path.dirname(PATH)):
			try:
				os.makedirs(os.path.dirname(PATH))
			except OSError as exc: # Guard against race condition
				raise

		#Open the file. If already exists, overwrite it. 
		with open(PATH, 'w+') as file:
			# Get the url to open to get the schedule
			url = self.get_url()
			# Open the espn.com url
			page = urllib2.urlopen(url)
			# Construct a BeautifulSoup Object with the page contents
			soup = BeautifulSoup(page, 'html.parser')

			# Construct a csv writer. This will allow me to write python lists to the csv file
			writer = csv.writer(file)

			# First, write a row for the header of the table. 
			writer.writerow(['team'] + ['week.' + str(week_num) for week_num in range(1,18)])

			'''
			Extract the table of data which shows the paiwise schedule. 
			The first row is an empty header, and the second has the headers (which I already wrote). So only write rows 2 and onwards.
			'''
			table = soup.find_all('tr')

			for row_index in range(2, len(table)):
				'''
				Write row of data to file. 
				first, extract a row and store it in memory as a list, then write the row. 
				'''
				writer.writerow([value.get_text().encode('ascii', 'ignore') for value in table[row_index]])

# A function that will use the class defined above to wrtie tables for all years after and includeing [start_year]
def main(start_year = 2011):
	# Iterate through previous years starting at [start_year]
	for year in range(start_year, 2017 + 1):
		print "Completed scraping schedule for year %d" % (year).
		# construct scraper opject
		scraper = ScheduleScraper(year = year)
		# write file of schedule
		scraper.write_data()

if __name__ == '__main__':
	main()