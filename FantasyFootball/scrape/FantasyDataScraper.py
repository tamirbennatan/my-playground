#!/usr/bin/python

"""
Utilities for scraping fantasy football data for individuals/teams on a game by game basis for all seasons past 2001.

"""

from bs4 import BeautifulSoup
import urllib2
import csv
import os
import errno
import pdb 

class FantasyDataScraper(object):

	def __init__(self, base_path = None, scoring_scheme = 0, season_type = 0, year = 0, week = 0, position = 0):
		# Base url for NFL Fantasy data
		self.base_path = "https://fantasydata.com/nfl-stats/nfl-fantasy-football-stats.aspx"
		'''
		Fantasy scoring scheme. Default is the standard fantasy scoring scheme. 

		Options:
			0: standard
			1: PPR
			2: FanDuel
			3: DraftKings
			4: Yahoo
		'''
		self.scoring_scheme = scoring_scheme
		'''
		Season type: pick between pre-season, regular season or post-season. Default regular season.

		Options:
			0: Regular Season
			1: Pre-Season
			2: Post-Season
		'''
		self.season_type = season_type
		# Season year. Encoded as number of seasons before current season. For example, 0 is 2017, and 1 is 2016. Default: 0 (2017 NFL season)
		self.year = year
		# Week since the begining of the season type. For example, if currently in regular season, '0' represents week one. If in playoffs, '0' represents wildcard week. 
		self.week = week
		'''
		Limit data to players of certain positions Default is 0 (all postions)

		Options:
			0: All positions
			1: Team Offense
			2: Quarter Back (QB)
			3: Running Back (RB)
			4: Wide Receiver (WR)
			5: Tight End (TE)
			6: Team Defense
			7: Defensive Line (DL)
			8: Line Backer (LB)
			9: Defensive Back (DB)
			10: Kicker (K)
			11: Team Defensive and Special Teams (DST)
		'''
		self.position = position

	# Return the url for the fantasydata.com page, given parameters passed to the class constructor. 
	def get_url(self):
		url = "%s?fs=%d&stype=%d&sn=%d&scope=1&w=%d&ew=%d&s=&t=0&p=%d&st=FantasyPoints&d=1&ls=&live=false&pid=false&minsnaps=4" % (self.base_path, self.scoring_scheme, self.season_type, self.year, self.week, self.week, self.position)
		return url

	def write_data(self):
		'''
		Set parameters needed to define the file name.
		File will live in /data/player_data/[POSITION]/[YEAR]/week-[WEEK].csv. below YEAR, POSITION, and WEEK  are stored. 
		'''
		# store position as string shorthand
		position_options = {
		0 : 'all-positions',
		1: 'team-offense', 
		2: 'QB',
		3: 'RB',
		4: 'WR',
		5: 'TE', 
		6: 'team-defense',
		7: 'DL',
		8: 'LB',
		9: 'DB',
		10: 'K',
		11: 'DST'
		}
		POSITION = position_options[self.position]
		# Store year as string
		YEAR = str(2017 - self.year)
		# Store week as string. Add one because in fantasydata.com url weeks are 0-indexed, but I want subdirectory names to be 1-indexed. 
		WEEK = str(self.week + 1)

		# set path to write to
		PATH = './data/player_data/{0}/{1}/week-{2}.csv'.format(POSITION, YEAR, WEEK)

		# get fantasydata.com url based on parameters passed to contructor
		url = self.get_url()
		


		# Create the subdirectories where the file will live (if do not already exist)
		if not os.path.exists(os.path.dirname(PATH)):
			try:
				os.makedirs(os.path.dirname(PATH))
			except OSError as exc: # Guard against race condition
				if exc.errno != errno.EEXIST:
					raise

		# Open a file. If it already exists, overwrite it. 
		with open(PATH, 'w+') as file:


			# Open the fantasydata.com url.
			page = urllib2.urlopen(url)
			# Construct a BeautifulSoup object with the page contents
			soup = BeautifulSoup(page, 'html.parser')

			# Extract only the headers of the table. Store as a list. 
			headers = [header.get_text().encode('ascii', 'ignore') for header in soup.find_all('th')]
			



			'''
			Extract only the table of data in this page and store it in a list. Each row (<tr> tag) populates an element of the list.
			Ignore the first row, because this will contain the header names, which were already written to the file. 
			'''
			table = soup.find_all('tr')[1:]
			# Iterate through the rows of this table
			for row in table:
				# To write rows to file by populating dictionaries and writing dictionaries to file, need to list order of keys beforehand (which will become the columns). 
				# These keys will be specified in 'fieldnames'
				fieldnames = list()

				# Populate a dictionary with the contents of the row that will be written.
				row_contents = dict()
				'''
				For each header, Populate the corresponding contents. 
				For some reason, each the first and last element in each 'row' list is a newline character, so look at the values in this list except the first and last. 
				''' 

				for i in range(len(headers)):
					# Avoid overwriting  data with duplicate column names (this exists in FantasyData.com data - dont ask me why)
					if headers[i] not in row_contents:
						row_contents[headers[i]] = row.contents[1:-1][i].get_text().encode('ascii', 'ignore')
						# add header to fieldnames
						fieldnames.append(headers[i])
					else:
						row_contents[headers[i] + '_x'] = row.contents[1:-1][i].get_text().encode('ascii', 'ignore')
						# add header to fieldnames
						fieldnames.append(headers[i] + '_x')
					


				


				# Add the year, week, and flag if it is during the regular season or not. 
				row_contents['year'] = 2017 - self.year
				row_contents['week'] = self.week + 1
				row_contents['regular-season'] = ( 1 if self.season_type == 0 else 0)

				# Add fields which will be part of the csv table written, regardless of input to the class constructor.
				fieldnames.extend(['year', 'week', 'regular-season'])

				# Construct a csv writer. This will allow me to write contents of dictionaries to csv file.
				writer = csv.DictWriter(file, fieldnames = fieldnames)

				# Write the header for the csv file just before writing the first row. 
				if row == table[0]:
					writer.writeheader()


				# Write the contents of the dictionary to the file.
				writer.writerow(row_contents)

# A function that will use the class defined above to write tables for all the positions, years and weeks. 
def main():
	# Iterate through all the combinations of positions, weeks, and years. 
	for position in range(12):
		for year in range(7):
			for week in range(18):
				# Create a scraper object
				scraper = FantasyDataScraper(year = year, week = week, position = position)
				# Write a CSV File
				scraper.write_data()


if __name__ == '__main__':
	main()



