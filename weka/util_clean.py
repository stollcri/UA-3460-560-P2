#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, re

def process_file():
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		csv_new = ['', 'USER-UNKNOWN', 'GROUP-UNKNOWN', '', '', '']

		if (len(csv_row) >= 1):
			if(csv_row[0] != 'Incident #'):
				csv_new[0] = int(csv_row[0])
			else:
				csv_new[0] = csv_row[0]

		if (len(csv_row) >= 6):
			if len(csv_row[5]) > 0:
				csv_new[1] = csv_row[5]

		if (len(csv_row) >= 5):
			if len(csv_row) > 0:
				csv_new[2] = csv_row[4]

		if (len(csv_row) >= 2):
			csv_new[3] = re.sub('"', "'", csv_row[1]) + ' '

		if (len(csv_row) >= 3):
			csv_new[4] = re.sub('"', "'", csv_row[2]) + ' '

		if (len(csv_row) >= 4):
			csv_new[5] = re.sub('"', "'", csv_row[3]) + ' '

		csv_output = csv.writer(sys.stdout, delimiter=',', 
			quotechar='"', quoting=csv.QUOTE_NONNUMERIC)
		csv_output.writerow(csv_new)

if __name__ == "__main__":
	process_file()