#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, csv

def process_file(file_name, output_columns):
	with open(file_name) as csv_file:
		csv_reader = csv.reader(csv_file, delimiter=',', quotechar='"')
		for csv_row in csv_reader:
			csv_new = ['', '']
			csv_new[0] = csv_row[0]
			
			if output_columns == '12':
				csv_new[1] = csv_row[1]
			
			if output_columns == '13':
				csv_new[1] = csv_row[2]
			
			if output_columns == '14':
				csv_new[1] = csv_row[3]

			if csv_new[1] != '':
				csv_output = csv.writer(sys.stdout, delimiter=',', 
					quotechar='"', quoting=csv.QUOTE_MINIMAL)
				csv_output.writerow(csv_new)

if __name__ == "__main__":
	if len(sys.argv) == 3:
		process_file(sys.argv[1], sys.argv[2])
	else:
		print "Syntax: cvscat.py filename.csv [ 12 | 13 | 14]"
		print " prints columns of a cvs file to standard output"
		print "  12 -- print columns 1 and 2"
		print "  13 -- print columns 1 and 3"
		print "  14 -- print columns 1 and 4"