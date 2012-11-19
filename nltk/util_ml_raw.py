#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast

def process_file():
	row_to_use = 1
	dict_graph = {}
	dict_size_limit = 32 # 2x the average, excludes ~7% of possible phrases
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			csv_col = ast.literal_eval(csv_row[row_to_use])
			for sent in csv_col:
				dict_phrase = '-'.join(sent)
				if len(dict_phrase) > 0 and len(dict_phrase) < dict_size_limit:
					if dict_phrase in dict_graph:
						dict_graph[dict_phrase].append(int(csv_row[0]))
					else:
						dict_graph[dict_phrase] = []
						dict_graph[dict_phrase].append(int(csv_row[0]))

	print dict_graph


if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: process.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()