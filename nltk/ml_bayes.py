#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator

def process_file():
	dict_graph = {}
	dict_size_limit = 28 # 2x the average, excludes ~7% of possible phrases
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			csv_col = ast.literal_eval(csv_row[1])
			for sent in csv_col:
				dict_phrase = '-'.join(sent)
				if len(dict_phrase) < dict_size_limit:
					if dict_phrase in dict_graph:
						dict_graph[dict_phrase].append(int(csv_row[0]))
					else:
						dict_graph[dict_phrase] = []
						dict_graph[dict_phrase].append(int(csv_row[0]))

	print dict_graph

	key_sizes = []
	limit_a = 14
	limit_b = 28
	limit_a_count = 0
	limit_b_count = 0
	for key in dict_graph:
		key_sizes.append(len(key))
		if len(key) > limit_a:
			limit_a_count = limit_a_count + 1
		if len(key) > limit_b:
			limit_b_count = limit_b_count + 1

	print
	print "dictionary size: ", sys.getsizeof(dict_graph), "bytes"
	print "number of keys: ", len(key_sizes)
	print "average size: ", sum(key_sizes)/len(key_sizes)
	print "limit a: ", limit_a
	print "limit b: ", limit_b
	print "limit a #/%: ", limit_a_count, "{0:.0f}%".format(float(limit_a_count)/len(key_sizes) * 100)
	print "limit b #/%: ", limit_b_count, "{0:.0f}%".format(float(limit_b_count)/len(key_sizes) * 100)

	area_a = dict_graph["the-error-intern"]
	area_b = dict_graph["benefit-and-payment"]
	print
	print area_a
	print area_b
	intersection = set(area_a) & set(area_b)
	print intersection

if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: process.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()