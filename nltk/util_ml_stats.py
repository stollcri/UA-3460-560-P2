#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator

def process_file():
	dict_graph = {}
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			csv_col = ast.literal_eval(csv_row[1])
			for sent in csv_col:
				dict_phrase = '-'.join(sent)
				if dict_phrase in dict_graph:
					dict_graph[dict_phrase] = dict_graph[dict_phrase] + 1
				else:
					dict_graph[dict_phrase] = 1

	print sorted(dict_graph.iteritems(), key=operator.itemgetter(1)) #, reverse=True)

	key_sizes = []
	limit_a = 28
	limit_b = 32
	limit_a_count = 0
	limit_b_count = 0
	for key in dict_graph:
		key_sizes.append(len(key))
		if len(key) > limit_a:
			limit_a_count = limit_a_count + 1
		if len(key) > limit_b:
			limit_b_count = limit_b_count + 1

	print
	print "number of keys: ", len(key_sizes)
	print "average size: ", sum(key_sizes)/len(key_sizes)
	print "limit a: ", limit_a
	print "limit b: ", limit_b
	print "limit a #/%: ", limit_a_count, "{0:.0f}%".format(float(limit_a_count)/len(key_sizes) * 100)
	print "limit b #/%: ", limit_b_count, "{0:.0f}%".format(float(limit_b_count)/len(key_sizes) * 100)


if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: process.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()