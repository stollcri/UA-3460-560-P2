#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator, math

def process_file():
	row_to_use = 1
	dict_graph = {}
	dict_size_limit = 512 # 2x the average, excludes ~7% of possible phrases
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			csv_col = ast.literal_eval(csv_row[row_to_use])
			for sent in csv_col:
				dict_phrase = '-'.join(sent)
				if len(dict_phrase) > 0 and len(dict_phrase) < dict_size_limit:
					if dict_phrase in dict_graph:
						dict_graph[dict_phrase] = dict_graph[dict_phrase] + 1
					else:
						dict_graph[dict_phrase] = 1

	print
	dict_list = sorted(dict_graph.iteritems(), key=operator.itemgetter(1), reverse=True)
	print "the top 50 words:"
	for x in xrange(0,49):
		print dict_list[x]

	key_sizes = []
	limit_a = 8
	limit_b = 16
	limit_c = 32
	limit_d = 64
	limit_a_count = 0
	limit_b_count = 0
	limit_c_count = 0
	limit_d_count = 0
	number_a = 1
	number_b = 2
	number_a_count = 0
	number_b_count = 0
	for key in dict_graph:
		key_sizes.append(len(key))
		if len(key) > limit_a:
			limit_a_count = limit_a_count + 1
		if len(key) > limit_b:
			limit_b_count = limit_b_count + 1
		if len(key) > limit_c:
			limit_c_count = limit_c_count + 1
		if len(key) > limit_d:
			limit_d_count = limit_d_count + 1
		if dict_graph[key] == number_a:
			number_a_count = number_a_count + 1
		if dict_graph[key] == number_b:
			number_b_count = number_b_count + 1

	print
	print "items used once:", number_a_count
	print "items used twice:", number_b_count
	print
	print "number of keys: ", len(key_sizes)
	print "median size: ", key_sizes[int(math.floor(len(key_sizes)/2))]
	print "average size: ", sum(key_sizes)/len(key_sizes)
	print "size limit a: ", limit_a
	print "size limit b: ", limit_b
	print "size limit c: ", limit_c
	print "size limit d: ", limit_d
	print "size limit a #/%: ", limit_a_count, "{0:.0f}%".format(float(limit_a_count)/len(key_sizes) * 100)
	print "size limit b #/%: ", limit_b_count, "{0:.0f}%".format(float(limit_b_count)/len(key_sizes) * 100)
	print "size limit c #/%: ", limit_c_count, "{0:.0f}%".format(float(limit_c_count)/len(key_sizes) * 100)
	print "size limit d #/%: ", limit_d_count, "{0:.0f}%".format(float(limit_d_count)/len(key_sizes) * 100)


if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: process.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()