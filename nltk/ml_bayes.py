#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator

def process_file(test_file, search_string):
	row_to_use = 1
	dict_graph = {}
	dict_size_limit = 32 # 2x the average, excludes ~7% of possible phrases

	# build 'graph' from training data
	csv_file = open(test_file, 'rb')
	csv_reader = csv.reader(csv_file, delimiter=',', quotechar='"')
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

	# make list of nodes in graph that match search nodes
	# and keep track of the hit percentage
	hit_made = 0
	hit_tries = 0
	hit_list = []
	search_list = ast.literal_eval(search_string)
	for search_sent in search_list:
		search_phrase = '-'.join(search_sent)
		hit_tries = hit_tries + 1

		if dict_graph.get(search_phrase, '') != '':
			hit_made = hit_made + 1
			hit_list.append(search_phrase)
			#print "yes", search_phrase
		else:
			#print "no", search_phrase
			continue

	hit_confidence_a = float(hit_made) / hit_tries
	#print "Confidence A:", "{0:.0f}%".format(hit_confidence_a * 100)

	# find the intersection of training and test data
	match_made = 0
	intersection = []
	if hit_confidence_a > .75:
		# longest matching nodes (more unique) first
		# (or sort by number of hits in graph?)
		hit_list.sort(key = lambda s: -len(s))
		for x in xrange(1,len(hit_list)):
			#print x, hit_list[x-1], hit_list[x]
			if x == 1:
				intersection = set(dict_graph[hit_list[x-1]]) & set(dict_graph[hit_list[x]])
				if len(intersection) > 0:
					match_made = match_made + 1
				#print "if", intersection
			else:
				new_intersection = set(intersection) & set(dict_graph[hit_list[x]])
				if len(new_intersection) > 0:
					match_made = match_made + 1
					intersection = new_intersection
				#intersection = set(intersection) & set(dict_graph[hit_list[x]])
				#if len(intersection) > 0:
				#	match_made = match_made + 1
				#print "el", intersection

		hit_confidence_b = float(match_made) / hit_made
		#print "Confidence B:", "{0:.0f}%".format(hit_confidence_b * 100)

		hit_confidence_c = float(match_made) / hit_tries
		#print "Confidence C:", "{0:.0f}%".format(hit_confidence_c * 100)

		if len(intersection) == 0:
			print "No results. (", hit_confidence_a, ", ", hit_confidence_b, ", ", hit_confidence_c, ")"
		elif hit_confidence_c < .5:
			print "Unreliable results. (", hit_confidence_a, ", ", hit_confidence_b, ", ", hit_confidence_c, ")"
		else:
			print "Reliable results. (", hit_confidence_a, ", ", hit_confidence_b, ", ", hit_confidence_c, "):",
			for item in intersection:
				print item,
			print


if __name__ == "__main__":
	if len(sys.argv) >= 2:
		process_file("./test/i1_train.csv", sys.argv[1])
