#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator

def build_graph(train_file):
	row_to_use = 1
	dict_probs = {}
	dict_solns = {}
	dict_graph = {}
	dict_size_limit = 64 # 2x the average, excludes ~7% of possible phrases

	# build 'graph' from training data
	csv_file = open(train_file, 'rb')
	csv_reader = csv.reader(csv_file, delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			csv_col = ast.literal_eval(csv_row[row_to_use])
			#print csv_row[0], csv_row[3]
			dict_probs[int(csv_row[0])] = csv_row[3]
			dict_solns[int(csv_row[0])] = csv_row[4]
			for sent in csv_col:
				#dict_phrase = '-'.join(sent)
				dict_phrase = sent
				if len(dict_phrase) > 0 and len(dict_phrase) < dict_size_limit:
					if dict_phrase in dict_graph:
						dict_graph[dict_phrase].append(int(csv_row[0]))
					else:
						dict_graph[dict_phrase] = []
						dict_graph[dict_phrase].append(int(csv_row[0]))
					#print csv_row[0], dict_phrase
	return dict(graph=dict_graph, problems=dict_probs, solutions=dict_solns)

def search_graph(dict_graph, dict_probs, dict_solns, search_string, index, orig_problem, orig_solution):
	#print

	# make list of nodes in graph that match search nodes
	# and keep track of the hit percentage
	hit_made = 0
	hit_tries = 0
	hit_dict = {}
	hit_list = []
	hit_tuples = []
	search_list = ast.literal_eval(search_string)
	for search_sent in search_list:
		#search_phrase = '-'.join(search_sent)
		search_phrase = search_sent
		hit_tries += 1

		if dict_graph.get(search_phrase, '') != '':
			hit_made += 1
			# Prevent duplication
			if hit_dict.get(search_phrase, '') == '':
				hit_dict[search_phrase] = 1
				hit_list.append(search_phrase)
				hit_tuples.append((search_phrase, dict_graph[search_phrase]))
				#print "yes:", search_phrase
		else:
			#print "no:", search_phrase
			continue

	hit_confidence_a = float(hit_made) / hit_tries
	hit_confidence_b = 0
	hit_confidence_c = 0

	# find the intersection of training and test data
	match_made = 0
	intersection = []
	if hit_confidence_a > .6:
		# to search dict starting with most common words
		hit_tuples = sorted(hit_tuples, key=lambda hit: -len(hit[1]))

		# longest matching nodes (more unique) first
		# (or sort by number of hits in graph?)
		#
		# This (below) makes accuracy slightly worse
		#hit_list.sort(key = lambda s: -len(s))
		#
		# TODO:
		# need to grab all intersections
		# then sort based upon the number of nodes
		# then perform the union
		# -OR-
		# join all of the possible matches and then sort by probability?
		#print hit_list
		for x in xrange(1,len(hit_list)):
			last_phrase = hit_tuples[x-1][0]
			this_phrase = hit_tuples[x][0]
			if x == 1:
				intersection = set(dict_graph[last_phrase]) & set(dict_graph[this_phrase])
				if len(intersection) > 0:
					match_made += 2
				#print "if", intersection
			else:
				new_intersection = set(intersection) & set(dict_graph[this_phrase])
				if len(new_intersection) > 0:
					match_made += 1
					intersection = new_intersection
				#intersection = set(intersection) & set(dict_graph[hit_list[x]])
				#if len(intersection) > 0:
				#	match_made += 1
				#print "el", intersection
			#print "intersection:", last_phrase, this_phrase, intersection

		hit_confidence_b = float(match_made) / hit_made
		hit_confidence_c = float(match_made) / hit_tries

	if len(intersection) == 0:
		"""
		print "No results. (",
		print "{0:.0f}%, ".format(hit_confidence_a * 100),
		print "{0:.0f}%, ".format(hit_confidence_b * 100),
		print "{0:.0f}%) ".format(hit_confidence_c * 100)
		"""
		return False
	elif hit_confidence_c < .6:
		"""
		print "Unreliable results. (",
		print "{0:.0f}%, ".format(hit_confidence_a * 100),
		print "{0:.0f}%, ".format(hit_confidence_b * 100),
		print "{0:.0f}%) ".format(hit_confidence_c * 100)
		"""
		return False
	else:
		print "Reliable results. (",
		print "{0:.0f}%, ".format(hit_confidence_a * 100),
		print "{0:.0f}%, ".format(hit_confidence_b * 100),
		print "{0:.0f}%) ".format(hit_confidence_c * 100),
		print "Hit count: ", len(intersection)

		print "ACTUAL PROBLEM:   ", index, "--", orig_problem
		print "ACTUAL SOLUTION:  ", index, "--", orig_solution
		for item in intersection:
			#print item,
			print "PROPOSED PROBLEM: ", item, "--", dict_probs[item]
			print "PROPOSED SOLUTION:", item, "--", dict_solns[item]

		print
		return True


def run_test(train_file, test_file):
	test_bad = 0
	test_good = 0

	train_results = build_graph(train_file)
	train_graph = train_results["graph"]
	train_probs = train_results["problems"]
	train_solns = train_results["solutions"]

	test_file = open(test_file, 'rb')
	csv_test = csv.reader(test_file, delimiter=',', quotechar='"')
	for csv_row in csv_test:
		if len(csv_row) > 1:
			if len(csv_row[1]) > 0:
				test_results = search_graph(train_graph, train_probs, train_solns, csv_row[1], csv_row[0], csv_row[3], csv_row[4])
				if test_results:
					test_good += 1
				else:
					test_bad += 1
	return dict(good=test_good, bad=test_bad)


if __name__ == "__main__":
	test_bad = 0
	test_good = 0
	test_results = dict(good=0, bad=0)

	test_results = run_test("i1_train.csv", "i1_test.csv")
	test_bad += test_results["bad"]
	test_good += test_results["good"]

	print
	print "Bad results:", test_bad
	print "Good results", test_good
	print "Potential accuracy: {0:.0f}% ".format(float(test_good) / (test_good + test_bad) * 100)
"""
	test_results = run_test("i2_train.csv", "i2_test.csv")
	test_bad += test_results["bad"]
	test_good += test_results["good"]

	print
	print "Bad results:", test_bad
	print "Good results", test_good
	print "Potential accuracy: {0:.0f}% ".format(float(test_good) / (test_good + test_bad) * 100)

	test_results = run_test("i3_train.csv", "i3_test.csv")
	test_bad += test_results["bad"]
	test_good += test_results["good"]

	print
	print "Bad results:", test_bad
	print "Good results", test_good
	print "Potential accuracy: {0:.0f}% ".format(float(test_good) / (test_good + test_bad) * 100)

	test_results = run_test("i4_train.csv", "i4_test.csv")
	test_bad += test_results["bad"]
	test_good += test_results["good"]

	print
	print "Bad results:", test_bad
	print "Good results", test_good
	print "Potential accuracy: {0:.0f}% ".format(float(test_good) / (test_good + test_bad) * 100)

	test_results = run_test("i5_train.csv", "i5_test.csv")
	test_bad += test_results["bad"]
	test_good += test_results["good"]

	print
	print "Bad results:", test_bad
	print "Good results", test_good
	print "Potential accuracy: {0:.0f}% ".format(float(test_good) / (test_good + test_bad) * 100)
	print
"""