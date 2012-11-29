#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator

def build_graph(train_file):
	dict_graph = {}
	dict_score = {}
	corpus_size = 0
	reversed_phrases = {}
	dict_size_limit = 64

	csv_file = open(train_file, 'rb')
	csv_reader = csv.reader(csv_file, delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			#print "PROBLEM: ", csv_row[3]
			#print "SOLUTION:", csv_row[4]

			csv_col = ast.literal_eval(csv_row[1])
			phrase_prob = 1.0 / len(csv_col)
			for dict_phrase in csv_col:
				corpus_size += 1

				# record each phrase and problems which contain it
				if len(dict_phrase) > 0 and len(dict_phrase) < dict_size_limit:
					# phrase exists in the graph
					if dict_phrase in dict_graph:
						dict_graph[dict_phrase].append(int(csv_row[0]))
						dict_score[dict_phrase].append(phrase_prob)
					# new phrase
					else:
						dict_graph[dict_phrase] = []
						dict_graph[dict_phrase].append(int(csv_row[0]))
						dict_score[dict_phrase] = []
						dict_score[dict_phrase].append(phrase_prob)
				
				# phrases which may be reversable
				if "-and-" in dict_phrase:
					dict_phrase_list = dict_phrase.split("-")
					# phrases in the format "a and b"
					if len(dict_phrase_list) == 3:
						# phrases which are not "a and a"
						if dict_phrase_list[0] != dict_phrase_list[2]:
							# reverse the phrase and record it for latter
							dict_phrase_list = reversed(dict_phrase_list)
							dict_phrase_reverse = '-'.join(dict_phrase_list)
							reversed_phrases[dict_phrase] = dict_phrase_reverse

	# deal with reversable phrases
	for dict_phrase in reversed_phrases:
		dict_phrase_reverse = reversed_phrases[dict_phrase]

		# the reversal does not exist yet
		if dict_graph.get(dict_phrase_reverse, '') == '':
			dict_graph[dict_phrase_reverse] = dict_graph[dict_phrase]
			dict_score[dict_phrase_reverse] = dict_score[dict_phrase]
		else:
			phrase_joined = list(set.union(set(dict_graph[dict_phrase]), set(dict_graph[dict_phrase_reverse])))
			dict_graph[dict_phrase_reverse] = phrase_joined
			dict_graph[dict_phrase] = phrase_joined

			scores_joined = list(set.union(set(dict_score[dict_phrase]), set(dict_score[dict_phrase_reverse])))
			dict_score[dict_phrase_reverse] = scores_joined
			dict_score[dict_phrase] = scores_joined

	# probabilities
	dict_probs = {}
	for word in dict_graph:
		# probability of the phrase in the corpus
		phrase_probability_global = float(len(dict_graph[word])) / corpus_size * 100

		# average probability of phrase in context
		phrase_probability_local = 0
		word_scores = dict_score[word]
		for score in word_scores:
			phrase_probability_local = phrase_probability_local + score
		phrase_probability_local = phrase_probability_local / len(word_scores) * 100

		# sum probability
		phrase_probability_sum = phrase_probability_global * phrase_probability_local
		dict_probs[word] = phrase_probability_sum

	return dict(graph=dict_graph, scores=dict_score, probabilities=dict_probs, corpus_size=corpus_size)


def limit_graph(train_data, query_string_list):
	dict_graph = train_data["graph"]
	dict_score = train_data["scores"]

	hits_dict = {}
	hits_list = []
	hits_tuples = []
	# find nodes that the query overlaps
	for query_string in query_string_list:
		if dict_graph.get(query_string, '') != '':
			if hits_dict.get(query_string, '') == '':
				hits_dict[query_string] = 1
				hits_list.append(query_string)
				hits_tuples.append((query_string, dict_graph[query_string]))

	# sort to search dict starting with most common words
	hits_tuples = sorted(hits_tuples, key=lambda hit: -len(hit[1]))
	
	# join all nodes into a single set
	for x in xrange(1,len(hits_list)):
		last_phrase = hits_tuples[x-1][0]
		this_phrase = hits_tuples[x][0]
		if x == 1:
			intersection = set(dict_graph[last_phrase]).union(set(dict_graph[this_phrase]))
		else:
			intersection = set(intersection).union(set(dict_graph[this_phrase]))

	print len(intersection)


def test_graph(train_data, test_file):
	manual_data = ['do-not-hav', 'access', 'workspac', 'pdm-link', 'am', 'abl', 'log', 'prop', 'access-pdmlink', 'just-do-not-hav', 'access', 'context']
	test_data = limit_graph(train_data, manual_data)


def run_test(train_file, test_file):
	train_data = build_graph(train_file)
	test_graph(train_data, test_file)


if __name__ == "__main__":
	run_test("./test/i1_train.csv", "./test/i1_test.csv")
