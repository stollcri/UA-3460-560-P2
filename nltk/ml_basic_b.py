#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, ast, operator

def build_graph(train_file):
	corpus = {}
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

			csv_col = ast.literal_eval(csv_row[3])
			corpus[int(csv_row[0])] = csv_col
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

	return dict(corpus=corpus, graph=dict_graph, scores=dict_score, probabilities=dict_probs, corpus_size=corpus_size)


def compare_statements(string_a, string_b):
	max_value = 0
	matrix = []
	for idx_a, phrase_a in enumerate(string_a):
		matrix.append([])

		for idx_b, phrase_b in enumerate(string_b):
			matrix[idx_a].append(0)

			last_value = 0
			if idx_a > 0 and idx_b >0:
				last_value = matrix[idx_a-1][idx_b-1]

			this_value = last_value
			if phrase_b == phrase_a:
				this_value += 1
			else:
				if this_value > 0:
					this_value -= 1

			matrix[idx_a][idx_b] = this_value

			if this_value > max_value:
				max_value = this_value
		
	return float(max_value) / max(len(string_a), len(string_b))


def search_graph(train_data, query_string_list):
	corpus = train_data["corpus"]
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
	
	# join all nodes into a single set
	all_hits = []
	for x in xrange(1,len(hits_list)):
		last_phrase = hits_tuples[x-1][0]
		this_phrase = hits_tuples[x][0]
		if x == 1:
			all_hits = set(dict_graph[last_phrase]).union(set(dict_graph[this_phrase]))
		else:
			all_hits = set(all_hits).union(set(dict_graph[this_phrase]))

	hits_compared = []
	for hit in all_hits:
		compared = compare_statements(query_string_list, corpus[hit])
		hits_compared.append((compared, hit))

	hits_compared = sorted(hits_compared, key=lambda hit: -hit[0])
	return hits_compared


def test_graph(train_data, test_file):
	hit_pass = 0
	hit_fail = 0
	corpus = train_data["corpus"]
	csv_file = open(test_file, 'rb')
	csv_reader = csv.reader(csv_file, delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):
			csv_col = ast.literal_eval(csv_row[1])
			test_data = search_graph(train_data, csv_col)

			hit_pct = 0
			hit_desc = ""
			if len(test_data) > 0:
				if test_data[0][0] > .5:
					hit_pass += 1
					hit_pct = test_data[0][0]
					hit_desc = corpus[test_data[0][1]]
					#print test_data[0][0], corpus[test_data[0][1]]
				else:
					hit_fail += 1
			else:
				hit_fail += 1
			
			print "go:", hit_pass, "ng:", hit_fail, "|", hit_pct, "|", csv_col, "|", hit_desc

	print "pass:", hit_pass, ", fail:", hit_fail
	print "Potential accuracy: {0:.0f}% ".format(float(hit_pass) / (hit_pass + hit_fail) * 100)


def run_test(train_file, test_file):
	train_data = build_graph(train_file)
	test_graph(train_data, test_file)


if __name__ == "__main__":
	run_test("./test/i1_train.csv", "./test/i1_test.csv")
