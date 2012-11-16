import collections

	text_sentns = nltk.word_tokenize(csv_file_string)
	bigram_measure = nltk.collocations.BigramAssocMeasures()
	finder = nltk.collocations.BigramCollocationFinder.from_words(text_sentns)
	scored = finder.score_ngrams(bigram_measure.likelihood_ratio)
	prefix_keys = collections.defaultdict(list)
	for key, scores in scored:
		prefix_keys[key[0]].append((key[1], scores))
	for key in prefix_keys:
		prefix_keys[key].sort(key = lambda x: -x[1])
	print prefix_keys
	print