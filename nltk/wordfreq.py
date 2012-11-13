#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, nltk

def process_file(file_name):
	dictionary = {}
	dict_strg = open('/usr/share/dict/words', 'r').read()
	dict_list = dict_strg.split()
	for word in dict_list:
		dictionary[word.lower()] = 1
	dict_list = []
	dict_strg =""

	file_string = open(file_name, 'r').read()
	file_list = file_string.split()
	word_freq = nltk.FreqDist(file_list)
	word_list = list(word_freq)

	odd_words = []
	for word in word_list:
		if dictionary.get(word, 0) == 0:
			odd_words.append(word)

	print odd_words

if __name__ == "__main__":
	if len(sys.argv) >= 2:
		process_file(sys.argv[1])