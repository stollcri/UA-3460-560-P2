#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, nltk
# import cPickle

# from NLTK book examples
# (actually a cascaded finite-state transducer)
def cfst(text_raw, text_stemmer, text_parser_a):
	# TOKENIZATION: split into sentences
	text_sentences = nltk.sent_tokenize(text_raw)
	
	# TOKENIZATION: split into words
	text_words = [nltk.word_tokenize(sent) for sent in text_sentences]

	# TOKENIZATION: tag the words' part of speach
	text_posed = [nltk.pos_tag(sent) for sent in text_words]
	
	# TOKENIZATION: stem and also remove sentences without either verbs or nouns
	text_cleaned = []
	for sent in text_posed:
		noun_found = False
		verb_found = False
		new_sent = []
		for word in sent:
			if word[1].startswith('NN'):
				noun_found = True
			if word[1].startswith('VB'):
				verb_found = True
			new_sent.append((text_stemmer.stem(word[0]), word[1]))

		if noun_found and verb_found:
			text_cleaned.append(new_sent)
			# reverse sentence order while we are here
			# (put the sentences in chronological order)
			# (gets weird when sentences are split wrong)
			#text_cleaned.insert(0, new_sent)
	
	# only keep three sentences
	# (these are most likely to be from the user)
	#text_cleaned = text_cleaned[:3]

	# COMPLEX-WORD HANDLING
	# TODO: Add something here (or earlier)
	#		Look for most common tri-grams and bi-grams?

	# BASIC-GROUP HANDLING: chunk the words at named entities
	text_chunked = nltk.batch_ne_chunk(text_cleaned)#[nltk.chunk.ne_chunk(sent) for sent in text_cleaned]

	# COMPLEX-PHRASE HANDLING: chunk the words
	text_chunked = [text_parser_a.parse(sent) for sent in text_chunked]
	#for sent in text_chunked:
	#	print sent
	#	print

	# unwind the tree only keeping interesting parts
	text_done = []
	for sent in text_chunked:
		for x in xrange(0, len(sent)):
			tmp_string = str(sent[x])
			if tmp_string.startswith('(NP') or tmp_string.startswith('(VP'):
				text_frag = []
				for y in xrange(0, len(sent[x])):
					word = sent[x][y]
					if len(word) == 2:
						if len(word[0][0]) > 1:
							for sub_word in word:
								if len(sub_word[0]) > 0:
									#print "a", str(sub_word[0]).lower()
									text_frag.append(str(sub_word[0]).lower())
						else:
							if len(word[0]) > 0:
								#print "b", str(word[0]).lower()
								text_frag.append(str(word[0]).lower())
					elif len(word) > 2:
						for sub_word in word:
							if len(sub_word[0]) > 0:
								#print "c", len(sub_word), str(sub_word[0]).lower()
								text_frag.append(str(sub_word[0]).lower())
				if (len(text_frag) > 0):
					text_done.append('-'.join(text_frag))

	return text_done


def process_file():
	#text_stemmer = nltk.PorterStemmer()
	#text_stemmer = nltk.snowball.EnglishStemmer()
	text_stemmer = nltk.lancaster.LancasterStemmer()
	parser_grammar_a = r"""
		NP: {<NN|NNS|PERSON><CC><NN|NNS|PERSON>}
		NP: {<JJ|ORGANIZATION|PERSON|NN.*>+}
		VP: {<RB|VB|VBD|VBG|VBN|VBP>+}
		"""
	text_parser_a = nltk.RegexpParser(parser_grammar_a, loop=1)

	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 4):

			# nlp for the problem statement
			row_one_chunked = cfst(csv_row[2], text_stemmer, text_parser_a)
			if len(row_one_chunked) > 0:

				# nlp for the problem solution
				row_two_chunked = cfst(csv_row[3], text_stemmer, text_parser_a)
				if len(row_two_chunked) > 0:
					csv_new = ['', '', '', '', '', '', '']
					csv_new[0] = csv_row[0] 		# index number
					csv_new[1] = csv_row[4]			# solved by (group)
					csv_new[2] = csv_row[5]			# solved by (person)
					csv_new[3] = row_one_chunked	# problem (nlp'd)
					csv_new[4] = row_two_chunked	# solution (nlp'd)
					csv_new[5] = csv_row[2]			# problem (original)
					csv_new[6] = csv_row[3]			# solution (original)

					# TODO: Maybe use the pickle function?
					#print cPickle.dumps(csv_new)
					csv_output = csv.writer(sys.stdout, delimiter=',', 
						quotechar='"', quoting=csv.QUOTE_MINIMAL)
					csv_output.writerow(csv_new)


if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: process.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()
		