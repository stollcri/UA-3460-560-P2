#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, re, nltk

# from the NLTK
class ConsecutiveNPChunkTagger(nltk.TaggerI): # [_consec-chunk-tagger]
	def __init__(self, train_sents):
		train_set = []
		for tagged_sent in train_sents:
			untagged_sent = nltk.tag.untag(tagged_sent)
			history = []
			for i, (word, tag) in enumerate(tagged_sent):
				featureset = npchunk_features(untagged_sent, i, history) # [_consec-use-fe]
				train_set.append( (featureset, tag) )
				history.append(tag)
		self.classifier = nltk.MaxentClassifier.train( # [_consec-use-maxent]
			train_set, algorithm='megam', trace=0)

	def tag(self, sentence):
		history = []
		for i, word in enumerate(sentence):
			featureset = npchunk_features(sentence, i, history)
			tag = self.classifier.classify(featureset)
			history.append(tag)
		return zip(sentence, history)

# from the NLTK
class ConsecutiveNPChunker(nltk.ChunkParserI): # [_consec-chunker]
	def __init__(self, train_sents):
		tagged_sents = [[((w,t),c) for (w,t,c) in
						nltk.chunk.tree2conlltags(sent)]
						for sent in train_sents]
		self.tagger = ConsecutiveNPChunkTagger(tagged_sents)

	def parse(self, sentence):
		tagged_sents = self.tagger.tag(sentence)
		conlltags = [(w,t,c) for ((w,t),c) in tagged_sents]
		return nltk.chunk.conlltags2tree(conlltags)

def npchunk_features(sentence, i, history):
		word, pos = sentence[i]
		return {"pos": pos}



# from the NLTK
class UnigramChunker(nltk.ChunkParserI):
	def __init__(self, train_sents): 
		train_data = [[(t,c) for w,t,c in nltk.chunk.tree2conlltags(sent)]
						for sent in train_sents]
		self.tagger = nltk.UnigramTagger(train_data) 

	def parse(self, sentence): 
		pos_tags = [pos for (word,pos) in sentence]
		tagged_pos_tags = self.tagger.tag(pos_tags)
		chunktags = [chunktag for (pos, chunktag) in tagged_pos_tags]
		conlltags = [(word, pos, chunktag) for ((word,pos),chunktag)
						in zip(sentence, chunktags)]
		return nltk.chunk.conlltags2tree(conlltags)



# from NLTK book examples
def chunk_regex(text_string):
	# TODO: don't leave this here, no reason to do this work repeatedly
	#		if this is used for production, abstract these two declarations
	parser_grammar = r"""
		NP: {<DT|PP\$>?<JJ>*<NN>} # chunk determiner/possessive, adjectives and nouns
			{<NNP>+}  			  # chunk sequences of proper nouns
		"""
	parser_grammar = r"""
		NP: {<CD|DT|JJ|NN.*>+}       # Chunk sequences of DT, JJ, NN
		NP: {<NP><CC><NP>}
		PP: {<IN><NP>}               # Chunk prepositions followed by NP
		VB: {<MD|TO|VBZ><VB.*>} 
		VP: {<VB.*><NP|PP|CLAUSE|VB.*>+}  # Chunk verbs and their arguments
		CLAUSE: {<NP><VP>}           # Chunk NP, VP
		"""
	parser_grammar = r"""
		NPP: {<NPP>+}
		NP: {<IN|CD|DT|JJ|NN.*>+}       # Chunk sequences of DT, JJ, NN
		NP: {<NP><CC><NP>}
		VB: {<MD|RB|TO|VBZ><VB.*>+} 
		VP: {<VB><NP|PP|RP>+} 
		CLAUSE: {<NP><VP>}           # Chunk NP, VP
		"""
	text_parser = nltk.RegexpParser(parser_grammar, loop=2)

	# split into sentences
	text_sentns = nltk.sent_tokenize(text_string)
	
	# split into words
	text_sentns = [nltk.word_tokenize(sent) for sent in text_sentns]
	
	# spellcheck
	#snts_new = []
	#sent_new = []
	#for sent in text_sentns:
	#	for word in sent:
	#		if dictionary.get(word, 0) != 0:
	#			sent_new.append(word)
	#	snts_new.append(sent_new)
	#	sent_new = []
	#text_sentns = snts_new

	# tag the words' part of speach
	text_sentns = [nltk.pos_tag(sent) for sent in text_sentns]
	
	# remove sentences without verbs or nouns
	new_sent = []
	new_sentns = []
	for sent in text_sentns:
		noun_found = False
		verb_found = False
		for word in sent:
			if word[1].startswith('NN'):
				noun_found = True
			if word[1].startswith('VB'):
				verb_found = True

		if noun_found and verb_found:
			new_sentns.append(sent)

	# chunk the words
	text_parsed = [text_parser.parse(sent) for sent in new_sentns]
	
	return text_parsed


def process_file():
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 2):
			row_chunked = chunk_regex(csv_row[1])
			if len(row_chunked) > 0:
				print "----- ----- -----"
				for sent in row_chunked:
					print sent

if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: process.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()