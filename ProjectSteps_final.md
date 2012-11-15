# Automatic Helpdesk Problem Sover

## Inductive Learning Algorithm

### Create Corpus
Steps to process training/example files into a corpus

#### Python pre-processor
(`>>> import nltk, re, pprint`)

* Remove bogus entries/lines
	* ones which were automatically handled (password resets)
* Normalize the text
	* remove dates and times
	* remove other know bogus strings
* Stem the verbs
	* `>>> nltk.PorterStemmer()`
* Lemmatize the words
	* `>>> nltk.WordNetLemmatizer()`
* Segment the text into sentences
	* `>>> sentences = nltk.sent_tokenize(document)`
* Tokenize the text
	* `>>> sentences =  [nltk.word_tokenize(sent) for sent in sentences]`
* Tag the parts of speech
	* `>>> sentences = [nltk.pos_tag(sent) for sent in sentences]`
* Perform entity dection
* Perform relation detection

### Train Algorithm Using Training Examples

### Test Algorithm Using Testing Examples

### Create Decision Tree

## Inference Engine

# Take 2
* Information Retrieval
	* Question Answering
	* Information Extraction
		* Cascade Finite-state Transducer
			* Tokenization
			* Complex word handling
			* Basic group handling
			* Complex phrase handling
			* Structure merging
		* Probabilistic models
			* Hidden Markov Model