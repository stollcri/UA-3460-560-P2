# Automatic Helpdesk Problem Sover

## Inductive Learning Algorithm

### Create Corpus
Steps to process training/example files into a corpus

1. Remove bogus entries/lines
	* ones which were automatically handled (password resets)
2. Normalize the text
	* convert everything to lowwer case
	* remove dates and times
	* remove other know bogus strings
3. Tokenize the teext
	* nltk.word_tokenize(raw)
4. Stem the verbs
	* nltk.PorterStemmer()
5. Lemmatize the words
	* nltk.WordNetLemmatizer()

### Train Algorithm Using Training Examples

### Test Algorithm Using Testing Examples

### Create Decision Tree

## Inference Engine