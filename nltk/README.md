# Install NLTK
These are my version of the instructions locatate at [the NLTK site](http://nltk.org/install.html).

## Install setuptools for Python
First download the right version of setuptools for your version of Python, then install them. You can find your version of Python by typing `python -V`, the downloads are located here: [http://pypi.python.org/pypi/setuptools](http://pypi.python.org/pypi/setuptools)

	sudo sh setuptools-0.6c11-py2.7.egg

## Install Pip

	sudo easy_install pip

## Install Numpy

	sudo pip install -U numpy

## Install PyYAML and NLTK

	sudo pip install -U pyyaml nltk

## Test

	python
	>>> import nltk

## Download NLTK data

	python
	>>> nltk.download()

# Use NTLK for your data

## Load custom corpus
(As seen[http://nltk.googlecode.com/svn/trunk/doc/book/ch02.html](http://nltk.googlecode.com/svn/trunk/doc/book/ch02.html))

	python
	>>> from nltk.corpus import PlaintextCorpusReader
	>>> corpus_root = '/usr/share/dict'
	>>> wordlists = PlaintextCorpusReader(corpus_root, '.*')
	>>> wordlists.fileids()



# Install scikit-learn

	sudo pip install -U scikit-learn