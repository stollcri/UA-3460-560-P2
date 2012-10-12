#!/usr/bin/env python

import sys, re

def usefull_line(text_line):
	discard_markers = [
		"SAP password reset was processed automatically.",
		"Hello, the reset for your desired system was processed.",
		"Hello, the reset for your desired SAP-system was processed."
	]
	for discard_marker in discard_markers:
		if discard_marker in text_line:
			return false
	return true

def remove_static_cruft(text_line):
	useless_texts = [
		"Thank you for using the Schaeffler Group IT Service Desk.",
		"Your request has been completed.",
		"If you require further assistance, please contact the Schaeffler Group IT Service Desk.",
		"Thanks, IT Support",
		"Schaeffler Group North America Email - it-support-sg-na@schaeffler.com",
		"Phone - (888) 462-6811 or X1188",
		"Self Service - http://tsd.de.ina.com/magicsshd"
	]
	text_cleaned = text_line
	for useless_text in useless_texts:
		text_cleaned = re.sub(useless_text, '', text_cleaned)
	return text_cleaned

def remove_regexp_cruft(text_line):
	text_cleaned = text_line
	text_cleaned = re.sub('\*\*\*\*+', '', text_cleaned)
	text_cleaned = re.sub('----+', '', text_cleaned)
	text_cleaned = re.sub('____+', '', text_cleaned)
	text_cleaned = re.sub('-=-=+', '', text_cleaned)
	text_cleaned = re.sub('(-=- -=- )+', '', text_cleaned)
	return text_cleaned

def remove_cruft(text_line):
	remove_static_cruft(text_line)
	remove_regexp_cruft(text_line)

def process_file(file_name):
	with open(file_name) as text_file:
		for text_line in text_file:
			if usefull_line(text_line):
				text_cleaned = remove_cruft(text_line)