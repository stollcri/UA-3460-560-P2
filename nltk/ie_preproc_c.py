#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, csv, re

def remove_cruft(text_line):
	"""Remove uselesss text using dynamic selection with regular expresions
	text_line -- a string representing the line
	"""
	text_cleaned = text_line + ' '
	# Useless courtesies
	text_cleaned = re.sub('(H|h)ello Mr( |. |s. )[a-zA-Z]{2,16}[,]', '', text_cleaned)
	text_cleaned = re.sub('(Hello|Hi) [a-zA-Z]+,', '', text_cleaned)
	text_cleaned = re.sub('( If)[ .,a-zA-Z]+(Please)[ a-zA-Z,]+?(1188)[.]', '', text_cleaned)
	text_cleaned = re.sub('(Please)[ .,a-zA-Z]+?(1188)[.]', '', text_cleaned)
	text_cleaned = re.sub('(Note: This E-mail).+?\.', '', text_cleaned)
	text_cleaned = re.sub('(If).{4,16}?(email).+?(error).+?(\.)', '', text_cleaned)
	text_cleaned = re.sub('( |^)((incoming|Incoming).{1,8}?){0,1}((mail|Mail).{1,8}?){0,1}(user|User).{1,16}?(called).{1,32}?(ticket.)', '', text_cleaned)
	text_cleaned = re.sub('( |^)(can|Can|please|Please).{1,16}?(check|look|status).{1,16}?(this|ticket)[\.\?]', '', text_cleaned)
	text_cleaned = re.sub('( |^)(sent|Sent).{1,16}?(email|Email).{1,8}?(call).{1,8}?(\.)', '', text_cleaned)
	text_cleaned = re.sub('( |^)(called|Called|left|Left).{1,16}?(vm|VM).{1,16}?(back)(\.)', '', text_cleaned)
	text_cleaned = re.sub('(user|User).{1,16}?(called).{1,32}?(ticket)', '', text_cleaned)
	text_cleaned = re.sub('( |^)(closing|Closing).{1,8}?(ticket)(\.)', '', text_cleaned)
	text_cleaned = re.sub('(i |I |and ){0,1}(l|L)(eft).{1,16}?(vmail|vm|v.m.)(\.){0,1}', '', text_cleaned)
	text_cleaned = re.sub('(i|I){0,1}.{1,2}?(c|C)(alled).{1,16}?(user)(\.){0,1}', '', text_cleaned)
	text_cleaned = re.sub('(problem).{1,16}?(resolved)(\.){0,1}', '', text_cleaned)
	# this just anoys me, I'll pick for you
	text_cleaned = re.sub('and/or', 'or', text_cleaned)
	# datetime
	text_cleaned = re.sub('[ ]([0-9]{1,2})[-/.]([0-9]{1,2})[-/.]([0-9]{4})([ -:]{1,5})([0-9]{1,2})[:]([0-9]{1,2})[ :]([0-9]{0,2})[ ]{0,2}((AM|PM|am|pm){0,1})', '', text_cleaned)
	text_cleaned = re.sub('[ ]([0-9]{4})[-.]([0-9]{1,2})[-.]([0-9]{1,2})([ -:]{1,5})([0-9]{1,2})[:]([0-9]{1,2})[ :]([0-9]{0,2})[ ]{0,2}((AM|PM|am|pm){0,1})', '', text_cleaned)
	# date
	text_cleaned = re.sub('[ ]([0-9]{1,2})[-/.]([0-9]{1,2})[-/.]([0-9]{4})[ ]', '', text_cleaned)
	text_cleaned = re.sub('[ ]([0-9]{4})[-.]([0-9]{1,2})[-.]([0-9]{1,2})[ ]', '', text_cleaned)
	text_cleaned = re.sub('[ ]([0-9]{1,2})[-./]([0-9]{1,2})[-./]([0-9]{1,2})[ ]', '', text_cleaned)
	text_cleaned = re.sub('([0-9]{4,})[-.]([0-9]{1,2})[-.]([0-9]{1,2}).{3,10}?(EDT|EST)', '', text_cleaned)
	text_cleaned = re.sub('[0-9]{1,2}[\-][janfebmrpyulgsoctvd]{3,}[\-][0-9]{2,4}', '', text_cleaned)
	text_cleaned = re.sub('(^| )[0-9]{1,2}[janfebmrpyulgsoctvd]{3,3}[0-9]{2,4}', '', text_cleaned)
	# time
	text_cleaned = re.sub('[ ]([0-9]{1,2})[:]([0-9]{1,2})[ :]([0-9]{1,2})[ ]', '', text_cleaned)
	text_cleaned = re.sub('\(SelfService\)[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}[ ]PM[ :]', '', text_cleaned)
	# email
	text_cleaned = re.sub('(Sent|Enviado el|Gesendet):.+?(am|AM|a.m.|A.M.|pm|PM|p.m.|P.M.)[ ]', '', text_cleaned)
	text_cleaned = re.sub('(To|Cc|De|An):.{0,40}([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})', '', text_cleaned)
	text_cleaned = re.sub('(mailto|To|Cc|De|An):.{0,40}(.com)[ ;]', '', text_cleaned)
	text_cleaned = re.sub('(From|Para|Von):.{0,80}([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})[ ]', '', text_cleaned)
	text_cleaned = re.sub('(From|Para|Von|Email):.{0,40}(.com)[ ]', '', text_cleaned)
	text_cleaned = re.sub('(Subject|Asunto|Betreff):[ ]', '', text_cleaned)
	text_cleaned = re.sub('(RE|WG|FW): Service Desk Ticket #[0-9]{6,8}# (closed|opened)', '', text_cleaned)
	text_cleaned = re.sub('(Email|email|E-Mail|E-mail|e-mail):[ ][a-zA-Z]{2,20}@[a-zA-Z]{3,20}.com', '', text_cleaned)
	text_cleaned = re.sub('\[mailto:.+?\]', '', text_cleaned)
	text_cleaned = re.sub('<.+?(>){1,2}', '', text_cleaned)
	text_cleaned = re.sub('[a-zA-Z.]{3,32}@[a-zA-Z]{2,12}[.com|.org|.net|.de]', '', text_cleaned)
	# Phone numbers
	text_cleaned = re.sub('(Phone|Mobile|Work|Home|Telephone|Tele|Tel|Fax):[ ][ .\-\(\)0-9]+[ ]', '', text_cleaned)
	text_cleaned = re.sub('(Phone|phone|Cell|cell)[ #+1]{1,3}:[ ]{0,}(nnn-nnnn|ext.[0-9]{3,6})', '', text_cleaned)
	text_cleaned = re.sub('(Tel|tel|Fax|fax)[.:] \+[0-9\(\)]{8,10}[ .-][0-9]{4,}', '', text_cleaned)
	text_cleaned = re.sub('[\(][0-9\-]{3,16}[\)][0-9]{3,}[\-][0-9]{4,}', '', text_cleaned)
	text_cleaned = re.sub('[0-9\(\)]{5,}[ ][0-9]{3,}[.-][0-9]{4,}', '', text_cleaned)
	text_cleaned = re.sub('[0-9]{3,}[.-][0-9]{3,}[.-][0-9]{4,}', '', text_cleaned)
	text_cleaned = re.sub('\+[0-9\(\)]{9,}[.-][0-9]{4,}', '', text_cleaned)
	text_cleaned = re.sub('(x|X)[0-9]{3,5}', '', text_cleaned)
	# Department codes
	text_cleaned = re.sub('([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})', '', text_cleaned)
	# User names
	text_cleaned = re.sub('(Username): [a-zA-Z]+?[ ]', '', text_cleaned)
	# Computer names
	text_cleaned = re.sub('(PC Name): [a-zA-Z]+?[ ]', '', text_cleaned)
	text_cleaned = re.sub('[pPsSuUxX]{1,2}[0-9]{6,8}', '', text_cleaned)
	# MAC addresses
	text_cleaned = re.sub('[0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}', ' ', text_cleaned)
	text_cleaned = re.sub('[ ][0-9a-f]{12,12}', ' ', text_cleaned)
	# IP addresses
	text_cleaned = re.sub('[ ][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[ .,:\]]', ' ', text_cleaned)
	text_cleaned = re.sub('[ ][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}$', ' ', text_cleaned)
	# Web addresses
	text_cleaned = re.sub('(http|HTTP|www).{3,64}(\.jsp\?).{3,128}( )', '', text_cleaned)
	text_cleaned = re.sub('(http|HTTP|www).{3,32}(\.com|\.de|\.org)[^ ]{2,64}( )', '', text_cleaned)
	text_cleaned = re.sub('(http|HTTP|www).{3,32}(\.com|\.de|\.org|\.html)', '', text_cleaned)
	# UNC addresses
	text_cleaned = re.sub('(\\\\).+?(.com).+?( )', '', text_cleaned)
	text_cleaned = re.sub('(\\\\).+?[ ]', '', text_cleaned)
	# Useless jargon
	text_cleaned = re.sub('(ISA\*00\*).+?(IEA\*1\*[0-9]+)', '', text_cleaned)
	text_cleaned = re.sub('(ISA\^00\^).+?(IEA\^1\^[0-9]+)', '', text_cleaned)
	# Ticket reference numbers
	text_cleaned = re.sub('(\*ref#)[0-9]{1,4}-[0-9]{5,9}', '', text_cleaned)
	text_cleaned = re.sub('(ref#)[0-9]{1,4}-[0-9]{5,9}', '', text_cleaned)
	text_cleaned = re.sub('(tkt#|TKT#)[ .,0-9]{0,9}', '', text_cleaned)
	text_cleaned = re.sub('(tkt|TKT)[ .,]{0,1}', '', text_cleaned)
	# general cruft (remnants from other rules)
	text_cleaned = re.sub('(Email).{2,4}(Self Service).{2,3}[ ]', ' ', text_cleaned)
	text_cleaned = re.sub('[\(][a-zA-Z0-9\-]{3,16}[\)]', '', text_cleaned)
	text_cleaned = re.sub('(SY-)[A-Z]{3,6}(.)', ' ', text_cleaned)
	text_cleaned = re.sub('(Done\.)', '', text_cleaned)
	# compress dashes/dots
	text_cleaned = re.sub('[!?]{2,}', '.', text_cleaned)
	text_cleaned = re.sub('[-]{2,}', '-', text_cleaned)
	text_cleaned = re.sub('[.]{2,}', '.', text_cleaned)
	# remove numbers : TODO : TOO DRASTIC?
	text_cleaned = re.sub('[ ][0-9#,\-\.\(\)]+[ ]', ' ', text_cleaned)
	text_cleaned = re.sub('[0-9\-\(\)]+', '', text_cleaned)
	# remove punctutions
	text_cleaned = re.sub('( )[`~!@#$%^&\*\(\)\-_\+=\[\]\{\}:;,\.\?/]+( )', ' ', text_cleaned)
	text_cleaned = re.sub('[\*]+[a-zA-Z]+[\*]+', ' ', text_cleaned)
	text_cleaned = re.sub('[ ]["\'\.\?!\-]', ' ', text_cleaned)
	text_cleaned = re.sub('["\'][ ]', ' ', text_cleaned)
	text_cleaned = re.sub('[^a-zA-Z0-9 &\-\.\,\'"\?!<>]+', ' ', text_cleaned)
	#text_cleaned = re.sub('[\*=/]{2,}', ' ', text_cleaned)
	#text_cleaned = re.sub('[:;_]+', ' ', text_cleaned)
	#text_cleaned = re.sub('(=-)+', ' ', text_cleaned)
	#text_cleaned = re.sub('( =)+', ' ', text_cleaned)
	#text_cleaned = re.sub('( -)+', ' ', text_cleaned)
	# remove remaining punctuation : TODO : TOO DRASTIC?
	text_cleaned = re.sub('[`~@#$%^\*\(\)\-_\+=\[\]\{\}:;/]+', ' ', text_cleaned)
	# Leading special characters
	text_cleaned = re.sub('^[^a-z^A-Z^0-9]+', '', text_cleaned)
	# compress spaces
	text_cleaned = re.sub('[ ]{2,}', ' ', text_cleaned)
	return text_cleaned

def remove_description(user_info, description):
	"""If the description is repeated in the user info the delete it
	user_info -- the user info string
	description -- the description string
	"""
	return user_info.replace(description, '')

def remove_solution(user_info, solution):
	"""If the solution is repeated in the user info the delete it
	user_info -- the user info string
	solution -- the solution string
	"""
	return user_info.replace(solution, '')

def process_file():
	csv_reader = csv.reader(fileinput.input(), delimiter=',', quotechar='"')
	for csv_row in csv_reader:
		if (len(csv_row) >= 6):
			csv_cleaned = ['', '', '', '', '', '']
			csv_cleaned[0] = csv_row[0]
			csv_cleaned[1] = remove_cruft(csv_row[1])
			csv_cleaned[2] = remove_cruft(csv_row[2])
			csv_cleaned[3] = remove_cruft(csv_row[3])
			csv_cleaned[4] = csv_row[4]
			csv_cleaned[5] = csv_row[5]
			
			# remove incident description from user info column, if matched
			csv_cleaned[1] = remove_description(csv_cleaned[1], csv_cleaned[2])
			# remove incident solution form the user info column, if matched
			csv_cleaned[1] = remove_solution(csv_cleaned[1], csv_cleaned[3])

			# only description is filled in
			if (csv_cleaned[1] == '') and (csv_cleaned[3] == ''):
				continue
			else:
				# no solution is provided
				if (csv_cleaned[3] == ''):
					continue
				else:
					# too long (1024 is arbitrary, need emperical analysis)
					if (len(csv_cleaned[1]) > 1024):
						continue
					else:
						csv_output = csv.writer(sys.stdout, delimiter=',', 
							quotechar='"', quoting=csv.QUOTE_MINIMAL)
						csv_output.writerow(csv_cleaned)

if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: preprocess.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()
