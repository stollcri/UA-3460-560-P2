#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, csv, re

def usefull_line(csv_cols):
	"""Determine if a csv row is usefull.
	csv_cols -- a list of columns
	"""
	discard_markers = [
		"Automatically generated ticket by SAP Global Technical Application Development",
		"This ticket has been created automatically",
		"SAP password reset was processed automatically.",
		"The required SAP authorizations have been requested for you.",
		"The required SAP authorization (",
		"Hello, the reset for your desired system was processed.",
		"Hello, the reset for your desired SAP-system was processed.",
		"Hallo, ihr Kennwort wurde grundgestellt.",
		"Hello, the validity date for your desired SAP-system was extended.",
		"Your requested new user account(s) have been created.",
		"the requested permissions for file access have been given.",
		"Request for file access was approved by data owner",
		"Antrag wurde vom Verzeichnisowner genehmigt.",
		"your file access request was rejected",
		"Request was approved by data owner!",
		"extend the validity date",
		"no response from user closing ticket",
		"the following malware components were found",
		"Ticket closed automatically",
		"Please close this ticket.",
		"PASSWORTER",
		"Passworter",
		"passworter",
		"PASSWORT",
		"Passwort",
		"passwort",
		"PASSWORD",
		"Password",
		"password",
		"pwd:",
		"Confidential",
		"confidential",
		"Social Security",
		"Social security",
		"social security",
		"Birth Date",
		"Birth date",
		"birth date",
		"Birthday",
		"birthday",
		"SSN"
	]
	for discard_marker in discard_markers:
		for csv_col in csv_cols:
			if discard_marker in csv_col:
				return False
	return True

def remove_static_cruft(text_line):
	"""Remove know useless text from the line.
	text_line -- a string representing the line
	"""
	useless_texts = [
		"DISCLAIMER This electronic message transmission contains information that may be confidential or privileged.",
		"The information is intended to be for the use of the individual or entity named above.",
		"If you are not the intended recipient, be aware that any disclosure,",
		"copying, distribution, or use of the contents of this information is prohibited.",
		"If you have received this electronic transmission in error,",
		"please notify the sender and delete the material from any computer.",
		"Para ayuda técnico por favor comunicase con el Schaeffler Group IT Service Desk:",
		"Thank you for using the Schaeffler Group IT Service Desk.",
		"Gracias por usar el Schaeffler Group IT Service Desk.",
		"Your request has been completed.",
		"Su petición a sido completado.",
		"Original Message From: IT-Support-Schaeffler-Group@schaeffler.com",
		"[mailto:IT-Support-Schaeffler-Group@schaeffler.com]",
		"To: IT-Support Schaeffler-Group North America",
		"Para: IT-Support Schaeffler-Group North America",
		"From: IT-Support-Schaeffler-Group@schaeffler.com",
		"An: IT-Support Schaeffler-Group",
		"To: IT-Support Schaeffler-Group",
		"If you still need assitance, please call at ext 1188.",
		"If you require further assistance, please contact the Schaeffler Group IT Service Desk.",
		"IT Support Schaeffler Group North America Email - it-support-sg-na@schaeffler.com",
		"Schaeffler Group North America Email - it-support-sg-na@schaeffler.com",
		"IT-Service-Desk Schaeffler Group e-mail: it-support-sg@schaeffler.com",
		"Can you please have him call us at ext 1188.",
		"E-mail: it-support-sg-na@schaeffler.com",
		"Email: it-support-sg-na@schaeffler.com",
		"Email ? it-support-sg-na@schaeffler.com",
		"Email - it-support-sg-na@schaeffler.com",
		"it-support-sg-na@schaeffler.com",
		"Email – Self Service – ",
		"Phone - (888) 462-6811 or X1188",
		"Phone - (888) 462-6811 or x1188",
		"Phone Ext: x1188 or 1-888-462-6811",
		"Por Teléfono: x1188 o 001-888-462-6811",
		"Phone – extension 1188 or toll free (888) 462-6811",
		"Phone: extension 1188 or toll free (888) 462-6811",
		"Phone ? (888) 462-6811",
		"x1188 or 1-888-462-6811",
		"x1188 o 001-888-462-6811",
		"telephone: 01805 22 33 11",
		"Self Service - http://tsd.de.ina.com/magicsshd",
		"Self Service: http://tsd.de.ina.com/magicsshd",
		"Self Service ? http://tsd.de.ina.com/magicsshd",
		"Self Service - http://tsduss.de.ina.com/magicsshd",
		"http://tsd.de.ina.com/magicsshd",
		"On Behalf Of IT-Support Schaeffler-Group",
		"IT Support Schaeffler Group North America",
		"IT-Support Schaeffler-Group North America",
		"Schaeffler Service Desk, North America",
		"Saludos, - Schaeffler Service Desk, Norteamérica",
		"Thank you, Schaeffler IT Service Desk, NA",
		"Thank you, IT Support Schaeffler Group",
		"Thank you, Schaeffler IT Service Desk,",
		"Thank you, IT Support",
		"Thanks, IT Support Schaeffler Group",
		"Thanks, IT Support",
		"Schaeffler IT Service Desk, North America",
		"Schaeffler Service Desk, Norteamérica",
		"IT-Service-Desk Schaeffler Group",
		"IT Operations Schaeffler Group",
		"IT Support Schaeffler Group",
		"Schaeffler IT Service Desk",
		"Schaeffler Service Desk",
		"IT-Support, INA India",
		"IT-Support,",
		"IT-Support",
		"IT Support",
		"NA-EDITeam@schaeffler.com;",
		"LuK USA LLC 3401 Old Airport Road Wooster, OH 44691",
		"LuK USA LLC 3401 Old Airport Road Wooster, Ohio 44691",
		"LuK USA LLC 3401 Old Airport Road",
		"3401 Old Airport Road Wooster, Ohio 44691",
		"3401 Old Airport Rd. Wooster, Ohio 44691",
		"3401 Old Airport Road, Wooster, OH 44691",
		"3401 Old Airport Road Wooster, OH 44691",
		"3401 Old Airport Rd. Wooster, OH 44691",
		"3401 Old Airport Rd. Wooster,OH 44691",
		"3401 Old Airport Road Wooster",
		"308 Springhill Farm Road Fort Mill, SC 29715",
		"308 Springhill Farm Rd. Fort Mill, SC 29715",
		"308 Springhill Farm Road",
		"308 Springhill Farm Rd.",
		"Fort Mill, SC 29715",
		"Director: North America Technology Services",
		"Email address: Tom.Miller@schaeffler.com www.lukusa.com",
		"Email address: Tom.Miller@schaeffler.com",
		"-----Original Message-----",
		"# == == == == == == == ==",
		"Category: USERSELFSERVICE",
		"Dear Madam or Sir,",
		"Good morning,",
		"Dear Sir,",
		"Hello, ",
		"Importance: High",
		"URGENT!!! PLEASE RUSH!!",
		"URGENT request!",
		"Urgent request.",
		"URGENT!!",
		"PLEASE RUSH!!",
		"Sorry, ",
		"Your ticket request.",
		"sorry for the late answer,",
		"sorry for the late answer",
		", sorry for the delay again",
		"sorry for the delay again",
		"sorry for the delay",
		"Please call me back.",
		"We have tried to contact you regarding your issue/request.",
		"Hola, Hemos tratado de comunicarnos con usted.",
		"Si Sigue teniedo problemas, por favor llamenos.",
		"We will now close this ticket.",
		"Troubleshooting Data: ",
		"Closing this tkt.",
		"Please close ticket.",
		"Best regards Technische IT - Schaeffler Group ",
		"Mit freundlichen Grüßen / Best regards / Saludos",
		"Mit freundlichen Gruessen",
		"Mit freundlichen Grüßen",
		"Thank you, Rodrigo Melendez",
		"With best regards,",
		"Thanks. Regards,",
		"Best regards,",
		"Best Regards,",
		"Best regards",
		"Best Regards",
		"Thank you, -",
		"Thank You,",
		"Thank you,",
		"Thank You.",
		"Thank You!",
		"Thank you.",
		"Thank you!",
		"Gracias,",
		"Regards,",
		"Regards.",
		"Regards",
		"Thanks,",
		"Thanks.",
		"Thanks!",
		"Thank you for contacting the North America service desk. Regards,",
		"Thank you for contacting the North America service desk.",
		"If you require further assistance, please contact the Schaeffler Group IT Service Desk",
		"Email – Self Service – ",
		"Email – Self Service",
		"Email –  Phone – ",
		"Email –  Phone",
		"IT/Client Coordinator",
		"Schaeffler Group USA Inc.",
		"Schaeffler North America",
		"Schaeffler Group USA",
		"http://www.schaeffler.us",
		"LuK USA LLC",
		"www.luk.com",
		", Norteamérica"
	]

	# this list should be filled automatically instead of manually
	usernames = [
		"Preston Swasey",
		"Christopher Stoll",
		"--melenrdr",
		"duffydni",
		"DUFFYDNI",
		"stollcri",
		"STOLLCRI",
		"romerira",
		"ROMERIRA",
		"Romerira",
		"Incein",
		"incein",
		"INCEIN",
		"iNCEIN",
		"swasepes",
		"SWASEPES",
		"Aguilgor",
		"mesenmch",
		"blunddbo",
		"Dziateic",
		"dZIATEIC",
		"crosbjse",
		"goochdug",
		"striefor",
		"(stoufsot)",
		"stoufsot",
		"whitecai",
		"MARAMGRI",
		"kandamru",
		"wrighwyn",
		"CRUZJOS",
		"ROSEAAM",
		"sousaeme",
		"royerdug",
		"yorictev",
		"Mabrybyc",
		"hasleguy",
		"OHRCRS",
		"WALKEMCH",
		"weigojmi",
		"vivalrca",
		"elmormke",
		"clawsjhn@",
		"clawsjhn",
		"WEBERTIA",
		"WORSFVRG"
	]
	
	text_cleaned = text_line
	for useless_text in useless_texts:
		text_cleaned = text_cleaned.replace(useless_text, '')
	for username in usernames:
		text_cleaned = text_cleaned.replace(username, '')
	return text_cleaned

def remove_regexp_cruft(text_line):
	"""Remove uselesss text using dynamic selection with regular expresions
	text_line -- a string representing the line
	"""
	text_cleaned = text_line
	# datetime: nn-nn-nnnn -or- nn/nn/nnnn -or- nn.nn.nnnn -and- nn:nn nn -or- nn:nn:nn
	text_cleaned = re.sub('[ ]([0-9]{1,2})[-/.]([0-9]{1,2})[-/.]([0-9]{4})[ ]([0-9]{1,2})[:]([0-9]{1,2})[ ]', '', text_cleaned)
	# datetime: nnnn-nn-nn -or- nnnn.nn.nn -and- nn:nn nn -or- nn:nn:nn
	text_cleaned = re.sub('[ ]([0-9]{4})[-.]([0-9]{1,2})[-.]([0-9]{1,2})[ ]([0-9]{1,2})[:]([0-9]{1,2})[ ]', '', text_cleaned)
	# date: nn-nn-nnnn -or- nn/nn/nnnn -or- nn.nn.nnnn
	text_cleaned = re.sub('[ ]([0-9]{1,2})[-/.]([0-9]{1,2})[-/.]([0-9]{4})[ ]', '', text_cleaned)
	# date: nnnn-nn-nn -or- nnnn.nn.nn
	text_cleaned = re.sub('[ ]([0-9]{4})[-.]([0-9]{1,2})[-.]([0-9]{1,2})[ ]', '', text_cleaned)
	# time: nn:nn nn -or- nn:nn:nn
	text_cleaned = re.sub('[ ]([0-9]{1,2})[:]([0-9]{1,2})[ :]([0-9]{1,2})[ ]', '', text_cleaned)
	# time: after self service update
	text_cleaned = re.sub('\(SelfService\)[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}[ ]PM[ :]', '', text_cleaned)
	# date: nnnn-nn-nn -or- nnnn.nn.nn
	text_cleaned = re.sub('([0-9]{4,})[-.]([0-9]{1,2})[-.]([0-9]{1,2}).{3,10}?(EDT|EST)', '', text_cleaned)
	# email sent string
	text_cleaned = re.sub('(Sent|Enviado el|Gesendet):.+?(am|AM|a.m.|A.M.|pm|PM|p.m.|P.M.)[ ]', '', text_cleaned)
	# email to string
	text_cleaned = re.sub('(To|Cc|De|An):.{0,80}([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})[ ]', '', text_cleaned)
	# email to string
	text_cleaned = re.sub('(To|Cc|De|An):.{0,40}([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})', '', text_cleaned)
	# email from string
	text_cleaned = re.sub('(mailto|To|Cc|De|An):.{0,40}(.com)[ ;]', '', text_cleaned)
	# email from string
	text_cleaned = re.sub('(From|Para|Von):.{0,80}([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})[ ]', '', text_cleaned)
	# email from string
	text_cleaned = re.sub('(From|Para|Von|Email):.{0,40}(.com)[ ]', '', text_cleaned)
	# email subject string
	text_cleaned = re.sub('(Subject|Asunto|Betreff):[ ]', '', text_cleaned)
	# email subject string
	text_cleaned = re.sub('(RE|WG|FW): Service Desk Ticket #[0-9]{6,8}# (closed|opened)', '', text_cleaned)
	# email signatures
	text_cleaned = re.sub('(Phone|Mobile|Work|Home|Telephone|Tele|Tel|Fax):[ ][ .\-\(\)0-9]+[ ]', '', text_cleaned)
	# email signatures
	text_cleaned = re.sub('(Email|email|E-Mail|E-mail|e-mail):[ ][a-zA-Z]{2,20}@[a-zA-Z]{3,20}.com', '', text_cleaned)
	# mailto statements
	text_cleaned = re.sub('\[mailto:.+?\]', '', text_cleaned)
	# four or more hashes
	text_cleaned = re.sub('####+', '', text_cleaned)
	# four or more asterisks
	text_cleaned = re.sub('\*\*\*\*+', '', text_cleaned)
	# four or more pluses
	text_cleaned = re.sub('\+\+\+\++', '', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub('(-=- -=- )+', '', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub('(-=- -=-)+', '', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub('(- - )+', '', text_cleaned)
	# four or more dashes
	text_cleaned = re.sub('----+', '', text_cleaned)
	# four or more equals
	text_cleaned = re.sub('====+', '', text_cleaned)
	# four or more underscores
	text_cleaned = re.sub('____+', '', text_cleaned)
	# four or more forward slashes
	text_cleaned = re.sub('////+', '', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub('-=-=+', '', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub('=-=-+', '', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub(' *  -- ', ' ', text_cleaned)
	# fancy text seperators
	text_cleaned = re.sub(' * - - ', ' ', text_cleaned)
	# PC names
	text_cleaned = re.sub('(PC Name): [a-zA-Z]+?[ ]', '', text_cleaned)
	# User names
	text_cleaned = re.sub('(Username): [a-zA-Z]+?[ ]', '', text_cleaned)
	# MAC addresses
	text_cleaned = re.sub('[0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}[:-][0-9a-fA-F]{2,}', 'a:b:c:d:e:f', text_cleaned)
	# IP addresses
	text_cleaned = re.sub('[ ][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[ .,:\]]', ' a.b.c.d ', text_cleaned)
	# IP addresses
	text_cleaned = re.sub('[ ][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}$', ' a.b.c.d ', text_cleaned)
	# Computer names
	text_cleaned = re.sub('[pPsSxX][0-9]{8,}', 'Pnnn', text_cleaned)
	# Phone numbers
	text_cleaned = re.sub('[0-9]{3,}[.-][0-9]{3,}[.-][0-9]{4,}', 'nnn-nnnn', text_cleaned)
	# Phone numbers
	text_cleaned = re.sub('[0-9\(\)]{5,}[ ][0-9]{3,}[.-][0-9]{4,}', 'nnn-nnnn', text_cleaned)
	# Phone numbers
	text_cleaned = re.sub('(Tel|tel|Fax|fax)[.:] \+[0-9\(\)]{8,10}[ .-][0-9]{4,}', '', text_cleaned)
	# Phone numbers
	text_cleaned = re.sub('\+[0-9\(\)]{9,}[.-][0-9]{4,}', 'nnn-nnnn', text_cleaned)
	# Phone numbers
	text_cleaned = re.sub('(Phone|phone|Cell|cell)[ #+1]{1,3}:[ ]{0,}(nnn-nnnn|ext.[0-9]{3,6})', '', text_cleaned)
	# Department codes
	text_cleaned = re.sub('([A-Z]{2,}/[A-Z]{3,}-[A-Z0-9]{1,5})[ ]', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('(H|h)ello (Mr.|Mrs.) [a-zA-Z]{2,16}[,]', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('(I tried to call)[ a-zA-Z,]+[.]?', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('( If)[ .,a-zA-Z]+(Please)[ a-zA-Z,]+?(1188)[.]', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('( If)[ .,a-zA-Z]+(please)[ a-zA-Z,]+?(1188)[.]', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('(Please)[ .,a-zA-Z]+?(1188)[.]', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('(Note: This E-mail).+?\.', '', text_cleaned)
	# Useless courtesies
	text_cleaned = re.sub('(We have tried to contact).+?\.', '', text_cleaned)
	# Useless jargon
	text_cleaned = re.sub('(ISA\*00\*).+?(IEA\*1\*[0-9]+)', '', text_cleaned)
	# Useless jargon
	text_cleaned = re.sub('(ISA\^00\^).+?(IEA\^1\^[0-9]+)', '', text_cleaned)
	# Ticket reference numbers
	text_cleaned = re.sub('(\*ref#)[0-9]{1,4}-[0-9]{5,9}', '', text_cleaned)
	# Ticket reference numbers
	text_cleaned = re.sub('(ref#)[0-9]{1,4}-[0-9]{5,9}', '', text_cleaned)
	# remove cruft
	text_cleaned = re.sub('(Email).{2,4}(Self Service).{2,3}[ ]', ' ', text_cleaned)
	# remove cruft
	text_cleaned = re.sub('(,EST )', ' ', text_cleaned)
	# Leading special characters
	text_cleaned = re.sub('^[^a-z^A-Z^0-9]+', '', text_cleaned)
	# compress dashes
	text_cleaned = re.sub('[-]{2,}', '-', text_cleaned)
	# compress dots
	text_cleaned = re.sub('[.]{2,}', '.', text_cleaned)
	# compress spaces
	text_cleaned = re.sub('[ ]{2,}', ' ', text_cleaned)
	# remove cruft
	text_cleaned = re.sub('( \* )', ' ', text_cleaned)
	# remove cruft
	text_cleaned = re.sub('( -)+', ' ', text_cleaned)
	# remove cruft
	text_cleaned = re.sub('[ ][.-][ ]', ' ', text_cleaned)
	return text_cleaned

def remove_cruft(text_line):
	"""Remove the various types of cruft from the line.
	text_line -- a string represning the line
	"""
	text_cleaned = remove_static_cruft(text_line)
	text_cleaned = remove_regexp_cruft(text_cleaned)
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

def process_file(file_name):
	with open(file_name) as csv_file:
		#csv_dialect = csv.Sniffer().sniff(csv_file.read(1024))
		#csv_file.seek(0)
		#csv_reader = csv.reader(csv_file, csv_dialect)
		csv_reader = csv.reader(csv_file, delimiter=',', quotechar='"')
		for csv_row in csv_reader:
			if usefull_line(csv_row):
				if (len(csv_row) >= 4):
					csv_cleaned = ['', '', '', '']
					csv_cleaned[0] = csv_row[0]
					csv_cleaned[1] = remove_cruft(csv_row[1])
					csv_cleaned[2] = remove_cruft(csv_row[2])
					csv_cleaned[3] = remove_cruft(csv_row[3])
					
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

								#print '['
								#print '<<< 0 >>> '+csv_cleaned[0]
								#print '<<< 1 >>> '+csv_cleaned[1]
								#print '<<< 2 >>> '+csv_cleaned[2]
								#print ']'
								#print

if __name__ == "__main__":
	if len(sys.argv) == 2:
		process_file(sys.argv[1])
	else:
		print "Syntax: preprocess.py filename.csv"
		print " prints processed file to stdout in csv format"