#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, re

def remove_useless_texts(text_line):
	useless_texts = [
		"disclaimer this electronic message transmission contains information that may be confidential or privileged.",
		"if you require further assistance, please contact the Company group it service desk.",
		"the information is intended to be for the use of the individual or entity named above.",
		"para ayuda técnico por favor comunicase con el Company group it service desk:",
		"copying, distribution, or use of the contents of this information is prohibited.",
		"Company group north america email - support-email@Company.com",
		"it-service-desk Company group e-mail: it-support-sg@Company.com",
		"if you are not the intended recipient, be aware that any disclosure,",
		"please notify the sender and delete the material from any computer.",
		"hank you for using the Company Group IT Service Desk. Phone or ",
		"origincl message from: support-email@Company.com",
		"thank you for contacting the north america service desk. regards,",
		"if you have received this electronic transmission in error,",
		"Please review the attached report and CORRECT IMMEDIATELY.",
		"we have tried to contact you regarding your issue/request.",
		"thank you for using the Company group it service desk.",
		"thank you for contacting the north america service desk.",
		"gracias por usar el Company group it service desk.",
		"if you still need assitance, please call at ext 1411.",
		"mit freundlichen grüßen / best regards / saludos",
		"mailto:support-email@Company.com",
		"saludos, - Company service desk, norteamérica",
		"self service - http://tsduss.de.inc.com/magicsshd",
		"from: support-email@Company.com",
		"para: it-support Company-group north america",
		"si sigue teniedo problemas, por favor llamenos.",
		"best regards technische it - Company group ",
		"hola, hemos tratado de comunicarnos con usted.",
		"to: it-support Company-group north america",
		"can you please have him call us at ext 1411.",
		"director: north america technology services",
		"do you know if this has been taken care of?",
		"it support Company group north america",
		"it-support Company-group north america",
		"Company it service desk, north america",
		"thank you, Company it service desk, na",
		"on behalf of it-support Company-group",
		"e-mail: support-email@Company.com",
		"email - support-email@Company.com",
		"email ? support-email@Company.com",
		"email: support-email@Company.com",
		"Company service desk, norteamérica",
		"Company service desk, north america",
		"sent email to get an update on ticket.",
		"thank you, it support Company group",
		"thank you, Company it service desk,",
		"sent email to get update on ticket.",
		"thanks, it support Company group",
		"extension or toll free Self Service",
		"it-service-desk Company group",
		"your request has been completed.",
		"an: it-support Company-group",
		"support-email@Company.com",
		"su petición a sido completado.",
		"to: it-support Company-group",
		"it operations Company group",
		"we will now close this ticket.",
		"sent email to get an update.",
		"it support Company group",
		"thank you, rodrigo melendez",
		"-----origincl message-----",
		"na-editeam@Company.com;",
		"please ignore this ticket.",
		"Company it service desk",
		"sorry for the late answer,",
		"# == == == == == == == ==",
		"category: userselfservice",
		"i called user, no answer.",
		"mit freundlichen gruessen",
		"mit freundlichen grüßen",
		"Company group usa inc.",
		"sent email to get update.",
		"sorry for the delay again",
		"sorry for the late answer",
		"http://www.Company.us",
		"Company north america",
		"email – self service – ",
		"not able to reach user.",
		"Company service desk",
		"urgent!!! please rush!!",
		"troubleshooting data: ",
		"it-support, inc india",
		"it/client coordinctor",
		"thank you, it support",
		"please call me back.",
		"Company group usa",
		"your ticket request.",
		"email – self service",
		"sorry for the delay",
		"dear madam or sir,",
		"thanks, it support",
		"with best regards,",
		"email –  phone – ",
		"closing this tkt.",
		"problem resolved.",
		"please try again."
		"importance: high",
		"thanks. regards,",
		"wooster,oh 44691",
		"urgent request!",
		"urgent request.",
		"email –  phone",
		"good morning,",
		"mailing user.",
		"please rush!!",
		"user informed",
		"best regards",
		"i left a vm.",
		"it-support,",
		"co usa llc",
		"www.co.com",
		"it support",
		"it-support",
		"thank you,",
		"thank you!",
		"thank you.",
		"thank you ",
		"all ok now",
		"dear sir,",
		"thank you",
		"gracias,",
		"regards,",
		"regards.",
		"regards",
		"hello, ",
		"sorry, ",
		"thanks,",
		"thanks!",
		"thanks.",
		"pleased",
		"please",
		"urgent"
	]
	
	text_cleaned = text_line
	for useless_text in useless_texts:
		text_cleaned = re.sub(re.compile(useless_text, re.IGNORECASE), '', text_cleaned)
	return text_cleaned


def remove_usernames(text_line):
	usernames = [
		"username",
		"user",
	]
	
	text_cleaned = text_line
	for username in usernames:
		text_cleaned = re.sub(re.compile(username, re.IGNORECASE), '', text_cleaned)
	return text_cleaned


def remove_cruft(text_line):
	text_cleaned = text_line
	text_cleaned = remove_useless_texts(text_cleaned)
	text_cleaned = remove_usernames(text_cleaned)
	return text_cleaned

def process_file():
	for text_line in fileinput.input():
		text_cleaned = remove_cruft(text_line)
		if text_cleaned:
			print text_cleaned,


if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: prepreprocess.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()
