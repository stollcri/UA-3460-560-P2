#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, re

def remove_useless_texts(text_line):
	useless_texts = [
		"disclaimer this electronic message transmission contains information that may be confidential or privileged.",
		"if you require further assistance, please contact the schaeffler group it service desk.",
		"the information is intended to be for the use of the individual or entity named above.",
		"para ayuda técnico por favor comunicase con el schaeffler group it service desk:",
		"copying, distribution, or use of the contents of this information is prohibited.",
		"schaeffler group north america email - it-support-sg-na@schaeffler.com",
		"it-service-desk schaeffler group e-mail: it-support-sg@schaeffler.com",
		"if you are not the intended recipient, be aware that any disclosure,",
		"please notify the sender and delete the material from any computer.",
		"original message from: it-support-schaeffler-group@schaeffler.com",
		"thank you for contacting the north america service desk. regards,",
		"if you have received this electronic transmission in error,",
		"Please review the attached report and CORRECT IMMEDIATELY.",
		"we have tried to contact you regarding your issue/request.",
		"thank you for using the schaeffler group it service desk.",
		"thank you for contacting the north america service desk.",
		"hank you for using the Schaeffler Group IT Service Desk. Phone or "
		"gracias por usar el schaeffler group it service desk.",
		"if you still need assitance, please call at ext 1188.",
		"phone – extension 1188 or toll free (888) 462-6811",
		"mit freundlichen grüßen / best regards / saludos",
		"mailto:it-support-schaeffler-group@schaeffler.com",
		"phone: extension 1188 or toll free (888) 462-6811",
		"saludos, - schaeffler service desk, norteamérica",
		"self service - http://tsduss.de.ina.com/magicsshd",
		"from: it-support-schaeffler-group@schaeffler.com",
		"para: it-support schaeffler-group north america",
		"si sigue teniedo problemas, por favor llamenos.",
		"best regards technische it - schaeffler group ",
		"hola, hemos tratado de comunicarnos con usted.",
		"self service - http://tsd.de.ina.com/magicsshd",
		"self service ? http://tsd.de.ina.com/magicsshd",
		"self service: http://tsd.de.ina.com/magicsshd",
		"to: it-support schaeffler-group north america",
		"can you please have him call us at ext 1188.",
		"director: north america technology services",
		"do you know if this has been taken care of?",
		"it support schaeffler group north america",
		"it-support schaeffler-group north america",
		"schaeffler it service desk, north america",
		"thank you, schaeffler it service desk, na",
		"email address: tom.miller@schaeffler.com",
		"on behalf of it-support schaeffler-group",
		"e-mail: it-support-sg-na@schaeffler.com",
		"email - it-support-sg-na@schaeffler.com",
		"email ? it-support-sg-na@schaeffler.com",
		"por teléfono: x1188 o 001-888-462-6811",
		"email: it-support-sg-na@schaeffler.com",
		"schaeffler service desk, norteamérica",
		"schaeffler service desk, north america",
		"sent email to get an update on ticket.",
		"thank you, it support schaeffler group",
		"thank you, schaeffler it service desk,",
		"sent email to get update on ticket.",
		"thanks, it support schaeffler group",
		"phone ext: x1188 or 1-888-462-6811",
		"it-service-desk schaeffler group",
		"your request has been completed.",
		"an: it-support schaeffler-group",
		"http://tsd.de.ina.com/magicsshd",
		"it-support-sg-na@schaeffler.com",
		"phone - (888) 462-6811 or x1188",
		"su petición a sido completado.",
		"to: it-support schaeffler-group",
		"it operations schaeffler group",
		"we will now close this ticket.",
		"sent email to get an update.",
		"email – self service – ",
		"it support schaeffler group",
		"thank you, rodrigo melendez",
		"-----original message-----",
		"na-editeam@schaeffler.com;",
		"please ignore this ticket.",
		"schaeffler it service desk",
		"sorry for the late answer,",
		"# == == == == == == == ==",
		"category: userselfservice",
		"i called user, no answer.",
		"mit freundlichen gruessen",
		"mit freundlichen grüßen",
		"schaeffler group usa inc.",
		"sent email to get update.",
		"sorry for the delay again",
		"sorry for the late answer",
		"telephone: 01805 22 33 11",
		"308 springhill farm road",
		"http://www.schaeffler.us",
		"schaeffler north america",
		"x1188 o 001-888-462-6811",
		"308 springhill farm rd.",
		"not able to reach user.",
		"schaeffler service desk",
		"urgent!!! please rush!!",
		"x1188 or 1-888-462-6811",
		"email – self service",
		"phone ? (888) 462-6811",
		"troubleshooting data: ",
		"3401 old airport road",
		"email –  phone – ",
		"it-support, ina india",
		"it/client coordinator",
		"thank you, it support",
		"3401 old airport rd.",
		"closing this ticket.",
		"please call me back.",
		"please close ticket.",
		"schaeffler group usa",
		"your ticket request.",
		"fort mill, sc 29715",
		"sorry for the delay",
		"wooster, ohio 44691",
		"dear madam or sir,",
		"thanks, it support",
		"with best regards,",
		"closing this tkt.",
		"problem resolved.",
		"wooster, oh 44691",
		"please try again."
		"email –  phone",
		"importance: high",
		"thanks. regards,",
		"wooster,oh 44691",
		"urgent request!",
		"urgent request.",
		"good morning,",
		"mailing user.",
		"please rush!!",
		"best regards",
		"i left a vm.",
		"it-support,",
		"luk usa llc",
		"www.luk.com",
		"it support",
		"it-support",
		"thank you,",
		"thank you!",
		"thank you.",
		"thank you ",
		"dear sir,",
		"thank you",
		"gracias,",
		"regards,",
		"regards.",
		"urgent!!",
		"hello, ",
		"regards",
		"sorry, ",
		"thanks,",
		"thanks!",
		"thanks."
	]
	
	text_cleaned = text_line
	for useless_text in useless_texts:
		text_cleaned = re.sub(re.compile(useless_text, re.IGNORECASE), '', text_cleaned)
	return text_cleaned


def remove_usernames(text_line):
	usernames = [
		"aguilgor",
		"betzeadr",
		"bezolabr",
		"blanksef",
		"blunddbo",
		"bhanupak",
		"bonczaex",
		"bosicmry",
		"burgetnj",
		"cartebrr",
		"chapmrob",
		"clawsjhn",
		"crosbjse",
		"cruzjos",
		"debleaan",
		"drexemke",
		"duffydni",
		"duffydni",
		"dziateic",
		"dziateic",
		"eckenjer",
		"eckenltz",
		"elmormke",
		"fellnpte",
		"floricrn",
		"frerimtt",
		"foerspet",
		"goochdug",
		"gopalvnu",
		"hampewlt",
		"hansecri",
		"harwerbe",
		"hasleguy",
		"heinkcri",
		"hoelzadr",
		"incein",
		"jamesbb",
		"kandamru",
		"keesapav",
		"khanaub",
		"klernmtt",
		"konaksin",
		"linnedbr",
		"loflilnn",
		"mabrybyc",
		"maramgri",
		"meindhra",
		"melenrdr",
		"mendocrl",
		"mesenmch",
		"myerskm",
		"ohrcrs",
		"paveytom",
		"pooscsbi",
		"prasakpp",
		"pusulssh",
		"riehlkrk",
		"romerira",
		"roseaam",
		"rossobth",
		"royerdug",
		"ruckesef",
		"schuefrn",
		"shahpas",
		"sheltdyl",
		"simonklu",
		"simondav",
		"soekohnr",
		"solaradr",
		"solarjre",
		"sousaeme",
		"stollcri",
		"stoufsot",
		"striefor",
		"swasepes",
		"trappuri",
		"vivalrca",
		"waddeptt"
		"walkemch",
		"waltejso",
		"webertia",
		"weigojmi",
		"westpsnd",
		"whitecai",
		"woodnil",
		"woolfath",
		"worsfvrg",
		"wrighwyn",
		"wysocgrh",
		"yorictev"
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
