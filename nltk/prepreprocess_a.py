#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, re

def usefull_line(text_line):
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
		if discard_marker in text_line:
			return False
	return True


def process_file():
	for text_line in fileinput.input():
		if usefull_line(text_line):
			print text_line,


if __name__ == "__main__":
	if len(sys.argv) >= 2 and sys.argv[1] == "-?":
		print "Syntax: prepreprocess.py filename.csv"
		print " prints processed file to stdout in csv format"
	else:
		process_file()
