#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, re

def usefull_line(text_line):
	discard_markers = [
		# created only for tracking/metric purposes
		"Automatically generated ticket by SAP Global Technical Application Development",
		"the following malware components were found",
		"This ticket has been created automatically",
		# covered by well known defined processes
		"Your requested new user account(s) have been created.",
		"Hello, the validity date for your desired SAP-system was extended.",
		"Hello, the reset for your desired SAP-system was processed.",
		"Hello, the reset for your desired system was processed.",
		"SAP password reset was processed automatically.",
		"Hallo, ihr Kennwort wurde grundgestellt.",
		"extend the validity date",
		"The required SAP authorizations have been requested for you.",
		"The required SAP authorization (",
		"the requested permissions for file access have been given.",
		"Request for file access was approved by data owner",
		"Antrag wurde vom Verzeichnisowner genehmigt.",
		"your file access request was rejected",
		"Request was approved by data owner!",
		"request for deleting was approved ",
		# died due to lack of information
		"no response from user closing ticket",
		"request has already been taken care",
		"Ticket closed automatically",
		"Please close this ticket.",
		"Closing this ticket.",
		"Closing ticket",
		# potentially sensitive information
		"Social Security",
		"Social security",
		"social security",
		"Confidential",
		"confidential",
		"PASSWORTER",
		"Passworter",
		"passworter",
		"PASSWORT",
		"Passwort",
		"passwort",
		"PASSWORD",
		"Password",
		"password",
		"Birth Date",
		"Birth date",
		"birth date",
		"Birthday",
		"birthday",
		"pwd:",
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
