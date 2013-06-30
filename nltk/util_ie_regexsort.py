#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, fileinput, re

if __name__ == "__main__":
	useless_texts = [
		"category: userselfservice",
		"closing this ticket.",
		"closing this tkt.",
		"copying, distribution, or use of the contents of this information is prohibited.",
		"dear madam or sir,",
		"dear sir,",
		"director: north america technology services",
		"disclaimer this electronic message transmission contains information that may be confidential or privileged.",
		"your request has been completed.",
		"your ticket request."
	]
	useless_texts = sorted(useless_texts, key=len, reverse=True)
	for text in useless_texts:
		print '"', text, '",'
