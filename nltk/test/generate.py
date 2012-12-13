#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, csv, random

def process_files(pass_no):
	in_name_train = "../dataset1/hddata_e.csv"
	out_name_train = "./i"+str(pass_no)+"_train.csv"
	in_name_test = "../dataset1/hddata_e.csv"
	out_name_test = "./i"+str(pass_no)+"_test.csv"

	in_file_train = open(in_name_train, 'rb')
	csv_in_train = csv.reader(in_file_train, delimiter=',', quotechar='"')

	out_file_train = open(out_name_train, 'wb')
	csv_out_train = csv.writer(out_file_train, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	
	train_nos = []
	for csv_row in csv_in_train:
		# 1 / 5 go into the test file
		rand_no = random.randrange(1, 6, 1)
		# test
		if rand_no == 2:
			# save the index numbers
			train_nos.append(csv_row[0])
		# train
		else:
			csv_out_train.writerow(csv_row)
	
	in_file_test = open(in_name_test, 'rb')
	csv_in_test = csv.reader(in_file_test, delimiter=',', quotechar='"')

	out_file_test = open(out_name_test, 'wb')
	csv_out_test = csv.writer(out_file_test, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	
	for csv_row in csv_in_test:
		if len(train_nos) > 0:
			if csv_row[0] == train_nos[0]:
				csv_out_test.writerow(csv_row)
				train_nos.remove(csv_row[0])
	

if __name__ == "__main__":
	# generate 5 test groups
	for x in xrange(1,6):
		process_files(x)
