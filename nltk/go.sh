#!/usr/bin/env sh

if [ -e "hddata_a.csv" ]; then
	echo
	date
	echo "Running pre-pre-process a (remove useless lines) ..."
	./prepreprocess_a.py hddata_a.csv > hddata_b.csv
fi

if [ -e "hddata_b.csv" ]; then
	echo
	date
	echo "Running pre-pre-process b (remove usernames, etc.) ..."
	./prepreprocess_b.py hddata_b.csv > hddata_c.csv
	rm hddata_b.csv
fi

if [ -e "hddata_c.csv" ]; then
	echo
	date
	echo "Running pre-process (regular expresion processing) ..."
	./preprocess.py hddata_c.csv > hddata_d.csv
	#rm hddata_c.csv
fi

if [ -e "hddata_d.csv" ]; then
	echo
	date
	echo "Running process (cascaded finite-state transducer) ..."
	./process.py hddata_d.csv 13 > hddata_e.csv
	#rm hddata_d.csv
fi
