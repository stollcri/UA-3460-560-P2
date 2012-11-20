#!/usr/bin/env sh

echo "Information Extraction Process Begin"

if [ -e "hddata_a.csv" ]; then
	date
	echo "Running pre-pre-process a (remove useless lines) ..."
	./ie_preproc_a.py hddata_a.csv > hddata_b.csv
	#rm hddata_a.csv
fi

if [ -e "hddata_b.csv" ]; then
	date
	echo "Running pre-pre-process b (remove usernames, etc.) ..."
	./ie_preproc_b.py hddata_b.csv > hddata_c.csv
	rm hddata_b.csv
fi

if [ -e "hddata_c.csv" ]; then
	date
	echo "Running pre-process (regular expresion processing) ..."
	./ie_preproc_c.py hddata_c.csv > hddata_d.csv
	#rm hddata_c.csv
fi

if [ -e "hddata_d.csv" ]; then
	date
	echo "Running process (cascaded finite-state transducer) ..."
	./ie_proc.py hddata_d.csv > hddata_e.csv
	#rm hddata_d.csv
fi
