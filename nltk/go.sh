#!/usr/bin/env sh

echo
date
echo "Running pre-pre-process a (remove useless lines) ..."
./prepreprocess_a.py hddata_a.csv > hddata_b.csv

echo
date
echo "Running pre-pre-process b (remove usernames) ..."
./prepreprocess_b.py hddata_b.csv > hddata_c.csv

echo
date
echo "Removing temp file ..."
rm hddata_b.csv

echo
date
echo "Running pre-process ..."
./preprocess.py hddata_c.csv > hddata_d.csv

echo
date
echo "Removing temp file ..."
#rm hddata_c.csv

echo
date
echo "Generate description file ..."
./csvcat.py hddata_d.csv 13 > hddata_d_descript.csv
echo "Generate solution file ..."
./csvcat.py hddata_d.csv 14 > hddata_d_solution.csv

echo
date
echo "Removing temp file ..."
#rm hddata_d.csv
