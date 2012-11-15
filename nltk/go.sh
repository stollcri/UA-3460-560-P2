#!/usr/bin/env sh

echo
date
echo "Running pre-pre-process a (remove useless lines) ..."
./prepreprocess_a.py hddata_a.csv > hddata_b.csv

echo
date
echo "Running pre-pre-process b (remove usernames, etc.) ..."
./prepreprocess_b.py hddata_b.csv > hddata_c.csv
rm hddata_b.csv

echo
date
echo "Running pre-process ..."
./preprocess.py hddata_c.csv > hddata_d.csv
#rm hddata_c.csv

echo
date
echo "Generate description/solution files ..."
./util_csvcat.py hddata_d.csv 13 > hddata_d_des.csv
./util_csvcat.py hddata_d.csv 14 > hddata_d_sol.csv
#rm hddata_d.csv
