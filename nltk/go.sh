#!/usr/bin/env sh

echo "Running preprocess ..."
./preprocess.py hddata_a.csv > hddata_b.csv

echo "Generate description file ..."
./csvcat.py hddata_b.csv 13 > hddata_b_descript.csv
echo "Generate solution file ..."
./csvcat.py hddata_b.csv 14 > hddata_b_solution.csv