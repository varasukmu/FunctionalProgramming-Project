#!/bin/bash
# Check the encoding of Dataset.csv
file Dataset.csv
echo "First 5 lines:"
head -5 Dataset.csv | od -c | head -20
