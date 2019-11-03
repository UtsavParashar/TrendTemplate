#!/bin/bash
set -e
COUNT_CSV_FILES=`ls -l /Users/utsav/Downloads/5*.csv | wc -l`
if [ "$COUNT_CSV_FILES" -ne "1" ]; then
	rm -rf /Users/utsav/Downloads/5*.csv
	echo "More than one csv file found, Removed csv file, please download again"
	exit 0;
fi

if [ -f /Users/utsav/Downloads/5*.csv ]; then
	mv /Users/utsav/Downloads/5*.csv /Users/utsav/Investment/trading/data/data.csv
	tail -r /Users/utsav/Investment/trading/data/data.csv > /Users/utsav/Investment/trading/data/nd.csv
	sed '1h;1d;$!H;$!d;G' /Users/utsav/Investment/trading/data/nd.csv > /Users/utsav/Investment/trading/data/data.csv
	echo "You have the data copied"
else
	echo "File Not found"
fi

