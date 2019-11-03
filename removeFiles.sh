#!/bin/bash

FileName=$1

if [ -f $FileName ]; then
	rm -rf $FileName
	echo "Removed File $FileName"
fi
