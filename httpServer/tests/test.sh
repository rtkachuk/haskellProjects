#!/bin/bash

if [ $# -eq 0 ]
then
	echo "No argument has been applied"
	exit 0
fi

for i in {1..200}
do
	if [[ $1 == *"p"* ]]; then
	        curl -X POST --data "value$i" http://127.0.0.1:8888/key$i
		curl -X POST http://127.0.0.1:8888/key$i
	fi

	if [[ $1 == *"g"* ]]; then
		curl -X GET http://127.0.0.1:8888/key$i
	fi

	if [[ $1 == *"d"* ]]; then
		curl -X DELETE http://127.0.0.1:8888/key$i
	fi

	echo $i
done
