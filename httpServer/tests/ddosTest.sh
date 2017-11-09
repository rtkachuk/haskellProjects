#!/bin/sh

if [[ $# < 2 ]]
then
	printf "No argument has been applied"
	exit 0
fi

for (( i=1; i<=$2; i++ ))
do
	printf "==========\t$i\t ==========\n"
	if [[ $1 == *"p"* ]]; then
		printf "POST (READ): SENT"
	        curl -X POST --data "value$i" http://127.0.0.1:8888/key$i > /dev/null 2>&1 &
		printf "\nPOST (WRITE): SENT"
		curl -X POST http://127.0.0.1:8888/key$i > /dev/null 2>&1 &
		printf "\n"
	fi

	if [[ $1 == *"g"* ]]; then
		printf "GET: SENT"
		curl -X GET http://127.0.0.1:8888/key$i >> /dev/null 2>&1 &
		printf "\n"
	fi

	if [[ $1 == *"d"* ]]; then
		printf "DELETE: SENT"
		curl -X DELETE http://127.0.0.1:8888/key$i >> /dev/null 2>&1 &
		printf "\n"
	fi

	printf "==========\t$i\t ==========\n"
done
