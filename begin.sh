#!/bin/bash
set -euo pipefail
DAY=$(date +%d | bc)
YEAR=$(date +%Y)
URL="https://adventofcode.com/${YEAR}/day/${DAY}"
EX_FILE="input_ex/day${DAY}_1.txt"
INPUT_FILE="input/day${DAY}.txt"
ERL_FILE="src/day${DAY}.erl"
touch "${EX_FILE}"
echo -n "Fetching input ... "
if [[ ! -e $INPUT_FILE ]]; then
    curl "$URL/input" --cookie "session=${AOC_SESSION}" -s > ${INPUT_FILE}
    echo "ok"
else
    echo "already exists, skip"
fi
echo -n "Generating ${ERL_FILE} ... "
if [[ ! -e $ERL_FILE ]]; then
    sed "s/@@DAY@@/${DAY}/g" day_template.erl > "${ERL_FILE}"
    echo "ok"
else
    echo "already exists, skip"
fi
echo "Launching firefox."
firefox "${URL}"
echo "Launching emacs."
emacsclient -n -a emacs "${ERL_FILE}" "${INPUT_FILE}" "${EX_FILE}" &
echo ${ERL_FILE} | entr -s "rebar3 eunit -m day${DAY}"
