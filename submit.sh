#!/bin/bash

case $# in
    1) DIR="$1"
       ;;
    *) echo "Usage: $0 dir-with-inputs" 1>&2
       exit -1
       ;;
esac

OUTPUT=/tmp/output.txt

set -e

for input in $(ls "$DIR"/*[0-9].in)
do
    id=$(basename "$input" | sed 's/^0*//g;s/\.in$//g')
    echo "Processing $input..."
    stack exec solver < "$input" > "$OUTPUT"
    curl --compressed -L -H Expect: -H 'X-API-Key: 246-2a711bdbced516bc9ce8f3205b413a58' \
         -F "problem_id=$id" -F "solution_spec=@$OUTPUT" \
         'http://2016sv.icfpcontest.org/api/solution/submit'
    sleep 2
done
