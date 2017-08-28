#!/bin/bash

# Default corpus
DIRS=$(echo /home/camfort/corpus/*)

# Define where things are
# Override this with export CORPUS=/dir/
: ${CORPUS:=$DIRS}

# Choose which things we are interested in investigating by modifying DIRS variable.
DIRS=$(echo $CORPUS/*)

# select the 'sensible' name from the full pathname, somewhat subjectively
function find_sensible_name() {
    declare -A SENSELESSNAMES=([trunk]=1 [src]=1)
    n=`basename "$1"`
    if [ -n "${SENSELESSNAMES[$n]}" ]; then
       find_sensible_name `dirname "$1"`
    else
        echo $n | tr '-' '_' | tr ' ' '_'
    fi
}


for d in $DIRS; do
    if [ -d "$d" ]; then
        n=`find_sensible_name "$d"`
        find "$d" '(' -iname '*.f9?' -o -iname '*.f' ')' -printf "$n,%p\n"
    fi
done
