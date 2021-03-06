#!/bin/bash

# Define where things are:
CORPUS=/local/scratch/mrd45/corpus

# Choose which things we are interested in investigating by modifying DIRS variable.
DIRS=$(echo $CORPUS/*)
# DIRS="$CORPUS/navier $CORPUS/arpack-ng $CORPUS/specfem3d $CORPUS/geos-chem-camfort $CORPUS/blas $CORPUS/computational-physics-1 $CORPUS/e3me $CORPUS/hybrid4 $CORPUS/mudpack $CORPUS/cliffs-src"

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


for d in $CORPUS; do
    if [ -d "$d" ]; then
        n=`find_sensible_name "$d"`
        find "$d" '(' -iname '*.f9?' -o -iname '*.f' ')' -printf "$n,%p\n"
    fi
done
