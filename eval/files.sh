#!/bin/bash

# define where things are

CORPUS=/local/scratch/mrd45/corpus
UNIFIEDMODEL=$HOME/um

# choose which things we are interested in investigating

#DIRS="$CORPUS/samples $CORPUS/computational-physics-1 $CORPUS/computational-physics-2 $CORPUS/e3mg-ea $CORPUS/e3mg-mark $CORPUS/e3mg-modernise $CORPUS/hybrid4 $CORPUS/navier $UNIFIEDMODEL/trunk/src"
#DIRS="$CORPUS/computational-physics-1"

# for paper
# ARPACK_NG, BLAS, CP, E3MG (EA), GEOS-CHEM, Hybridr4, Navier, SpecFem3D, Mudpack, Cliffs, UM
DIRS="$CORPUS/arpack-ng $CORPUS/specfem3d $CORPUS/geos-chem-camfort $CORPUS/blas $CORPUS/computational-physics-1 $CORPUS/e3me $CORPUS/hybrid4 $CORPUS/navier $CORPUS/mudpack $CORPUS/cliffs-src $UNIFIEDMODEL/trunk/src"

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
