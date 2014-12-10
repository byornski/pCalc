#!/bin/bash

bin_dir=""

#Check for input/output dirs
for i in `ls $src_dir/*.f90`
do


    filename=$(basename $i)

#    should use gfortran -M -cpp $i........
#    but it does not work if the program is not already compiled


    #Grep for first module filename
    oname=`echo $filename | awk '{sub(".f90",".o"); print}'`
    modname=$bin_dir`grep -m1 -i 'module' $i | awk '{print tolower($2)}'`

#    echo $oname

    if [ -z "$modname" ]
    then
	objnames=`echo $oname:`
    else
	objnames=`echo $oname $modname.mod:`
    fi
    
    depslist=`grep 'use' $i | awk -v bindir=$bin_dir '{sub(",",""); print bindir tolower($2) ".mod"}' | paste -s -d " "`


    echo $objnames ../$i $depslist


done