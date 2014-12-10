#!/bin/bash
for i in `ls *.f90`
do
    gfortran -M -cpp $i
done