#!/bin/bash

tim="/usr/bin/time --format %e--%U"
run="timeout 121m $tim"

for d in formula_*; do
  echo $d
  mkdir -p flat_$d
  for f in $d/*.aut; do
    echo $f
    $run ../ONS/ConvertFormulaAutomaton < $f > flat_$f
  done
  date
done
