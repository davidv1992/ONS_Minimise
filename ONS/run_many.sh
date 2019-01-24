#!/bin/bash

tim="/usr/bin/time --format %e--%U"
run="timeout 121m $tim"

date

# These are so small for ONS, that we run 10x the same file
for d in ../Automata/random_*; do
  echo $d x10
  for f in $d/*.aut; do
    $run ./MinimizeTest $f $f $f $f $f $f $f $f $f $f > /dev/null
  done
  date
done

for d in ../Automata/flat_formula_*; do
  echo $d
  for f in $d/*.aut; do
    $run ./MinimizeTest $f > /dev/null
  done
  date
done

for i in $(seq 1 5); do
  echo Fifo $i
  $run ./MinimizeTest ../Automata/fifo/$i.aut > /dev/null
  date
done

for i in $(seq 1 5); do
  echo ww $i
  $run ./MinimizeTest ../Automata/running/$i.aut > /dev/null
  date
done

echo Lint
$run ./MinimizeTest ../Automata/Example/Lint.aut > /dev/null
date

echo Lmax
$run ./MinimizeTest ../Automata/Example/Lmax.aut > /dev/null
date
