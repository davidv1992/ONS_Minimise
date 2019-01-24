#!/bin/bash

tim="/usr/bin/time --format %e--%U"
run="timeout 121m $tim"

date

for d in ../Automata/random_*; do
  echo $d
  for f in $d/*.aut; do
    $run ./minimize $f > /dev/null
  done
  date
done

# Formula things not implemented
#for d in ../Automata/formula_*; do
#  echo $d
#  for f in $d/*.aut; do
#    $run ./minimize_form $f > /dev/null
#  done
#  date
#done

for i in $(seq 1 5); do
  echo Fifo $i
  $run ./fifo $i > /dev/null
  date
done

for i in $(seq 1 5); do
  echo ww $i
  $run ./running $i > /dev/null
  date
done

echo Lint
$run ./lint > /dev/null
date

echo Lmax
$run ./lmax > /dev/null
date

