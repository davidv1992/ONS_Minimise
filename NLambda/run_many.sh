#!/bin/bash

tim="/usr/bin/time --format %e--%U"
run="timeout 121m $tim"

mini="./.stack-work/install/x86_64-linux/lts-13.1/8.6.3/bin/minimize"

minimize="$mini File"
minimizeForm="$mini Formula"
fifo="$mini Fifo"
lint="$mini Lint"
lmax="$mini Lmax"
running="$mini DoubleWord"

date

for d in ../Automata/random_*; do
  echo $d
  for f in $d/*.aut; do
    $run $minimize $f > /dev/null
  done
  date
done

 Formula things not implemented
for d in ../Automata/formula_*; do
  echo $d
  for f in $d/*.aut; do
    $run $minimizeForm $f > /dev/null
  done
  date
done

for i in $(seq 1 5); do
  echo Fifo $i
  $run $fifo $i > /dev/null
  date
done

for i in $(seq 1 5); do
  echo ww $i
  $run $running $i > /dev/null
  date
done

echo Lint
$run $lint > /dev/null
date

echo Lmax
$run $lmax > /dev/null
date
