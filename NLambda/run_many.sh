#!/bin/bash

mini="./.stack-work/install/x86_64-linux/lts-13.1/8.6.3/bin/minimize"

minimize="$mini File"
fifo="$mini Fifo"
lint="$mini Lint"
lmax="$mini Lmax"
running="$mini DoubleWord"

date
echo random_5_1
timeout 61m time -p $minimize ../Automata/random_5_1/*.aut  > /dev/null
date
echo random_10_1
timeout 61m time -p $minimize ../Automata/random_10_1/*.aut > /dev/null
date
echo random_10_2
timeout 61m time -p $minimize ../Automata/random_10_2/*.aut > /dev/null
date
echo random_15_1
timeout 61m time -p $minimize ../Automata/random_15_1/*.aut > /dev/null
date
echo random_15_2
timeout 61m time -p $minimize ../Automata/random_15_2/*.aut > /dev/null
date
echo random_15_3
timeout 61m time -p $minimize ../Automata/random_15_3/*.aut > /dev/null
date
echo fifo1
timeout 61m time -p $fifo 1 > /dev/null
date
echo fifo2
timeout 61m time -p $fifo 2 > /dev/null
date
echo fifo3
timeout 61m time -p $fifo 3 > /dev/null
date
echo fifo4
timeout 61m time -p $fifo 4 > /dev/null
date
echo fifo5
timeout 61m time -p $fifo 5 > /dev/null
date
echo ww1
timeout 61m time -p $running 1 > /dev/null
date
echo ww2
timeout 61m time -p $running 2 > /dev/null
date
echo ww3
timeout 61m time -p $running 3 > /dev/null
date
echo ww4
timeout 61m time -p $running 4 > /dev/null
date
echo ww5
timeout 61m time -p $running 5 > /dev/null
date
echo int
timeout 61m time -p $lint > /dev/null
date
echo max
timeout 61m time -p $lmax > /dev/null
date
