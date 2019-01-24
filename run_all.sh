#!/bin/bash

logd=$(pwd)/log
echo "Will write to $logd"

cd Automata
./conv_form.sh > $logd/conversion.txt
cd ..

cd ONS
./run_many.sh > $logd/ons.txt
cd ..

cd NLambda
./run_many.sh > $logd/nlambda.txt
cd ..

cd LOIS
./run_many.sh > $logd/lois.txt
cd ..
