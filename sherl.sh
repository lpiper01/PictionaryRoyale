#!/bin/bash

part1=`pwd`
part2="client/resources/erlport-0.9.8/priv/python2/"
full="$part1/$part2"
PYTHONPATH=$PYTHONPATH:$full

resources="-pa ./server ./client ./client/resources/erlport-0.9.8/ebin/"



cmd="erl -sname $1 ${resources} -env PYTHONPATH ${PYTHONPATH}"

echo $PYTHONPATH
echo $cmd
eval $cmd
