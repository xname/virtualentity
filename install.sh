#!/bin/bash

PREFIX=/usr

echo 'installing virtual entity in ' $PREFIX '...'
echo 'most likely you will have to be superuser to do this'
# add loop to install library if it is missing
aptitude install python-mysqldb
mkdir   -p $PREFIX/share/soul/
install -m 644 src/*.py $PREFIX/share/soul
install -m 755 src/soul $PREFIX/bin/soul
install -m 644 AUTHORS COPYING INSTALL metadata_sets README TODO USAGE virtualities $PREFIX/share/soul
install -m 644 soul.1.gz $PREFIX/share/man/man1/
# add if - elif loop to check whether things are done or not
echo 'hopefully done' 

