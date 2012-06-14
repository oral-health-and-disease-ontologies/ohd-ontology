#!/bin/bash
export trunk=`dirname $0`/../..
cd $trunk/src/tools
source stardog-env.sh
stardog-admin server stop

# clean up from before
mv $STARDOG_HOME/stardog-license-key.bin $STARDOG_HOME/.stardog-license-key.bin 
rm -rf $STARDOG_HOME/*
mv $STARDOG_HOME/.stardog-license-key.bin $STARDOG_HOME/stardog-license-key.bin 

#start server
stardog-admin server start

# create database, see http://stardog.com/docs/
stardog-admin create -n r21db -t D -i -u admin -p admin --server snarl://127.0.0.1:5820/ 

# add the files
stardog add -u admin -p admin -c "snarl://127.0.0.1:5820/r21db;reasoning=$expressivity" $ontfiles $instancefiles

