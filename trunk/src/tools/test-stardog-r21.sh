#/bin/bash
trunk=`dirname $0`/../..

# This is where the database files will be kept (not checked in, of course)
export STARDOG_HOME=$trunk/store/stardog-r21/
echo PATH\=$STARDOG_HOME\:\$PATH
# install the stardog distribution somewhere and then soft link in the executables stardog-admin, stardog-shell, and stardog into trunk/bin
export PATH=$trunk/bin:$PATH

export STARDOG_JAVA_ARGS='-DdisableSecurity=true'

ontfiles="$trunk/src/ontology/pitt-ub-ohsu-r21/imports/*.owl $trunk/src/ontology/pitt-ub-ohsu-r21/imports/BFO2/*.owl $trunk/src/ontology/pitt-ub-ohsu-r21/ohd.owl"

# instance files are kept in a dropbox folder rather than in repo as they have might have phi, and soft linked into this folder
instancefiles="$trunk/src/ontology/pitt-ub-ohsu-r21/dropbox/*.owl"

# Reasoning expressivity for stardog
expressivity=EL

# clean up from before
rm -rf $STARDOG_HOME/*

#start server
stardog-admin server start

# create database, see http://stardog.com/docs/
stardog-admin create -n r21db -t D -u admin -p admin --server snarl://127.0.0.1:5820/ 

# add the files
stardog add -u admin -p admin -c "snarl://127.0.0.1:5820/r21db;reasoning=$expressivity" $ontfiles $instancefiles

# print out metadata for the database
stardog-admin get -n r21db

#sanity check
stardog query -c "http://127.0.0.1:5822/r21db;reasoning=$expressivity" -q "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> SELECT  ?s WHERE { ?s rdf:type owl:Thing . } LIMIT 10"

#instances of tooth - should return (without bulk instances loaded) http://purl.obolibrary.org/obo/OHD_0000016 tooth 14
stardog query -c "http://127.0.0.1:5822/r21db;reasoning=$expressivity" -q "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT * WHERE { ?s rdf:type <http://purl.obolibrary.org/obo/FMA_12516> . ?s rdfs:label ?l } LIMIT 10"

# # ask for types of tooth14
# stardog query -c "http://127.0.0.1:5822/r21db;reasoning=$expressivity" -q "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT * WHERE { <http://purl.obolibrary.org/obo/OHD_0000016> rdf:type ?t. optional {?t rdfs:label ?l} }"

# test sparql endpoint over http
echo "testing sparql over http"
wget --quiet --header='accept: application/sparql-results+json' -O - 'http://127.0.0.1:5822/r21db;reasoning=$expressivity/query?query=select%20*%20where%20{?s%20?p%20?o}%20limit%2010&'

# stop the server
#stardog-admin server stop

