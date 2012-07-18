#/bin/bash
export trunk=`dirname $0`/../..
cd $trunk/src/tools
source stardog-env.sh

# Reasoning expressivity for stardog
expressivity=RL

stardog-admin server start
stardog-admin create -n r21db -t D -u admin -p admin --server snarl://localhost:5820/ $ontfiles $instancefiles

#sanity check
stardog query -c "http://127.0.0.1:5822/r21db;reasoning=$expressivity" -q "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> SELECT  ?s WHERE { ?s rdf:type owl:Thing . } LIMIT 10"

#instances of tooth - should return (without bulk instances loaded) http://purl.obolibrary.org/obo/OHD_0000016 tooth 14
stardog query -c "http://127.0.0.1:5822/r21db;reasoning=$expressivity" -q "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT * WHERE { ?s rdf:type <http://purl.obolibrary.org/obo/FMA_12516> . ?s rdfs:label ?l } LIMIT 10"

# # ask for types of tooth14
# stardog query -c "http://127.0.0.1:5822/r21db;reasoning=$expressivity" -q "PREFIX owl: <http://www.w3.org/2002/07/owl#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT * WHERE { <http://purl.obolibrary.org/obo/OHD_0000016> rdf:type ?t. optional {?t rdfs:label ?l} }"

# test sparql endpoint over http
echo "testing sparql over http"
#wget --quiet --header='accept: application/sparql-results+json' -O - 'http://127.0.0.1:5822/r21db;reasoning=$expressivity/query?query=select%20*%20where%20{?s%20?p%20?o}%20limit%2010&'


