# load necessary libraries for sparql
library(rrdf)
library(MASS)

library(SPARQL)

set_rdf_type_converter("http://www.w3.org/2001/XMLSchema#date",identity)

# variables for connecting to triple store
dungeon_r21_nightly <- "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"

current_endpoint <- dungeon_r21_nightly
current_sparqlr <- "rrdf";

queryc <- function(string)
{ if (current_sparqlr=="rrdf")
    {sparql.remote(current_endpoint,querystring(string)) }
  else if (current_sparqlr=="SPARQL")
    {res<-SPARQL(current_endpoint,querystring(string))
     res$results}
}

# strings for prefixes
prefixes <- function  ()
{
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX obo: <http://purl.obolibrary.org/obo>
PREFIX dental_patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX birth_date:  <http://purl.obolibrary.org/obo/OHD_0000050>
PREFIX occurrence_date:  <http://purl.obolibrary.org/obo/OHD_0000015>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
PREFIX participates_in: <http://purl.obolibrary.org/obo/BFO_0000056>
PREFIX dental_procedure: <http://purl.obolibrary.org/obo/OHD_0000002>
"}


querystring <- function(string)
  { paste(prefixes(),"\n",string,"\n") }

