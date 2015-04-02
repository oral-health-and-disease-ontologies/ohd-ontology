## Author: Alan Ruttenberg
## Project: OHD
## Date: May, 2013
##
## Modified by Bill Duncan December, 2015
##
## Support functions for demos. See load.r
## Cache query results during session so debugging is easier.

# load necessary libraries for sparql
library(rrdf) # defunct
library(MASS)
library(SPARQL)
source("SPARQL.r")

# if we use SPARQL library don't translate xsd:Date, so we can be compatible with RRDF
set_rdf_type_converter("http://www.w3.org/2001/XMLSchema#date",identity)

# triple store on local machine
local_r21_nightly <- "http://localhost:8080/openrdf-sesame/repositories/ohd-partial"

# triple store nightly build on the imac in the dungeon
dungeon_r21_nightly <- "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"

duncan_r21 <- "http://192.168.1.137:8080/openrdf-sesame/repositories/ohd-partial"

if(!exists("current_endpoint"))
 { print("You have to set current_endpoint first"); } # <<- duncan_r21 }

if(!exists("current_sparqlr"))
{ current_sparqlr <<- "rrdf"; }

# strings for prefixes
default_ohd_prefixes <- function  ()
{ all_prefixes_as_string(); }

lastSparqlQuery <- NULL;

querystring <- function(string,prefixes=default_ohd_prefixes)
  { lastSparqlQuery <- paste(prefixes_for_sparql(string),"\n",string,"\n");
    lastSparqlQuery
  }

sessionQueryCache <- new.env(hash=TRUE);

## See if we have a cached result for query executed at endpoint
cachedQuery <- function (query,endpoint)
{ if (exists(endpoint,sessionQueryCache))
  { endpointCache <- get(endpoint,sessionQueryCache);
    if (exists(query,endpointCache)) return(get(query,endpointCache)) }
  NULL
}

## Cache result for query executed at endpoint
cacheQuery <- function (query,endpoint,result)
{ if (exists(endpoint,sessionQueryCache))
  { endpointCache <- get(endpoint,sessionQueryCache)}
  else
  { endpointCache <<- new.env(hash=TRUE)
    assign(endpoint,endpointCache,sessionQueryCache) }
  assign(query,result,endpointCache)
  result
}

## return the session cache
queryCache <- function() { sessionQueryCache }

## execute a sparql query. endpoint defaults to current_endpoint, sparql library defaults to "rrdf"
queryc <- function(string,endpoint=current_endpoint,prefixes=default_ohd_prefixes,cache=TRUE)
{ if (identical(prefixes,FALSE))  { prefixes <- (function () {}) }
  cached <- cachedQuery(querystring(string,prefixes=prefixes),endpoint);
  if ((!is.null(cached)) && cache)
    cached
  else
    { queryres <- if (current_sparqlr=="rrdf")
                {sparql.remote(endpoint,querystring(string,prefixes=prefixes)) }
                else if (current_sparqlr=="SPARQL")
                {res<-SPARQL(endpoint,querystring(string,prefixes=prefixes))
                 res$results}
      cacheQuery(querystring(string,prefixes=prefixes),endpoint,queryres);
      queryres
    }
}

## retrieve the last sparql query result
lastSparql <- function() { cat(lastSparqlQuery) }
