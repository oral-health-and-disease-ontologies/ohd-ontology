## Author: Alan Ruttenberg
## Project: OHD
## Date: May, 2013
##
## Support functions for demos. See load.r
## Cache query results during session so debugging is easier.

# load necessary libraries for sparql
library(rrdf)
library(MASS)

library(SPARQL)
set_rdf_type_converter("http://www.w3.org/2001/XMLSchema#date",identity)

# variables for connecting to triple store.
dungeon_r21_nightly <- "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"

current_endpoint <- dungeon_r21_nightly
current_sparqlr <- "rrdf";

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

lastSparqlQuery <- NULL;

querystring <- function(string)
  { lastSparqlQuery <- paste(prefixes(),"\n",string,"\n");
    lastSparqlQuery
  }

# this cache is more painful that it should be. Surely there is an idiomatic way to do this in R...

sessionQueryCache <- new.env();

cachedQuery <- function (query,endpoint)
  { endpointCache <- mget(endpoint,sessionQueryCache,ifnotfound=notfound);
    if (is.environment(endpointCache[[1]]))
      { result<-mget(query,endpointCache[[1]],ifnotfound=notfound);
        if (result[[1]][1]!=notfound[[1]])
          {result[[1]]}
      }
  }

notfound <- as.list(c("notfound"));

cacheQuery <- function (query,endpoint,result)
  { endpointCache <- mget(endpoint,sessionQueryCache,ifnotfound=notfound);
#    cat("endpointcache is ",ls.str(endpointCache[[1]]),"\n");
    if (is.environment(endpointCache[[1]]))
      { cached <- mget(query,endpointCache[[1]],ifnotfound=notfound);
        if (cached[[1]]!=notfound)
          { #cat("returning cached\n");
            cached[[1]] }
        else
          { # cat("saving query\n");
            assign(query,result,endpointCache[[1]])
            result;
          }}
    else
      { assign(endpoint,new.env(),sessionQueryCache);
        cacheQuery(query,endpoint,result)
      }
  }

queryCache <- function() { sessionQueryCache }

queryc <- function(string)
{ cached <- cachedQuery(querystring(string),current_endpoint);
  # cat("length(cached): ",length(cached),"\n");
  if (!is.null(cached))
    cached
  else
    { queryres <- if (current_sparqlr=="rrdf")
                {sparql.remote(current_endpoint,querystring(string)) }
                else if (current_sparqlr=="SPARQL")
                {SPARQL(current_endpoint,querystring(string))
                 res$results}
      # cat("query had ",length(queryres)," results\n");
      cacheQuery(querystring(string),current_endpoint,queryres);
      queryres
    }
      
}

lastSparql <- function() { cat(lastSparqlQuery) }
