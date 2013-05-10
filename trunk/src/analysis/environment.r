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


if(is.null(.GlobalEnv$current_endpoint))
    { current_endpoint <<- dungeon_r21_nightly }

if(is.null(.GlobalEnv$current_sparqlr))
    { current_sparqlr <<- "rrdf"; }

# strings for prefixes
default_ohd_prefixes <- function  ()
{
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX dental_patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX birth_date:  <http://purl.obolibrary.org/obo/OHD_0000050>
PREFIX occurrence_date:  <http://purl.obolibrary.org/obo/OHD_0000015>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
PREFIX participates_in: <http://purl.obolibrary.org/obo/BFO_0000056>
PREFIX dental_procedure: <http://purl.obolibrary.org/obo/OHD_0000002>
PREFIX crown_restoration: <http://purl.obolibrary.org/obo/OHD_0000033>
PREFIX tooth_restoration_procedure: <http://purl.obolibrary.org/obo/OHD_0000004>
PREFIX intracoronal_restoration: <http://purl.obolibrary.org/obo/OHD_0000006>
PREFIX veneer_restoration: <http://purl.obolibrary.org/obo/OHD_0000027>
PREFIX inlay_restoration: <http://purl.obolibrary.org/obo/OHD_0000133>
PREFIX onlay_restoration: <http://purl.obolibrary.org/obo/OHD_0000134>
PREFIX surgical_procedure: <http://purl.obolibrary.org/obo/OHD_0000044>
PREFIX endodontic_procedure: <http://purl.obolibrary.org/obo/OHD_0000003>
PREFIX tooth_restoration_procedure: <http://purl.obolibrary.org/obo/OHD_0000004>
PREFIX tooth_to_be_restored_role: <http://purl.obolibrary.org/obo/OHD_0000007>
PREFIX realizes: <http://purl.obolibrary.org/obo/BFO_0000055>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
PREFIX tooth: <http://purl.obolibrary.org/obo/FMA_12516>
PREFIX is_part_of: <http://purl.obolibrary.org/obo/BFO_0000050>
"}

lastSparqlQuery <- NULL;

querystring <- function(string,prefixes=default_ohd_prefixes)
  { lastSparqlQuery <- paste(prefixes(),"\n",string,"\n");
    lastSparqlQuery
  }

# this cache is more painful that it should be. Surely there is an idiomatic way to do this in R...

sessionQueryCache <- new.env(hash=TRUE);

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
      { assign(endpoint,new.env(hash=TRUE),sessionQueryCache);
        cacheQuery(query,endpoint,result)
      }
  }

queryCache <- function() { sessionQueryCache }

queryc <- function(string,endpoint=current_endpoint,prefixes=default_ohd_prefixes)
{ cached <- cachedQuery(querystring(string,prefixes=prefixes),endpoint);
  # cat("length(cached): ",length(cached),"\n");
  if (!is.null(cached))
    cached
  else
    { queryres <- if (current_sparqlr=="rrdf")
                {sparql.remote(endpoint,querystring(string,prefixes=prefixes)) }
                else if (current_sparqlr=="SPARQL")
                {SPARQL(endpoint,querystring(string,prefixes=prefixes))
                 res$results}
      # cat("query had ",length(queryres)," results\n");
      cacheQuery(querystring(string,prefixes=prefixes),endpoint,queryres);
      queryres
    }

}

lastSparql <- function() { cat(lastSparqlQuery) }
