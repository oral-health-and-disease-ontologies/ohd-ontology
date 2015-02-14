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

# if we use SPARQL library don't translate xsd:Date, so we can be compatible with RRDF
set_rdf_type_converter("http://www.w3.org/2001/XMLSchema#date",identity)

# triple store on local machine
#local_r21_nightly <- "http://localhost:8080/openrdf-sesame/repositories/ohd-r21-nightly"
local_r21_nightly <- "http://localhost:8080/openrdf-sesame/repositories/owlim-se-claudio-only"

# triple store nightly build on the imac in the dungeon
dungeon_r21_nightly <- "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"

duncan_r21 <- "http://192.168.1.137:8080/openrdf-sesame/repositories/owlim-se-claudio-only"

if(!exists("current_endpoint"))
 { print("You have to set current_endpoint first"); } # <<- duncan_r21 }

if(!exists("current_sparqlr"))
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
PREFIX has_participant: <http://purl.obolibrary.org/obo/BFO_0000057>
PREFIX dental_procedure: <http://purl.obolibrary.org/obo/OHD_0000002>
PREFIX crown_restoration: <http://purl.obolibrary.org/obo/OHD_0000033>
PREFIX tooth_restoration_procedure: <http://purl.obolibrary.org/obo/OHD_0000004>
PREFIX intracoronal_restoration: <http://purl.obolibrary.org/obo/OHD_0000006>
PREFIX veneer_restoration: <http://purl.obolibrary.org/obo/OHD_0000027>
PREFIX inlay_restoration: <http://purl.obolibrary.org/obo/OHD_0000133>
PREFIX onlay_restoration: <http://purl.obolibrary.org/obo/OHD_0000134>
PREFIX surgical_procedure: <http://purl.obolibrary.org/obo/OHD_0000044>
PREFIX endodontic_procedure: <http://purl.obolibrary.org/obo/OHD_0000003>
PREFIX tooth_to_be_restored_role: <http://purl.obolibrary.org/obo/OHD_0000007>
PREFIX tooth_to_be_filled_role: <http://purl.obolibrary.org/obo/OHD_0000008>
PREFIX realizes: <http://purl.obolibrary.org/obo/BFO_0000055>
PREFIX tooth: <http://purl.obolibrary.org/obo/FMA_12516>
PREFIX is_part_of: <http://purl.obolibrary.org/obo/BFO_0000050>
PREFIX tooth_surface: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Surface_enamel_of_tooth>
PREFIX mesial: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Mesial_surface_enamel_of_tooth>
PREFIX distal: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Distal_surface_enamel_of_tooth>
PREFIX occlusal: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Occlusial_surface_enamel_of_tooth>
PREFIX buccal: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Buccal_surface_enamel_of_tooth>
PREFIX labial: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Labial_surface_enamel_of_tooth>
PREFIX lingual: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Lingual_surface_enamel_of_tooth>
PREFIX is_dental_restoration_of: <http://purl.obolibrary.org/obo/OHD_0000091>
PREFIX dental_restoration_material: <http://purl.obolibrary.org/obo/OHD_0000000>
PREFIX has_specified_input: <http://purl.obolibrary.org/obo/OBI_0000293>
PREFIX has_specified_output: <http://purl.obolibrary.org/obo/OBI_0000299>
PREFIX asserted_type: <http://purl.obolibrary.org/obo/OHD_0000092>
PREFIX tooth_number: <http://purl.obolibrary.org/obo/OHD_0000065>
PREFIX female: <http://purl.obolibrary.org/obo/OHD_0000049>
PREFIX male: <http://purl.obolibrary.org/obo/OHD_0000054>
PREFIX patient: <http://purl.obolibrary.org/obo/OHD_0000012>
"}

lastSparqlQuery <- NULL;

querystring <- function(string,prefixes=default_ohd_prefixes)
  { lastSparqlQuery <- paste(prefixes(),"\n",string,"\n");
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
queryc <- function(string,endpoint=current_endpoint,prefixes=default_ohd_prefixes)
{ cached <- cachedQuery(querystring(string,prefixes=prefixes),endpoint);
  if (!is.null(cached))
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
