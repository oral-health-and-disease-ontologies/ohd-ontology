
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
library(ggplot2)
library(ggthemes) ## needs to be downloaded from CRAN and installed: R CMD install ggthemes.tgz
library(gridExtra)
library(httr)

# if we use SPARQL library don't translate xsd:Date, so we can be compatible with RRDF
set_rdf_type_converter("http://www.w3.org/2001/XMLSchema#date",identity)

## triple store on local machine
local_r21_nightly <- "http://localhost:8080/openrdf-sesame/repositories/OHDRL20150412"

## note form of sparql update query is not as documented. Should be
## openrdf-sesame/repositories/OHDRL20150412/statements but that
## doesn't work.

local_r21_nightly_update <- "http://localhost:8080/openrdf-workbench/repositories/OHDRL20150412/update"

# triple store nightly build on the imac in the dungeon
dungeon_r21_nightly <- "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"


## Bill D: 2017-10-09
## I updated to graphdb-se 8.3 and R version to 3.4.2. This broke rrdf and SPARQL packages
## To fix, I had to change the endpoint. I could not get rrdf to work, but got SPARQL working.
current_endpoint <<- "http://192.168.1.11:7200/repositories/R21-study_2017-10-09"
current_sparqlr <<- "SPARQL"
# current_sparqlr <<- "rrdf" # I couldn't get this to work :(

if(!exists("current_endpoint"))
 { print("You have to set current_endpoint first"); } # <<- duncan_r21 }

if(!exists("current_sparqlr"))
{ current_sparqlr <<- "rrdf"; }

# strings for prefixes
default_ohd_prefixes <- function  ()
{ all_prefixes_as_string(); }

lastSparqlQuery <- NULL;

querystring <- function(...,prefixes=default_ohd_prefixes)
  { string <- paste(...,sep="\n");
    lastSparqlQuery <<- paste(prefixes_for_sparql(string),"\n",string,"\n");
    lastSparqlQuery
  }

clearSPARQLSessionCache <- function ()
 { sessionQueryCache <<- new.env(hash=TRUE); }

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

trace_sparql_queries <- FALSE;
send_queries_to_workbench <- FALSE;

check_sparql_syntax <- file.exists("jena/bin/qparse");

## execute a sparql query. endpoint defaults to current_endpoint, sparql library defaults to "rrdf"
queryc <- function(...,endpoint=current_endpoint,prefixes=default_ohd_prefixes,cache=TRUE,trace=trace_sparql_queries,nosend=F)
{ 
    if (send_queries_to_workbench && !nosend) { queryw(...,prefixes=prefixes); return(NULL)};
    string <- paste(..., sep="\n");
    if (identical(prefixes,FALSE))  { prefixes <- (function () {}) }
    querystring <- querystring(string,prefixes=prefixes);
    if (trace) { print(date());print(endpoint);cat(querystring) }
    if (check_sparql_syntax)
        { if (!checkSPARQLSyntax(querystring))
              { return( NULL) }}
    cached <- cachedQuery(querystring(string,prefixes=prefixes),endpoint);
    if ((!is.null(cached)) && cache)
        cached
    else
        { queryres <- if (current_sparqlr=="rrdf")
                          {sparql.remote(endpoint,querystring(string,prefixes=prefixes)) }
                      else if (current_sparqlr=="SPARQL")
                          {res<-SPARQL(endpoint,querystring(string,prefixes=prefixes))
                           res$results}
          qrrr<<-queryres;
          if (length(queryres)==2 && (dim(queryres)==cbind(0,0)) == c(F,F)) # the function returned a 0x0 empty matrix, i.e. error
              { cat("There was an error in the SPARQL query"); cat(querystring); cat(queryres); stop() }
          else 
              { cacheQuery(querystring(string,prefixes=prefixes),endpoint,queryres) }
# interferes with some tools          
#          attr(queryres,"rawQuery") <- string;
#          attr(queryres,"expandedQuery") <- querystring;
#          attr(queryres,"endpoint") <- endpoint;
          queryres
      }
}

## retrieve the last sparql query result
lastSparql <- function() { cat(lastSparqlQuery) }

## describe a term.
rdfd <- function(uri,limit=100,show.uris=F)
{  cat("What things have it as subject\n")
   write.table(queryc(paste("select distinct ?sl ",if(show.uris){"?p"} else {""}, "?pl ", if(show.uris){"?v"} else {""}, " ?vl where",
                            "{ <",uri,"> ?p ?v.",
                            "  <",uri,"> rdfs:label ?sl. ",
                            "optional",
                            "  {?p rdfs:label ?pl} ",
                            "optional",
                            "  { ?v rdfs:label ?vl}} ",
                            "limit ",limit,sep=""
                            ),nosend=T),quote=F,row.names=F )
   cat("What things have it as object\n")
   write.table(queryc(paste("select distinct ",if(show.uris){"?s"} else {""}, "?sl ", if(show.uris){"?p"} else {""}, " ?pl  ?ol where",
                            "{ ?s ?p <",uri,"> .",
                            "  <",uri,"> rdfs:label ?ol. ",
                            "optional {?p rdfs:label ?pl}",
                            "optional { ?s rdfs:label ?sl}}",
                            "limit ",limit,sep=""
                            ),nosend=T),quote=F,row.names=F)
}


## describe an entity given its label
rdfdl <- function(label,limit=100,show.uris=T)
{  cat("What things have it as subject\n");
   write.table(queryc(paste(" select distinct ?label ",if(show.uris){"?p"} else {""}, "?pl ", if(show.uris){"?v"} else {""}, " ?vl  where ",
              "{  ?uri rdfs:label \"",label,"\".",
              "   ?uri rdfs:label ?label.",
              "   ?uri ?p ?v. ",
              "   optional {?p rdfs:label ?pl}. ",
              "   optional {?v rdfs:label ?vl}.",
              " } limit ",limit,
                sep=""
              ),nosend=T),quote=F,row.names=F);
   cat("What things have it as object\n")
   write.table(queryc(paste(" select distinct ",if(show.uris){"?s"} else {""}, "?sl ", if(show.uris){"?p"} else {""}, " ?pl ?label where ",
              "{  ?uri rdfs:label \"",label,"\".",
              "   ?uri rdfs:label ?label.",
              "   ?s ?p ?uri. ",
              "   optional {?p rdfs:label ?pl}. ",
              "   optional {?s rdfs:label ?sl}.",
              " } limit",limit,
                sep=""
              ),nosend=T),sep=",",quote=F,row.names=F);
 }

rdfslabel <- function(uri)
  { queryc("select ?label where{ ",uri," rdfs:label ?label}")}

uriwithlabel <- function(label)
  { queryc(paste0("select ?uri where{ ?uri rdfs:label ?label filter (str(?label)=\"",label,"\")}"))}

# plot using SVG in the browser
bplot <- function (...)
  { svg(filename="/tmp/rsvg.svg")
    plot(...)
    dev.off()
    browseURL("file:///tmp/rsvg.svg")
  }

bplotf <- function (f,index=1,filebase="rsvg")
    { tryCatch({
        filename<-paste0("/tmp/",filebase,index,".svg");
        svg(filename=filename,height=8,width=10)
        f()
        },stop={},finally={
        dev.off()
        browseURL(paste0("file://",filename))})
  }

sparqlUpdate <- function (...,endpoint=paste0(current_endpoint,"/statements"),doit=TRUE,trace=trace_sparql_queries)
    { if (send_queries_to_workbench) { sparqlUpdatew(...); return(NULL);}
      clearSPARQLSessionCache();
      update <- querystring(paste(...,sep="\n"));
    if (trace) { print(date());print(endpoint);cat(update) }
    if (doit) { postForm(endpoint,update=update,style='POST') }
  }

read.sparql.file <- function (file, limit=0, print.query=FALSE){
  ## read contents from file 
  con <- file(file, "r", blocking = FALSE)
  sparql <- readLines(con)
  close(con)
  
  ## collapsed lines 
  sparql <- paste(sparql, sep = "", collapse="  \n ")
  
  ## check for limit
  if (limit > 0)
    sparql <- paste(sparql," limit ", limit, " \n", sep="")
  
  ## check for print to screen
  if (print.query) { cat(sparql) }
  
  ## return sparql query
  sparql
}

query.from.file <- function (file, limit=0, print.query=FALSE, as.data.frame=TRUE, remove.prefixes=TRUE) {
  ## get query string from file
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## check if prefixes need to be removed from query string
  if (remove.prefixes == TRUE) {
    query.string <- gsub("PREFIX[ ]*[^\n]*\n","",query.string)
  }
  
  ## get results
  res <- queryc(query.string)
  
  if (as.data.frame == TRUE) {
    ## convert to dataframe; note: stringsAsFactors must be false
    return.val <- as.data.frame(res, stringsAsFactors = F)
  }
  else {
    return.val <- res
  }
  
  ## return results
  return.val
}

prettysparql<-""
insertLabels<-FALSE;

## e.g."Lexical error at line 4, column 4.  Encountered: \" \" (32), after : \"s\""
checkSPARQLSyntax <-function(querystring)
    { prettysparql <<- NULL;
      if (file.exists("/tmp/r.sparql")) { file.remove("/tmp/r.sparql") }
      write(querystring,file="/tmp/r.sparql");
      result <- suppressWarnings(system("jena/bin/qparse --file /tmp/r.sparql 2>&1",intern=T));
      if (!is.null(attr(result,"status")))
          { cat("Error in query\n---------\n");
            cat("            111111111122222222223333333333344444444445555555555666666666677777777778\n");
            cat("   123456789012345678901234567890123456789001234567890123456789012345678901234567890\n");
            write.table(strsplit(querystring,"\n"),col.names=F,quote=F,sep=": ");
            cat(paste("--------\n",do.call(paste0,as.list(result)),"\n",sep=""))
            return(FALSE)}
      else { #fix some irritations
          result <- gsub("[(]\\s+","(",result,perl=TRUE);
          result <- gsub("\\s+[)]",")",result,perl=TRUE);
          result <- gsub("(?s)! (bound)","!\\1",result,perl=TRUE);
          prettysparql <<- result; return(TRUE) }
  }
        
sparqlUpdatew <- function (...,endpoint=current_sparql_endpoint,doit=TRUE,trace=trace_sparql_queries)
    { clearSPARQLSessionCache();
      update <- querystring(paste(...,sep="\n"));
      if (file.exists("/tmp/sparql.sparql")) { file.remove("/tmp/sparql.sparql") }
      write(update,file="/tmp/sparql.sparql");
      result <- suppressWarnings(system("osascript putsparql.scpt http://127.0.0.1:8080/graphdb-workbench-ee/update /tmp/sparql.sparql 2>&1",intern=T))
  }
    
queryw <- function (...,prefixes=default_ohd_prefixes,endpoint,trace)
  { 
    string <- paste(..., sep="\n");
    if (insertLabels) {string<- query_with_labels(string)};
    sparql <- querystring(string,prefixes=prefixes);
  if (check_sparql_syntax)
    { if (!checkSPARQLSyntax(sparql))
        { prettysparql <<- lastSparqlQuery }}
    if (file.exists("/tmp/sparql.sparql")) { file.remove("/tmp/sparql.sparql") }
    write(prettysparql,file="/tmp/sparql.sparql");
    result <- suppressWarnings(system("osascript putsparql.scpt http://127.0.0.1:8080/graphdb-workbench-ee/sparql /tmp/sparql.sparql 2>&1",intern=T))
}

#gsub("http://purl.obolibrary.org/obo/ohd/","ind:",gsub("http://purl.obolibrary.org/obo/ohd/individuals/","ohd:",fail[seq(1,100),]))

