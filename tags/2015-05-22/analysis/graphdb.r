## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-20
##
## Graphdb-specific functions

setRepositoryPrefixes <- function()
  { DELETE(paste0(current_endpoint,"/namespaces"));
    lapply(ls(prefixes),function(p) {PUT(paste0(current_endpoint,"/namespaces/",gsub(":","",p)),body=gsub("<|>","",get(p,prefixes)))});
    NULL
  }

## get the current query timeout, in seconds
get_graphdb_timeout <- function ()
{ system_endpoint <- gsub("(.*/)\\S+", "\\1SYSTEM", current_endpoint);
  repo <- gsub(".*/(\\S+)", "\\1", current_endpoint)
  queryc("select ?timeout ",
         "WHERE ",
         " { ?ctx a rep:RepositoryContext.",
         "   graph ?ctx",
         paste0("   {  _:a rdfs:label \"",repo,"\"."),
         "      _:a  a rep:Repository.",
         "     ?place owlim:query-timeout ?timeout.",
         "     }",
         "   }",
         endpoint=system_endpoint,cache=FALSE)
}

## set the current query timeout in seconds. Can be useful when debugging.
set_graphdb_timeout <- function (seconds)
{ system_update_endpoint <- gsub("(.*/)\\S+", "\\1SYSTEM/statements", current_endpoint);
  repo <- gsub(".*/(\\S+)", "\\1", current_endpoint)
  sparqlUpdate("DELETE { graph ?ctx ",
              "   {?place owlim:query-timeout ?timeout.} } ",
              "INSERT { graph ?ctx ",
              paste0("   {?place owlim:query-timeout \"",seconds,"\". }}"),
              "WHERE ",
              " { ?ctx a rep:RepositoryContext.",
              "   graph ?ctx",
              paste0("   {  _:a rdfs:label \"",repo,"\"."),
              "      _:a  a rep:Repository.",
              "     ?place owlim:query-timeout ?timeout.",
              "     }",
              "   }",
              endpoint=system_update_endpoint)
}


## the following two functions need testing
graphdb_ee_get_parameters <-function ()
  { write.table(queryc(
    "PREFIX sys:  <http://www.openrdf.org/config/repository#>",
    "PREFIX sail: <http://www.openrdf.org/config/repository/sail#>",
    "select ?id ?type ?param ?value",
    "where {",
    "  ?rep sys:repositoryID ?id .",
    "  ?rep sys:repositoryImpl ?delegate .",
    "  ?delegate sys:delegate ?impl .",
    "  ?impl sys:repositoryType ?type .",
    "  optional {",
    "    ?impl sail:sailImpl ?sail .",
    "    ?sail ?param ?value .",
    "  }",
                      # FILTER( ?id = "specific_repository_id" )
    "}",
    "ORDER BY ?id ?param",endpoint=gsub("(.*/)(.*)","\\1SYSTEM",current_endpoint,perl=TRUE)),quote=F,sep="\t")
  }

graphdb_ee_change_parameter <- function(repo,parameter,value)
  { sparqlupdate(
    "PREFIX sys:  <http://www.openrdf.org/config/repository#>",
    "PREFIX sail: <http://www.openrdf.org/config/repository/sail#>",
    "PREFIX onto: <http://www.ontotext.com/trree/owlim#>",
    "DELETE { GRAPH ?g {?sail ?param ?old_value } }",
    "INSERT { GRAPH ?g {?sail ?param ?new_value } }",
    "WHERE {",
    "  GRAPH ?g { ?rep sys:repositoryID ?id . }",
    "  GRAPH ?g { ?rep sys:repositoryImpl ?delegate . }",
    "  GRAPH ?g { ?delegate sys:repositoryType ?type . }",
    "  GRAPH ?g { ?delegate sys:delegate ?impl . }",
    "  GRAPH ?g { ?impl sail:sailImpl ?sail . }",
    "  GRAPH ?g { ?sail ?param ?old_value . }",
    "  FILTER( ?id = \"",repo,"\" ) .",
    "  FILTER( ?param = ",param," ) .",
    "  BIND( ",value," AS ?new_value ) .",
    "}",
    endpoint=gsub("(.*/)(.*)","\\1SYSTEM/statements",current_endpoint,perl=TRUE)
    )
  }

get_graphdb_timeout <- function ()
{ system_endpoint <- gsub("(.*/)\\S+", "\\1SYSTEM", current_endpoint);
  repo <- gsub(".*/(\\S+)", "\\1", current_endpoint)
  queryc("select ?timeout ",
         "WHERE ",
         " { ?ctx a rep:RepositoryContext.",
         "   graph ?ctx",
         paste0("   {  _:a rdfs:label \"",repo,"\"."),
         "      _:a  a rep:Repository.",
         "     ?place owlim:query-timeout ?timeout.",
         "     }",
         "   }",
         endpoint=system_endpoint,cache=FALSE)
}

set_graphdb_timeout <- function (seconds)
{ system_update_endpoint <- gsub("(.*/)\\S+", "\\1SYSTEM/statements", current_endpoint);
  repo <- gsub(".*/(\\S+)", "\\1", current_endpoint)
  sparqlUpdate("DELETE { graph ?ctx ",
              "   {?place owlim:query-timeout ?timeout.} } ",
              "INSERT { graph ?ctx ",
              paste0("   {?place owlim:query-timeout \"",seconds,"\". }}"),
              "WHERE ",
              " { ?ctx a rep:RepositoryContext.",
              "   graph ?ctx",
              paste0("   {  _:a rdfs:label \"",repo,"\"."),
              "      _:a  a rep:Repository.",
              "     ?place owlim:query-timeout ?timeout.",
              "     }",
              "   }",
              endpoint=system_update_endpoint)
}


## How can I script (automate) the creation of remote repositories?
## This can be achieved on the command line using a repository configuration file (usually in Turtle format) and curl. The following steps must be followed:

## Create a repository configuration file containing the repository ID and its configuration parameters, e.g. filename config.ttl containing:
## @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
## @prefix rep: <http://www.openrdf.org/config/repository#>.
## @prefix sr: <http://www.openrdf.org/config/repository/sail#>.
## @prefix sail: <http://www.openrdf.org/config/sail#>.
## @prefix owlim: <http://www.ontotext.com/trree/owlim#>.

## [] a rep:Repository ;
##    rep:repositoryID "my_repo_id" ;
##    rdfs:label "Description of my repository" ;
##    rep:repositoryImpl [
##      rep:repositoryType "openrdf:SailRepository" ;
##      sr:sailImpl [
##        owlim:ruleset "owl-horst-optimized" ;
##        sail:sailType "owlim:Sail" ;
##        owlim:base-URL "http://example.org/owlim#" ;
##        owlim:repository-type "file-repository" ;
##       ]
##    ].
## Update the Sesame SYSTEM repository with this configuration by issuing the following on the command line (all in one line) replacing the filename (config.ttl), URL to the remote server's SYSTEM directory (http://localhost:8080/openrdf-sesame/repositories/SYSTEM) and a unique context in which to put the repository configuration (http://example.com#g1):
## curl -X POST -H "Content-Type:application/x-turtle"
##     -T config.ttl
##     http://localhost:8080/openrdf-sesame/repositories/SYSTEM/rdf-graphs/service?graph=http://example.com#g1
## Finally, update the SYSTEM repository with a single statement to indicate that the unique context is an instance of sys:RepositoryContext:
## curl -X POST -H "Content-Type:application/x-turtle"
##     -d "<http://example.com#g1> a <http://www.openrdf.org/config/repository#RepositoryContext>."
##     http://localhost:8080/openrdf-sesame/repositories/SYSTEM/statements
