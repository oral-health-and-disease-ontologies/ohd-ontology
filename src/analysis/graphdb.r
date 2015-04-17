## http://owlim.ontotext.com/display/GraphDB6/GraphDB+FAQ#GraphDBFAQ-CannotconnecttheJMXclient%28jconsole%29totheApacheTomcatinstance.


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
