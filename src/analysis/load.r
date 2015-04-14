## Author: Alan Ruttenberg
## Project: OHD
## Date: May, 2013

# necessary packages: SPARQL, RCurl, bitops

# you might want to compiler::enableJIT(2) in your ~/.Rprofile

# Variables to customize:
# Customize these variables by either setting them in your ~/.Rprofile, or setting them after loading this file.

# current_endpoint default: dungeon_r21_nightly "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"
# current_sparqlr default: "rrdf", can be changed to "SPARQL" to use the SPARQL R package instead (which is slower)

# use function queryc to do a query, which uses the current endpoint and the auto adds prefixes()

# To install, use the R GUI to install either rrdf or SPARQL. Check off "Install Dependencies" before installing.

# To load, use source("~/repos/ohd-ontology/trunk/src/analysis/load.r",chdir=T)
# of course change the path to adjust to where your repo is
# Demo: age_to_first_treatment_statistics()

source("SPARQL.R") ; # patch to SPARQL.R
.GlobalEnv[["interpret_type"]]=interpret_rdf_type;

source("packages.r");
source("prefixes.r"); #list of PREFIXes
source("environment.r"); # endpoints etc
source("simple-statistics.r") # simple statistical functions on the repo
source("summary.r") # 
