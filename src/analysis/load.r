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

source("packages.r");

source("SPARQL.R") ; # patch to SPARQL.R
.GlobalEnv[["interpret_type"]]=interpret_rdf_type;

source("prefixes.r"); #list of PREFIXes
source("environment.r"); # endpoints etc
source("sesame.r");
source("graphdb.r");
source("patch-triplestore.r");
source("simple-statistics.r"); # simple statistical functions on the repo
source("summary.r"); # 
#source("restoration-time-intervals.r")
#source("patient-statistics.r")
source("next-encounter.r");
source("event-pairs.r");
source("survival.r");
source("sparql-template.r");
source("sparql-patterns.r");
source("ggsurv.r");
source("check-integrity.r");
cat("Local repositories: \n");
# ugly way to print pretty
count<-0;
apply(as.matrix(get_sesame_repositories()[,"uri"]),
      FUN=function(el){cat(paste0(count<<-count+1,": ",el,"\n"))},
      MARGIN=1);
set_sesame_repository(2);
cat("\nset_sesame_repository(#) if you want to use a local repository listed above.\nDo the analysis with analysis<- resin_restoration_survival_analysis();\nPlot the results with plot(analysis)\n\n");
