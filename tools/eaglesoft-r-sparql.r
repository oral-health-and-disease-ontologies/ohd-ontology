library(sparql)
source("eaglesoft-caplan-functions.r")

query.string <- get.caplan.query(limit="30")
query.url <- "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2012.12.05/query"
res <- query(query.string, query.url)


