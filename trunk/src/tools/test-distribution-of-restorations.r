## query to return list of patients that have at least one restoration
## so we need distinct patientid birthdate
library(rrdf)

get.distribution.sparql <-  function(query.string, endpoint="local") {
  ## check to see what endpoint to use
  if (endpoint == "local") {

    url <-
      "http://localhost:8080/openrdf-workbench/repositories/ohd-r21-nightly/query"
  } else if (endpoint == "remote") {
    url <-
      "http://365-imac.sdm.buffalo.edu:8080/openrdf-workbench/repositories/ohd-r21-nightly/query"
  } else if (endpoint == "claudio-only") {
    url <-
      "http://localhost:8080/openrdf-workbench/repositories/owlim-se-claudio-only/query"
  } else {
    url <- endpoint
  }

  ## execute query and return results
  results <- sparql.remote(url, query.string)
  return(results)    
}

get.distribution.data <- function(limit="10", patientid="", print.query=FALSE, filter="", endpoint="local") {
  query.string <- get.distribution.data.query(limit, patientid, print.query, filter)
  results <- get.distribution.sparql(query.string, endpoint)
}

get.distribution.data.query <- function(limit="", patientid="", print.query=FALSE, filter="") {
  query.string <-
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX tooth_restoration_procedure: <http://purl.obolibrary.org/obo/OHD_0000004>
PREFIX tooth_to_be_restored_role: <http://purl.obolibrary.org/obo/OHD_0000007>
PREFIX realizes: <http://purl.obolibrary.org/obo/BFO_0000055>
PREFIX female_dental_patient: <http://purl.obolibrary.org/obo/OHD_0000049>
PREFIX dental_patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX male_dental_patient: <http://purl.obolibrary.org/obo/OHD_0000054>
PREFIX asserted_type: <http://purl.obolibrary.org/obo/OHD_0000092>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
PREFIX birth_date: <http://purl.obolibrary.org/obo/OHD_0000050>
PREFIX tooth: <http://purl.obolibrary.org/obo/FMA_12516>
PREFIX is_part_of: <http://purl.obolibrary.org/obo/BFO_0000050>

SELECT DISTINCT ?patienti ?birthdate
WHERE {
?patienti rdf:type dental_patient: . # get instances of dental patients
?patienti birth_date: ?birthdate . # and has a birth date

# get teeth that are part of the patient that restoration role iheres in
?toothi rdf:type tooth: . 
?toothi is_part_of: ?patienti .
?restorationrolei inheres_in: ?toothi . # that the restoration role inheres in

" ## note the "}" is missing; it is appended later

  ## check for query on single patient or other custom filter
  if (nchar(patientid) > 0 || nchar(filter) > 0) {
    if (nchar(patientid) > 0) {
      query.string <- paste(query.string, " FILTER (?patientid = \"patient ", patientid, "\") ", sep="")
    } else {
      query.string <- paste(query.string, " ", filter)
    }
  }

  ## paste ending "}" to query string
  query.string <- paste(query.string, " } ")
  
  ## check to limit results
  if (nchar(limit) > 0 ) { query.string <- paste(query.string, "LIMIT", limit) }
  
  ## check to print query to sting
  if (print.query == TRUE) {
    cat(query.string)
    cat("\n")
  }
}


     

