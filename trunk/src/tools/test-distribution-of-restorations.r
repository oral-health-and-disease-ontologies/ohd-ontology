
library(rrdf) ## NOTE: rrdf does not work for aggregates!!!
library(SPARQL)

get.distribution.data <- function(limit="10", patientid="", print.query=FALSE, filter="", endpoint="local") {
  query.string <- get.distribution.data.query(limit, patientid, print.query, filter)
  results <- get.distribution.sparql(query.string, endpoint)
}

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
  ##results <- SPARQL(url=url, query=query.string)
  return(results)    
}

query.prefixes <- function() {
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
PREFIX occurrence_date: <http://purl.obolibrary.org/obo/OHD_0000015>
PREFIX tooth: <http://purl.obolibrary.org/obo/FMA_12516>
PREFIX is_part_of: <http://purl.obolibrary.org/obo/BFO_0000050>"
}

get.distribution.data.query <- function(limit="", patientid="", print.query=FALSE, filter="") {
  query.string <-
    "SELECT DISTINCT ?patienti ?birthdate
WHERE {
?patienti rdf:type dental_patient: . # get instances of dental patients
?patienti birth_date: ?birthdate . # and has a birth date

# get teeth that are part of the patient that restoration role iheres in
?toothi rdf:type tooth: . 
?toothi is_part_of: ?patienti .
?rolei rdf:type tooth_to_be_restored_role: .
?rolei inheres_in: ?toothi . # that the restoration role inheres in

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

  return(paste(query.prefixes(), query.string, sep="\n"))
}

test.aggregate.query <- function() {
  query.string <-
    "
SELECT ?patientid (MIN (?procdate) AS ?min_procdate)

WHERE {
  ?patient rdf:type dental_patient: .
  ?patient rdfs:label ?patientid .
  ?proci rdf:type tooth_restoration_procedure: .
  ?proci occurrence_date: ?procdate .
} GROUP BY ?patientid"
  

  return(paste(query.prefixes(), query.string, sep="\n"))
}

patient.restoration.count.query <- function() {
  query.string <-
    "
SELECT ?patientid ?role (COUNT (?proci) AS ?proc_count)

WHERE {
  # get instances of patients
  ?patienti rdf:type dental_patient: .
  ?patienti rdfs:label ?patientid .

  # get teeth that are part of the patient
  ?toothi rdf:type tooth: . 
  ?toothi is_part_of: ?patienti .

  # get roles that inhere in patients' teeth
  ?rolei rdf:type tooth_to_be_restored_role: .
  ?rolei inheres_in: ?toothi .
  ?rolei rdfs:label ?role .

  # get procures that realize tooth restoration roles
  ?proci rdf:type tooth_restoration_procedure: .
  ?proci realizes: ?rolei .

} GROUP BY ?patientid ?role
LIMIT 20"
  

  return(paste(query.prefixes(), query.string, sep="\n"))
}
