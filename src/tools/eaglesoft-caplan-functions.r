## load variables 
source("eaglesoft-caplan-variables.r")

# load necessary libraries for sparql
library(rrdf)

## function for simply writing out matrix
write.caplan.matrix <- function(results, file.name="~/Desktop/caplan.matrix.txt") {
  ## delete file if it already exists
  if (file.exists(file.name) == TRUE) { file.remove(file.name) }

  ## write out matrix
  ## note: row need to be transposed
  write.table(results, file=paste(file.name), sep="\t",
              col.names=TRUE, row.names=FALSE, append=FALSE)
}

## function for writing spreadsheet in ragged array form
write.caplan.spreadsheet <- function(results, file.name="~/Desktop/caplan.spreadsheet.txt") {
  ## delete file if it already exists
  if (file.exists(file.name) == TRUE) { file.remove(file.name) }

  ## write column names
  spreadsheet.column.names <-
    c("ID", "SEX", "BIRTHDATE", "TTHNUM",
      "PROCDATE1", "PROCCLASS1", "PROCCODE1", "MATM1", "MATO1", "MATD1", "MATF1", "MATL1", "DXM1", "DXO1", "DXD1", "DXF1", "DXL1", "DENTIST1",
      "PROCDATE2", "PROCCLASS2", "PROCCODE2", "MATM2", "MATO2", "MATD2", "MATF2", "MATL2", "DXM2", "DXO2", "DXD2", "DXF2", "DXL2", "DENTIST2",
      "PROCDATE3", "PROCCLASS3", "PROCCODE3", "MATM3", "MATO3", "MATF3", "MATF2", "MATL2", "DXM3", "DXO3", "DXD3", "DXF3", "DXL3", "DENTIST3",
      "PROCDATE4", "PROCCLASS4", "PROCCODE4", "MATM4", "MATO4", "MATD4", "MATF4", "MATL4", "DXM4", "DXO4", "DXD4", "DXF4", "DXL4", "DENTIST4",
      "PROCDATE5", "PROCCLASS5", "PROCCODE5", "MATM5", "MATO5", "MATD5", "MATF5", "MATL5", "DXM5", "DXO5", "DXD5", "DXF5", "DXL5", "DENTIST5",
      "PROCDATE6", "PROCCLASS6", "PROCCODE6", "MATM6", "MATO6", "MATD6", "MATF6", "MATL6", "DXM6", "DXO6", "DXD6", "DXF6", "DXL6", "DENTIST6",
      "PROCDATE7", "PROCCLASS7", "PROCCODE7", "MATM7", "MATO7", "MATD7", "MATF7", "MATL7", "DXM7", "DXO7", "DXD7", "DXF7", "DXL7", "DENTIST7",
      "PROCDATE8", "PROCCLASS8", "PROCCODE8", "MATM8", "MATO8", "MATD8", "MATF8", "MATL8", "DXM8", "DXO8", "DXD8", "DXF8", "DXL8", "DENTIST8",
      "PROCDATE9", "PROCCLASS9", "PROCCODE9", "MATM9", "MATO9", "MATD9", "MATF9", "MATL9", "DXM9", "DXO9", "DXD9", "DXF9", "DXL9", "DENTIST9",
      "PROCDATE10", "PROCCLASS10", "PROCCODE10", "MATM10", "MATO10", "MATD10", "MATF10", "MATL10", "DXM10", "DXO10", "DXD10", "DXF10", "DXL10", "DENTIST10")

  ## write column names
  write.table(t(spreadsheet.column.names), file=paste(file.name), sep="\t",
              col.names=FALSE, row.names=FALSE, append=TRUE)
  
  ## get info about first row
  row <- results[1, ]
  ##print(row)

  ## get info about 2nd through next to last row
  for (i in 2:(nrow(results) - 1)) {
    temprow <- results[i, ]
    
    ##print(temprow)

    ## compare patient ids (index 1) and tooth (index 4) between row and temprow
    ## if they match then "append" info to row
    ## otherwise write to file and set row to temprow (this moves the row forward)
    if (row[1] == temprow[1] && row[4] == temprow[4]) {
      row <- c(row, temprow[5:18])
    } else {
      ## note: row need to be transposed
      write.table(t(row), file=paste(file.name), sep="\t",
                  col.names=FALSE, row.names=FALSE, append=TRUE)
      row <- temprow
    }
  }

  ## get info about last row
  temprow <- results[nrow(results), ]
  
  if (row[1] == temprow[1] && row[4] == temprow[4]) {
    row <- c(row, temprow[5:18])
  } else {
    row <- temprow
  }

  ## write last row
  ## note: row need to be transposed
  write.table(t(row), file=file.name, sep="\t",
              col.names=FALSE, row.names=FALSE, append=TRUE)
}

get.caplan.data.query <- function(limit="", patientid="", print.query=FALSE, filter="") {
  query.string <- ._caplan.data.query

  ## check for query on single patient or other custom filter
  if (nchar(patientid) > 0 || nchar(filter) > 0) {
    if (nchar(patientid) > 0) {
      query.string <- paste(query.string, ". FILTER (?patientid = \"", patientid, "\") ", sep="")
    } else {
      query.string <- paste(query.string, ". ", filter)
    }
  }

  ## paste ending ". }" to query string
  query.string <- paste(query.string, " . } ")
  
  ## check to limit results
  if (nchar(limit) > 0 ) { query.string <- paste(query.string, "LIMIT", limit) }

  ## check to print query to sting
  if (print.query == TRUE) { cat(query.string) }

  return(query.string)
}

get.caplan.patient.id.query <- function(print.query=FALSE) {
  query.string <- ._caplan.patient.id.list.query

  ## check to print query to sting
  if (print.query == TRUE) { cat(query.string) }

  return(query.string)
}

get.caplan.patient.id <- function(limit="", patientid="", print.query=FALSE, filter="", endpoint="local") {
  query.string <- get.caplan.patient.id.query(print.query)
  results <- get.caplan.sparql(query.string, endpoint)
}

get.caplan.data <- function(limit="", patientid="", print.query=FALSE, filter="", endpoint="local") {
  query.string <- get.caplan.data.query(limit, patientid, print.query, filter)
  results <- get.caplan.sparql(query.string, endpoint)
}

get.caplan.sparql <-  function(query.string, endpoint="local") {
  ## check to see what endpoint to use
  if (endpoint == "local") {
    url <-
      "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2013.01.21/query"
      ##"http://localhost:8080/openrdf-workbench/repositories/owlim-se-2013.01.21"
  } else if (endpoint == "remote") {
    url <-
      "http://den287.sdm.buffalo.edu:8080/openrdf-workbench/repositories/ohd-r21-nightly/query"
  } else {
    url <- endpoint
  }

  ## execute query and return results
  results <- sparql.remote(url, query.string)
  return(results)    
}

order.caplan.rows <- function(results) {
  ## order the results by patientid, tooth number, procedure / finding date
  ## NB: the "drop=FALSE" is to prevent dimensions from being deleted in cases
  ##     where results is vector
  results <-
    results[order(results[,"patientid"], results[,"tthnum"],results[,"procdate"]), ,drop=FALSE]

  return(results)
}

order.caplan.columns <- function(results) {
  results <- results[, ._caplan.column.names]
  return(results)
}

fill.missing.caplan.columns <- function(results) {
  ## get column names returned with results
  column.names <- colnames(results)
  
  ## check results matrix to see what columns have been returned from query
  ## if a column is missing put NA in column and assign name to column
  for (colnum in 1:18) {
    name <- ._caplan.column.names[colnum]
    if (length(grep(name, column.names)) < 1) {
      results <- cbind(results, NA)            ## put NA in column
      colnames(results)[ncol(results)] <- name ## give column the name
    }
  }

  return(results)
}

trim.caplan.dates <- function(results) {
  ## get column names returned with results
  results.column.names <- colnames(results)
  
  ## dates in results are in form "YYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
  ## so I need to lop off the ^^http://www.w3.org/2001/XMLSchema#date part
  if (grep("birthdate", results.column.names) > 0) {
    ## check the dimesnions 
    ##if (is.null(dim(results) || dim(results) == 1
    results[ , "birthdate"] <- substring(results[ , "birthdate"], 1, 10)
  }

  if (grep("procdate", results.column.names) > 0) {
    results[ , "procdate"] <- substring(results[ , "procdate"], 1, 10)
  }

  return(results)
}


flip.caplan.dates <- function(results) {
  ## flip dates from YYYY-MM-DD to MM-DD-YYYY
  ## NB: do this after ordering!
  results[, "birthdate"] <-
    paste(substring(results[, "birthdate"], 6, 7), "-",
          substring(results[, "birthdate"], 9, 10), "-",
          substring(results[, "birthdate"], 1, 4), sep="")

  results[, "procdate"] <-
    paste(substring(results[, "procdate"], 6, 7), "-",
          substring(results[, "procdate"], 9, 10), "-",
          substring(results[, "procdate"], 1, 4), sep="")

  return(results)
}

fill.missing.caplan.values <- function(results) {
  results[is.na(results)] <- "."
  return(results)
}

transform.caplan.data <- function(results) {
  ## fill in missing column names
  results <- fill.missing.caplan.columns(results)

  ## dates in res are in form "YYYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
  ## so I need to lop off the "^^http://www.w3.org/2001/XMLSchema#date" part
  results <- trim.caplan.dates(results)

  ## order results by patientid, tooth number, procedure / finding date
  results.ordered <- order.caplan.rows(results)

  ## replace all NA values with "."
  results.ordered <- fill.missing.caplan.values(results.ordered)

  ## flip dates from YYYY-MM-DD to MM-DD-YYYY
  ## NB: do this after ordering rows!
  results.ordered <- flip.caplan.dates(results.ordered)

  ## put columns in order to match spreadsheet
  results.ordered <- order.caplan.columns(results.ordered)

  return(results.ordered)
}
