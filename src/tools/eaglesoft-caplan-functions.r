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
              col.names=FALSE, row.names=FALSE, append=TRUE)
}

## function for writing spreadsheet in ragged array form
write.caplan.spreadsheet <- function(results, file.name="~/Desktop/caplan.spreadsheet.txt") {
  ## delete file if it already exists
  if (file.exists(file.name) == TRUE) { file.remove(file.name) }

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

get.caplan.query <- function(limit="", patientid="", print=FALSE, filter="") {
  query.string <- ._caplan.query

  ## check for query on single patient or other custom filter
  if (nchar(patientid) > 0 || nchar(filter) > 0) {
    if (nchar(patientid) > 0) {
      query.string <- paste(query.string, ". FILTER (?patientid = \"", patientid, "\") ")
    } else {
      query.string <- paste(query.string, ". ", filter)
    }
  }

  ## paste ending ". }" to query string
  query.string <- paste(query.string, " . } ")
  
  ## check to limit results
  if (nchar(limit) > 0 ) { query.string <- paste(query.string, " LIMIT ", limit) }

  ## check to print query to sting
  if (print == TRUE || print == T) { cat(query.string) }

  return(query.string)
}

get.caplan.sparql <-  function(query.string, endpoint="local") {
  ## check to see what endpoint to use
  if (endpoint == "local") {
    url <-
      "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2012.12.05/query"
      ##"http://localhost:8080/openrdf-workbench/repositories/owlim-se-2012.12.05"
  } else if (endpoint == "remote") {
    url <-
      "http://den287.sdm.buffalo.edu:8080/openrdf-workbench/repositories/ohd-r21-nightly/query"
  } else {
    url <- endpoint
  }

  ## execute query and return results
  results <- res <- sparql.remote(url, query.string)
  return(results)    
}

order.caplan.rows <- function(results) {
  ## order the results by patientid, tooth number, procedure / finding date
  results <-
    results[order(results[,"patientid"], results[,"tthnum"],results[,"procdate"]), ]

  return(results)
}
order.caplan.columns <- function(results) {
  results <- results[ , ._caplan.column.names]
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

