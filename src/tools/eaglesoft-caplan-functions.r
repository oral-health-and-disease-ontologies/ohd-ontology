## load variables 
source("eaglesoft-caplan-variables.r")

# load necessary libraries
library(rrdf)
library(foreign)

## function for converting caplan text files to SAS files
convert.caplan.file.to.sas <- function(file.name,
                                       sas.data.file="~/Desktop/caplan.sas.data.txt",
                                       sas.code.file="~/Desktop/caplan.code.sas") {
  ## read data from the text file into a matrix
  caplan.data <- as.matrix(read.table(file.name, na.string=c(".", " ", ""),
                                      header=TRUE, fill=TRUE, strip.white=TRUE))

  ## put data into data frame
  caplan.df <- as.data.frame(caplan.data)

  ## coerce date columns
  column.names <- colnames(caplan.df)
  for (name in column.names) {
    if(length(grep("DATE", name, ignore.case=TRUE)) > 0) {
      caplan.df[[name]] <- as.Date(caplan.df[[name]], format="%m-%d-%Y")
    }
  }
  
  ## write data in SAS format
  write.foreign(caplan.df, sas.data.file, sas.code.file, package="SAS")
}

## function for writing SAS file from the dataframe
write.caplan.sas <- function(caplan.df,
                             sas.data.file="~/Desktop/caplan.sas.data.txt",
                             sas.code.file="~/Desktop/caplan.code.sas") {
  ## coerce date columns
  column.names <- colnames(caplan.df)
  for (name in column.names) {
    if(length(grep("DATE", name, ignore.case=TRUE)) > 0) {
      caplan.df[[name]] <- as.Date(caplan.df[[name]], format="%m-%d-%Y")
    }
  }
  
  ## write data in SAS format
  write.foreign(caplan.df, sas.data.file, sas.code.file, package="SAS")
}

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
  
  ## create ragged list and data frame from the results
  ragged.list <- get.caplan.ragged.list(results)
  ragged.df <- get.caplan.ragged.data.frame(ragged.list)

  ## replace NA's with "."
  ragged.df <-  fill.missing.caplan.values(ragged.df)

  ##print(ragged.df)
  
  ## save to file
  write.table(ragged.df, file=file.name, sep="\t",
              col.names=TRUE, row.names=FALSE, append=FALSE)
}

get.caplan.ragged.list <- function(results, print.list=FALSE) {
  ## initialize list to store ragged arrays
  ## and determine length of results
  results.list <- list()
  results.length <- nrow(results)

  ## get first row of results and iterate
  ## over the rest of results
  row <- results[1, ]
  for (i in 2: results.length) {
    temprow <- results[i, ]
    
    ## compare patient ids (index 1) and tooth (index 4) between row and temprow
    ## if they match then "append" info to row
    ## otherwise add row to list and set row to temprow (this moves the row forward)
    if (row[1] == temprow[1] && row[4] == temprow[4]) {
      row <- c(row, temprow[5:18])
    } else {
      results.list <- c(results.list, list(row))
      row <- temprow
    }

    ## check to see if we on last record, if so add row to list
    if (i == results.length) {
      results.list <- c(results.list, list(row))
    }
  }

  ## make each element in list to be of equal length
  ## fill missing values with an NA
  list.max <- max(sapply(results.list, length))
  for (i in 1:length(results.list)) {
    length(results.list[[i]]) <- list.max
  }
  
  if (print.list == TRUE) {
    print(results.list)
  }
  
  invisible(results.list)
}

get.caplan.ragged.data.frame <- function(caplan.list) {
  caplan.df <- NULL
  for (i in 1:length(caplan.list)) {
    caplan.df <- rbind(caplan.df, caplan.list[[i]], deparse.level=0)
  }

  ## convert to data frame
  caplan.df <- as.data.frame(caplan.df, stringsAsFactors=FALSE, row.names=NULL)

  ## put column names on data frame
  colnames(caplan.df) <- get.caplan.column.names(caplan.df)
  
  invisible(caplan.df)
}

get.caplan.column.names <- function(caplan.df) {
  ## in order to save in SAS format column/variable names can only be 8 characters
  spreadsheet.column.names <-
    c("ID", "SEX", "BDATE", "TTHNUM",
      "PDATE1", "PCLASS1", "PCODE1", "MATM1", "MATO1", "MATD1", "MATF1", "MATL1", "DXM1", "DXO1", "DXD1", "DXF1", "DXL1", "DENT1",
      "PDATE2", "PCLASS2", "PCODE2", "MATM2", "MATO2", "MATD2", "MATF2", "MATL2", "DXM2", "DXO2", "DXD2", "DXF2", "DXL2", "DENT2",
      "PDATE3", "PCLASS3", "PCODE3", "MATM3", "MATO3", "MATF3", "MATF2", "MATL2", "DXM3", "DXO3", "DXD3", "DXF3", "DXL3", "DENT3",
      "PDATE4", "PCLASS4", "PCODE4", "MATM4", "MATO4", "MATD4", "MATF4", "MATL4", "DXM4", "DXO4", "DXD4", "DXF4", "DXL4", "DENT4",
      "PDATE5", "PCLASS5", "PCODE5", "MATM5", "MATO5", "MATD5", "MATF5", "MATL5", "DXM5", "DXO5", "DXD5", "DXF5", "DXL5", "DENT5",
      "PDATE6", "PCLASS6", "PCODE6", "MATM6", "MATO6", "MATD6", "MATF6", "MATL6", "DXM6", "DXO6", "DXD6", "DXF6", "DXL6", "DENT6",
      "PDATE7", "PCLASS7", "PCODE7", "MATM7", "MATO7", "MATD7", "MATF7", "MATL7", "DXM7", "DXO7", "DXD7", "DXF7", "DXL7", "DENT7",
      "PDATE8", "PCLASS8", "PCODE8", "MATM8", "MATO8", "MATD8", "MATF8", "MATL8", "DXM8", "DXO8", "DXD8", "DXF8", "DXL8", "DENT8",
      "PDATE9", "PCLASS9", "PCODE9", "MATM9", "MATO9", "MATD9", "MATF9", "MATL9", "DXM9", "DXO9", "DXD9", "DXF9", "DXL9", "DENT9",
      "PDATE10", "PCLASS10", "PCODE10", "MATM10", "MATO10", "MATD10", "MATF10", "MATL10", "DXM10", "DXO10", "DXD10", "DXF10", "DXL10", "DENT10")

  ## find length of data frame row
  row.length <- length(caplan.df[1, ])

  ## return portion of column names that match
  spreadsheet.column.names[1:row.length]
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
  ##results.ordered <- fill.missing.caplan.values(results.ordered)

  ## flip dates from YYYY-MM-DD to MM-DD-YYYY
  ## NB: do this after ordering rows!
  results.ordered <- flip.caplan.dates(results.ordered)

  ## put columns in order to match spreadsheet
  results.ordered <- order.caplan.columns(results.ordered)

  return(results.ordered)
}
