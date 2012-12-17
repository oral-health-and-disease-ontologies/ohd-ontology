source("eaglesoft-load-r-variables.r")

## function for writing data
write.caplan.data <- function(results) {
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
      write.table(t(row), file="~/Desktop/r21.txt", sep="\t",
                  col.names=FALSE, row.names=FALSE, append=TRUE)
      row <- temprow
    }
  }
  ## get info about last row
  temprow <- res.caplan[nrow(res.caplan), ]
  
  if (row[1] == temprow[1] && row[4] == temprow[4]) {
    row <- c(row, temprow[5:18])
  } else {
    row <- temprow
  }

  ## write last row
  ## note: row need to be transposed
  write.table(t(row), file="~/Desktop/r21.txt", sep="\t",
              col.names=FALSE, row.names=FALSE, append=TRUE)
}


## column names in Caplan spreadsheet
column.names <- c("patientid", "sex", "birthdate", "tthnum", "procdate","procclass", "proccode",
                  "matm", "mato", "matd", "matf", "matl","dxm", "dxo", "dxd", "dxf", "dxl", "provider")

## get list of patient ids
## note: the query is ordered by patient id
res <- sparql.remote(owlim_se_r21, r21_query_all)

## get column names returned with res
res.column.names <- colnames(res)

## dates in res are in form "YYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
## so I need to lop off the ^^http://www.w3.org/2001/XMLSchema#date part
  if (grep("birthdate", res.column.names) > 0) {
  res[ , "birthdate"] <- substring(res[ , "birthdate"], 1, 10)
}
  
  if (grep("procdate", res.column.names) > 0) {
  res[ , "procdate"] <- substring(res[ , "procdate"], 1, 10)
}
        
## check matrix to see what columns have been returned from query
## if a column is missing put NA in column and assign name to column
for (colnum in 1:18) {
  column.name <- column.names[colnum]
  if (length(grep(column.name, res.column.names)) < 1) {
    res <- cbind(res, NA)                       ## put NA in column
    colnames(res)[ncol(res)] <- column.name ## give column the name
  }
}

## replace all NA values with "."
res[is.na(res)] <- "."

## order results by tooth number and procedure / finding date
res.ordered <- res[order(res[, "patientid"], res[, "tthnum"], res[, "procdate"]), ]

## flip dates from YYYY-MM-DD to MM-DD-YYYY
## NB: do this after ordering!
res.ordered[, "birthdate"] <-
  paste(substring(res.ordered[, "birthdate"], 6, 7), "-",
        substring(res.ordered[, "birthdate"], 9, 10), "-",
        substring(res.ordered[, "birthdate"], 1, 4), sep="")

res.ordered[, "procdate"] <-
  paste(substring(res.ordered[, "procdate"], 6, 7), "-",
        substring(res.ordered[, "procdate"], 9, 10), "-",
        substring(res.ordered[, "procdate"], 1, 4), sep="")

## order columns to match Caplan's spreadsheet
res.ordered <- res.ordered[, c("patientid", "sex", "birthdate", "tthnum", "procdate","procclass",
                               "proccode", "matm", "mato", "matd", "matf", "matl",
                               "dxm", "dxo", "dxd", "dxf", "dxl", "provider")]

## write results to file
write.caplan.data(res.ordered)
