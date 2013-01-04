source("eaglesoft-caplan-functions.r")

## get list of patient ids
##res.query <- get.caplan.query(limit="100")
res.query <- get.caplan.query("30")
res <- get.caplan.sparql(res.query)

## get column names returned with res
res.column.names <- colnames(res)

## dates in res are in form "YYYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
## so I need to lop off the "^^http://www.w3.org/2001/XMLSchema#date" part
if (grep("birthdate", res.column.names) > 0) {
  res[ , "birthdate"] <- substring(res[ , "birthdate"], 1, 10)
}
  
if (grep("procdate", res.column.names) > 0) {
  res[ , "procdate"] <- substring(res[ , "procdate"], 1, 10)
}
        
## fill in missing column names
res <- fill.missing.caplan.columns(res)

## order results by patientid, tooth number, procedure / finding date
res.ordered <- order.caplan.rows(res)

## replace all NA values with "."
res.ordered[is.na(res.ordered)] <- "."

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

res.ordered <- order.caplan.columns(res.ordered)

## write results to file
##write.caplan.spreadsheet(res.ordered)
##write.caplan.matrix(res.ordered)
