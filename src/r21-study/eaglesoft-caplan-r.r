source("eaglesoft-caplan-functions.r")

## create matrix that will be used hold Caplan's results
## in the below code, results for each patient will be appended
## to this matrix
res.caplan <- NULL

## get list of patient ids
## note: the query is ordered by patient id
patients <- get.caplan.patient.id()
##print(patients)

## get count of patients
patient.count <-  length(patients)
##print(patient.count)
for (i in 1:2) {
##for (i in 1:patient.count) {
  ## get patient id
  id <- patients[i]
  ##print(id)

  ## get results for that patient
  ## results are returned as matrix
  ## note: an error is produced for empty results
  ##res <- get.caplan.data(patientid="patient 4297") # used for testing
  res <- get.caplan.data(patientid=id)

  
  ## make sure results where returned
  ## print(length(res))
  if (length(res) > 0) {
    ## check matrix to see what columns have been returned from query
    ## if a column is missing put NA in column and assign name to column
    res <- fill.missing.caplan.columns(res)
    
    ## dates in res are in form "YYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
    ## so I need to lop off the ^^http://www.w3.org/2001/XMLSchema#date part
    res <- trim.caplan.dates(res)

    ## order results by tooth number and procedure / finding date
    res.ordered <- order.caplan.rows(res)
    
    ## replace all NA values with "."
    res.ordered <- fill.missing.caplan.values(res.ordered)
    
    ## flip dates from YYYY-MM-DD to MM-DD-YYYY
    ## NB: do this after ordering!
    res.ordered <- flip.caplan.dates(res.ordered)
      
    ## order columns to match Caplan's spreadsheet
    res.ordered <- order.caplan.columns(res.ordered)
    
    ## append rows to matrix
    res.caplan <- rbind(res.caplan, res.ordered, deparse.level=0)
  }
}

print(res.caplan)


