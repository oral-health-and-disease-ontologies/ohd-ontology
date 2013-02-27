source("eaglesoft-caplan-functions.r")

make.caplan.spreadsheet <-
  function(limit.rows="", patient.id="", filter="", endpoint="local",
           write.spreadsheet=TRUE, write.matrix=TRUE, write.sas=TRUE,
           spreadsheet.file.name="~/Desktop/caplan.spreadsheet.txt",
           matrix.file.name="~/Desktop/caplan.matrix.txt",
           sas.code.file="~/Desktop/caplan.code.sas",
           sas.data.file="~/Desktop/caplan.sas.data.txt",
           print.query=FALSE, print.results=FALSE) {
    
    ## get triples
    res <- get.caplan.data(limit.rows, patient.id, print.query, filter, endpoint)

    ## transform data into Caplan format
    res <- transform.caplan.data(res)

    if (write.spreadsheet == TRUE) {
      write.caplan.spreadsheet(res, spreadsheet.file.name)
    }

    if (write.matrix == TRUE) {
      write.caplan.matrix(res, matrix.file.name)
    }

    if (print.results == TRUE) {
      print(res)
    }

    if (write.sas == TRUE) {
      res.list <- get.caplan.ragged.list(res)
      res.df <- get.caplan.ragged.data.frame(res.list)
      res.df <- fill.missing.caplan.values(res.df)
      write.caplan.sas(res.df, sas.data.file=sas.data.file, sas.code.file=sas.code.file)
    }

    invisible(res)
  }

test.sas <- function(limit="5", col.limit=0, url="local") {
  res <- make.caplan.spreadsheet(limit.rows=limit, endpoint=url)

  if (col.limit > 0) {
    res <- res[, 1:col.limit]
  }
  
  res.list <- get.caplan.ragged.list(res)
  res.df <- get.caplan.ragged.data.frame(res.list)
  res.df <- fill.missing.caplan.values(res.df)

  #write.caplan.sas(res.df)
  invisible(res.df)
}

profile.test.sas <- function(limit="100") {
  Rprof("profilesas.txt")
  test.sas(limit=limit)
  Rprof(NULL)
}

make.caplan.spreadsheet.by.patient <-
  function(limit.rows="", patient.count=0, patient.id="", filter="",
           write.spreadsheet=TRUE, write.matrix=TRUE,
           spreadsheet.file.name="~/Desktop/caplan.spreadsheet.txt",
           matrix.file.name="~/Desktop/caplan.matrix.txt", print.query=FALSE, print.results=FALSE) {

    ## create matrix that will be used hold Caplan's results
    ## in the below code, results for each patient will be appended
    ## to this matrix
    res.caplan <- NULL

    ## get list of patient ids
    ## note: the query is ordered by patient id
    patients <- get.caplan.patient.id()
    
    ## get count of patients
    if (patient.count < 1) {
      patient.count <-  length(patients)
    }
    
    ##print(patient.count)
    for (i in 1:patient.count) {
      ## get patient id
      if (nchar(patient.id) > 0) {
        id <- patient.id
      } else {
        id <- patients[i]
      }
      
      ## get triples
      res <- get.caplan.data(limit.rows, id, print.query, filter)

      ## transform data into Caplan format
      res <- transform.caplan.data(res)

      ## append rows to matrix
      res.caplan <- rbind(res.caplan, res, deparse.level=0)
    }

    if (write.spreadsheet == TRUE) {
      write.caplan.spreadsheet(res.caplan, spreadsheet.file.name)
    }

    if (write.matrix == TRUE) {
      write.caplan.matrix(res.caplan, matrix.file.name)
    }

    if (print.results == TRUE) {
      print(res.caplan)
    }

    invisible(res.caplan)
  }
           
