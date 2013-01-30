source("eaglesoft-caplan-functions.r")

make.caplan.spreadsheet <-
  function(limit.rows="", patient.id="", filter="", write.spreadsheet=TRUE, write.matrix=TRUE,
           spreadsheet.file.name="~/Desktop/caplan.spreadsheet.txt",
           matrix.file.name="~/Desktop/caplan.matrix.txt", print.query=FALSE, print.results=FALSE) {

    ## get triples
    res <- get.caplan.data(limit.rows, patient.id, print.query, filter)

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
  }
           
