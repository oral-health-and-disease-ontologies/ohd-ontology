source("eaglesoft-load-r-variables.r")

## create data frame the will be used hold Caplan's results
## in the below code, results for each patient will be appended
## to this data frame
df.caplan <- NULL

## column names in Caplan spreadsheet
column.names <- c("patientid", "sex", "birthdate", "tthnum", "procdate","procclass",
                  "proccode", "matm", "mato", "matd", "matf", "matl",
                  "dxm", "dxo", "dxd", "dxf", "dxl", "provider")

## get list of patient ids
## note: the query is ordered by patient id
patients <- sparql.remote(owlim_se_r21, patient_list_query)

## get count of patients
patient_count <-  length(patients)
#print(patient_count)

for (i in 1:1) {
  ## get patient id
  patientid <- patients[i]
  #print(patientid)

  ## make query for patient by substituting the patient id
  patient_query <- sub("#patientid#", patientid, r21_query_by_patient)
  #patient_query <- sub("#patientid#", "patient 100", r21_query_by_patient)

  #print(patient_query)

  ## get results for that patient
  ## results are returned as matrix
  res <-  sparql.remote(owlim_se_r21, patient_query)
  #print(res)

  ## cast results in to data frame
  ## I do this b/c the res is a matrix and cannot have different data types
  ## I need different data types in order to sort by date
  df <- as.data.frame(res)

  ## dates in df are in form "YYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
  ## so I need to convert to R dates
  df$birthdate <- as.Date(df$birthdate)
  df$procdate <- as.Date(df$procdate)
  
  ## order results by tooth number and procedure / finding date
  df.ordered <- df[order(df$tthnum, df$procdate), ]
  
  
  print(df.ordered)
  ## append rows to Caplan data frame
  ##df.caplan <- rbind(df.caplan, df.ordered)
  
}

## order results to match Caplan's spreadsheet
##df.ordered[c("patientid", "sex", "birthdate", "tthnum", "procdate","procclass",
##              "proccode", "matm", "mato", "matd", "matf", "matl",
##              "dxm", "dxo", "dxd", "dxf", "dxl", "provider")]


## write $results of dataframe to SAS file.  So, if my dataframe is df, the call would look like:
## write.foreign(df, "~/Desktop/r21.txt", "~/Desktop/r21.sas", package="SAS")
