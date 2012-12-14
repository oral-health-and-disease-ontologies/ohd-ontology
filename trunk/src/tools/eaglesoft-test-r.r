source("eaglesoft-load-r-variables.r")

# get list of patient ids
# note: the query is ordered by patient id
patients <- sparql.remote(owlim_se_r21, patient_list_query)

# get count of patients
patient_count <-  length(patients)
#print(patient_count)

for (i in 1:1) {
  # get patient id
  patientid <- patients[i]
  print(patientid)

  # make query for patient by substituting the patient id
  patient_query <- sub("#patientid#", patientid, r21_query_by_patient)
  #print(patient_query)

  # get results for that patient
  # results are returned as matrix
  res <-  sparql.remote(owlim_se_r21, patient_query)
  #print(res)

  # cast results in to data frame
  # I do this b/c the res is a matrix and cannot have different data types
  # I need different data types in order to sort by date
  df <- as.data.frame(res)

  # dates in df are in form "YYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
  # so I need to convert to R dates
  df$birthdate <- as.Date(df$birthdate)
  df$procdate <- as.Date(df$procdate)
  
  # order results by tooth number and procedure / finding date
  df.ordered <- df[order(df$tthnum, df$procdate), ]
  
  #order results by procedure / finding date

  print(df.ordered)
}
