## Author: Bill Duncan
## Project: OHD
## Date: April 6, 2015
##
## Summary: Statics on dental proceedures

patient.age.distribution <- function (limit=0) {
  query.string <- "
select distinct ?patient ?birth_date 
where 
{
  ?patienti rdf:type dental_patient: .
  ?patienti birth_date: ?birth_date .
  ?patienti rdfs:label ?patient . # assign label
 
  ## filter out bad data, patient 3057 has birth date of 2035-04-19
  filter(?patient != \"patient 3057\")
}
order by ?birth_date

"
  
  ## check for limit
  if (limit > 0)
    query.string <- paste(query.string," limit ", limit, " \n", sep="")

  ## get reults and convert to dataframe
  res <- queryc(query.string)
  df <- as.data.frame(res, stringsAsFactors = F)
  
  ## convert birth date column from string to data and calculate age
  ## note: use 12/31/2011 to calc age of patient
  df$birth_date <- as.Date(df$birth_date)
  data.end.date <- as.Date("2011-12-31")
  df$age <- round(as.numeric(difftime(data.end.date, df$birth_date, units="days")) / 365.25)
  
  info <-
  hist(df$age, 
       xlab="Patient Age", 
       ylab="Number of Patients", 
       main = "Number of patients by age",
       col="lightblue",
       freq=TRUE,
       right=TRUE,
       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110))
  
  #print(info)
  
  ## build data frame to return
  ## note: b/c the length of the counts and breaks lists are different, I only use the length
  ## of the counts list. the breaks list contains a "110" entry for which there are no patients
  df <- as.data.frame(info$counts, row.names=as.character(info$breaks[1:length(info$counts)]))
}