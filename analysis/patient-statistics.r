## Author: Bill Duncan
## Project: OHD
## Date: April 10, 2015
##
## Summary: Statics on patients

## load plyr library
##library(dplyr)
source("load.r")

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

last.visit.summary <- function(limit=0) {
  
  ## get results
  df <- query.from.file("sparql/patient-visit-summary.sparql", limit)
  
  ## convert vist_date to date data type
  df$latest <- as.Date(df$latest)
  
  ## calculate elapsed time since last visit
  data.end.date <- as.Date("2011-12-31")
  df$elapsed.months <- round(as.numeric(difftime(data.end.date, df$latest, units="days"))/(365.25/12))

  ## calculate the patients last seen in 12, 24, 36, or greater months
  visit12 <- nrow(subset(df, elapsed.months <= 12))
  visit12to24 <- nrow(subset(df, elapsed.months > 12 & elapsed.months <= 24))
  visit24to36 <- nrow(subset(df, elapsed.months > 24 & elapsed.months <= 36))
  visit36plus <- nrow(subset(df, elapsed.months > 36))

  ## datafame of elapsed months
  elapsed <- c(visit12, visit12to24, visit24to36, visit36plus)
  rownames <- c("<= 12 months", "12 to 24 months", "24 to 36 months", "36+ months")
  info <- data.frame(elapsed.months=elapsed, row.names=rownames)
  
  ## make barplot
  barplot(info$elapsed.months, 
          main="Distribution of patient visits",
          xlab="Time period for last patient visit",
          ylab="Number of patients seen",
          col="blue",
          names.arg = row.names(info))
  ## return info dataframe
  info
}


patient.age.at.first.visit <- function(limit=0) {
  ## get results
  df <- query.from.file("sparql/patient-visit-summary.sparql", limit)
  
  ## convert vist_date to date data type
  df$earliest <- as.Date(df$earliest)
  
  ## calculate age of patient at first visit by taking the
  ## difference of the patient's birthdate and earliest visit
  df$age <- round(years_difference_from_dates(df$birth_date, df$earliest))
  
  ## remove records with age less than 0
  df <- subset(df, age > 0)
  
  ## we want only the patient and age columns
  df <- subset(df, select=c("patient", "age"))
  
  ## make barplot
  info <- table(df$age)
  barplot(info, 
          main="Summary of age of patient at first visit",
          xlab="Age of patient",
          ylab="Number of patients",
          col="blue")
  
  ## return data frame of summary of patient ages at first visit
  data.frame(patient.age=names(info), count=as.vector(info))
}
