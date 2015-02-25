
## Author: Bill Duncan
## Project: OHD
## Date: Dec. 1, 2015
##
## Summary: Statics on the time intervals between restoration proceedures

source("SPARQL.R") ; # patch to SPARQL.R
.GlobalEnv[["interpret_type"]]=interpret_rdf_type;

source("environment.r") # load environment variables

average.time.to.restoration.failure <- function (limit=0, print.query=FALSE, days=FALSE)
{

  ## get restults
  res <- failed.restoration.results(limit, print.query)

  ## build matrix with only the restoration procedure dates
  dates <- matrix(c(res[,"date1"], res[,"date2"]), ncol=2)

  ## add column to dates matrix that contains the difference
  ## between the dates in days
  dates <- cbind(dates, abs(floor(difftime(res[,"date2"], res[,"date1"], units = "days"))))

  ## determine average number of days to failure
  ave.failure.days <- as.numeric(mean(as.numeric(dates[,3])))

  ##  determine average number of years to failure
  if(ave.failure.days > 0) {
      ave.failure.years <- ave.failure.days/365.25
  } else {
      ave.failure.years <- 0
  }

  ## if days is true return average in days
  if (days) {
      ave.failure.days
  } else {
      ave.failure.years
  }
}

average.time.to.restoration.failure.by.sex <- function (limit=0, print.query=FALSE, days=FALSE)
{

  ## get restults
  res <- failed.restoration.results(limit, print.query)

### *** This commented code below uses matrices to calculate mean durations
###      I'm keeping it around for posterity
        
  ## build matrix with the patient type (i.e., sex) and restoration procedure dates
#   data <- matrix(c(res[, "patienttype"], res[,"date1"], res[,"date2"]), ncol=3)
# 
#   ## add column to dates matrix that contains the difference
#   ## between the dates in days
#   data <- cbind(data, abs(floor(difftime(res[,"date2"], res[,"date1"], units = "days"))))
# 
#   ## add meaningful names
#   colnames(data) <- c("sex", "date1", "date2", "ave")
# 
#   ## split the data by the patient type and give groups more appropriate names
#   data.sex <- split(as.numeric(data[,"ave"]), data[,"sex"])
#   names(data.sex) <- c("female", "male")
# 
#   ## determine average number of days to failure for each group (female/male)
#   sex.mean <- lapply(data.sex, FUN=mean)

### *** Instead of using matrices (as above) to calculate means, use data frames
###     and tapply function to get mean restoration failure time

  ## build data frame from sparql result set, note: stringsAsFactors must be false
  df <- as.data.frame(res, stringsAsFactors = F)
  
  ## add column to data frame that contains the difference
  ## between the date1 and date2 in days
  df$datediff <- as.numeric(abs(floor(difftime(df$date2, df$date1, units = "days"))))

  ## build table data of the mean time for restoration differences by sex
  ## if days is false calculate average in years
  ## set up custome labels for barplot
  if (days==FALSE) {
    info <- tapply(df$datediff, df$patienttype, function(x) { mean(x) / 365.25 })
    y.label <- "mean years until failure"
    main.label <- "mean time in years until restoration failure by sex"  
  } else {
    info <- tapply(df$datediff, df$patienttype, mean)  
    y.label <- "mean days until failure"
    main.label <- "mean time in days until restoration failure by sex"
  }
 
  ##  common labels
  x.label <- "sex of patient"
  y.lim <- ceiling(max(info[[1]], info[[2]]))
#   print(info[[1]])
#   print(info[[2]])
#   print(y.lim)

  ## draw barplot of info
  barplot(info, 
          main=main.label,
          xlab=x.label,
          ylab=y.label,
          ylim=c(0, as.numeric(y.lim)),
          names.arg=c("female", "male"), 
          col=c("pink", "blue"))

  ## return info about mean time to failure
  info
 }

average.time.to.restoration.failure.by.tooth <- function (limit=0, print.query=FALSE, days=FALSE)
{
  
  ## get restults
  res <- failed.restoration.results(limit, print.query)
  
  ## build data frame from sparql result set, note: stringsAsFactors must be false
  df <- as.data.frame(res, stringsAsFactors = F)
  
  ## add column the data frame that contains the difference
  ## between the date1 and date2 in days
  df$datediff <- as.numeric(abs(floor(difftime(df$date2, df$date1, units = "days"))))
  
  ## add a column to the data frame the contains the integer that represents the tooth number
  ## e.g., for tooth type "Tooth 12", this column contains the number 12
  ## this is needed for ordering the data properly (i.e., by tooth number)
  df$toothnum <- match.tooth.position(df$toothtype)
  
  ## build table data of the mean time for restoration differences by tooth
  ## if days is false calculate average in years
  ## set up custome labels for barplot
  if (days==FALSE) {
    info <- tapply(df$datediff, df$toothnum, function(x) { mean(x) / 365.25 })
    y.label <- "mean years until failure"
    main.label <- "mean time in years until restoration failure by tooth"  
  } else {
    info <- tapply(df$datediff, df$toothnum, mean)  
    y.label <- "mean days until failure"
    main.label <- "mean time in days until restoration failure by tooth"
  }
  
  ##  common labels
  x.label <- "tooth number of patient"
  
  ## draw barplot of info
  barplot(info, 
          main=main.label,
          xlab=x.label,
          ylab=y.label,
          col=rainbow(32))
          
  ## return info about mean time to failure
  info
}

match.tooth.position <- function(toothtype.string)
{
  ## vector of teeth
  teeth <- c("tooth 1", "tooth 2", "tooth 3", "tooth 4", "tooth 5", "tooth 6", "tooth 7", "tooth 8",
             "tooth 9", "tooth 10", "tooth 11", "tooth 12", "tooth 13", "tooth 14", "tooth 15", "tooth 16", 
             "tooth 17", "tooth 18", "tooth 19", "tooth 20", "tooth 21", "tooth 22", "tooth 23", "tooth 24", 
             "tooth 25", "tooth 26", "tooth 27", "tooth 28", "tooth 29", "tooth 30", "tooth 31", "tooth 32")
  
  ## find postion of toothtype
  position <- match(tolower(toothtype.string), teeth, nomatch = 0)
  
  ## return position
  position
}

failed.restoration.results <- function (limit=0, print.query=FALSE)
{
  ## get query.string
  query.string <- first.and.second.restoration.query.string(limit)

  ## print query.string when print.query true
  if (print.query)
  {
    cat(query.string)
  }

  ## return restults
  queryc(query.string)
}

first.and.second.restoration.query.string <- function (limit=0)
{
    query.string <- "
select distinct ?patienttype ?birthdate ?tooth ?toothtype ?surface ?surfacetype ?procedure1 ?date1 ?procedure2 ?date2

## look for surface specific for now. Find two procedures and an optional third
where
{
    ## patient's sex and birth date
    ?patienti rdf:type patient: .
    ?patienti asserted_type: ?patienttypei .
    ?patienti birth_date: ?birthdate .

    ## patient's tooth & tooth type
    ?toothi rdf:type tooth: .
    ?toothi asserted_type: ?toothtypei .
    ?toothi is_part_of: ?patienti .

## surfaces and their types that are part of tooth
    ?surfacei rdf:type tooth_surface: .
    ?surfacei asserted_type: ?surfacetypei .
    ?surfacei is_part_of: ?toothi .

##- get restoration procedure in general
## this is done by finding the procedures that realize
## some tooth to be resotred role that is borne by the tooth
## the procedures occurrence date (?date) is used to determine
## the first restoration date by taking the min of ?date
## see having clause below
    ?proci rdf:type tooth_restoration_procedure: .
    ?rolei rdf:type tooth_to_be_restored_role: .

    ## the tooth to be restored role inheres in the tooth
    ## and is realized by the procedure
    ?rolei inheres_in: ?toothi .
    ?proci realizes: ?rolei .
    ?proci occurrence_date: ?date . # date of procedure

##- first procedure: the tooth is same as the general procedure above.
## the first procedure is determined in a manner similar to the
## general procedure above
    ?proci1 rdf:type tooth_restoration_procedure: .
    ?rolei1 rdf:type tooth_to_be_restored_role: .

    ## the tooth to be restored role inheres in the tooth
    ## and is realized by the procedure
    ?rolei1 inheres_in: ?toothi .
    ?proci1 realizes: ?rolei1 .
    ?proci1 occurrence_date: ?date1 . # date of procedure 1

    ## surfaces that have been restored particpate in the procedure
    ?proci1 has_participant: ?surfacei .

##- second process: the tooth and surface remain the same as the first
## but a new process that realizes a new role is searched for
    ?proci2 rdf:type tooth_restoration_procedure: .
    ?rolei2 rdf:type tooth_to_be_restored_role: .

    ## the tooth to be restored role inheres in the tooth
    ## and is realized by the procedure
    ?rolei2 inheres_in: ?toothi .
    ?proci2 realizes: ?rolei2 .
    ?proci2 occurrence_date: ?date2 . # date fo procedure 2

    ## surfaces that have been restored particpate in the procedure
    ?proci2 has_participant: ?surfacei .

## we only those second procedure that are after the first
filter (?date2 > ?date1 && ?proci1 != ?proci2)

##- third process: the tooth and surface remain the same,
## but the date is between the other two
## (we check later that this doesn't succeed)
   optional {
    ?proci3 rdf:type tooth_restoration_procedure: .
    ?rolei3 rdf:type tooth_to_be_restored_role: .

    ## the tooth to be restored role inheres in the tooth
    ## and is realized by the procedure
    ?rolei3 inheres_in: ?toothi .
    ?proci3 realizes: ?rolei3 .
    ?proci3 occurrence_date: ?date3 . # date of procedure 3

    ## surfaces that have been restored particpate in the procedure
    ?proci3 has_participant: ?surfacei .

    ## we want only that procedures that are between
    ## two other procedures
    filter (?date3<?date2 && ?date3>?date1)}

    ## assign labels
    ?patienttypei rdfs:label ?patienttype .
    ?toothi rdfs:label ?tooth .
    ?toothtypei tooth_number: ?toothtype .
    ?surfacei rdfs:label ?surface .
    ?surfacetypei rdfs:label ?surfacetype .
    ?proci1 rdfs:label ?procedure1 .
    ?proci2 rdfs:label ?procedure2 .

    ## we only those records where the in between date (?date3)
    ## is not bound, this gives us adjacent dates
    filter (!bound(?date3))
}
group by ?patienttype ?birthdate ?tooth ?toothtype ?surface ?surfacetype ?procedure1 ?date1 ?procedure2 ?date2
## match the min and first procedure dates
having (?date1 = min(?date))
order by ?tooth ?surface ?date1

"
  if (limit > 0)
     query.string <- paste(query.string,"limit ", limit, "\n", sep="")

query.string
}

