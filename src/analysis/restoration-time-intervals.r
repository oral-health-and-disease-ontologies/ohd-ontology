
## Author: Bill Duncan
## Project: OHD
## Date: Dec. 1, 2015
##
## Summary: Statics on the time intervals between restoration proceedures

source("SPARQL.R") ; # patch to SPARQL.R
source("environment.r") # load environment variables

## NB: this lines comes after sourcing above files!
.GlobalEnv[["interpret_type"]]=interpret_rdf_type;

read.sparql.file <- function (file, limit=0, print.query=FALSE){
  ## read contents from file 
  con <- file(file, "r", blocking = FALSE)
  sparql <- readLines(con)
  close(con)
  
  ## collapsed lines 
  sparql <- paste(sparql, sep = "", collapse="  \n ")
  
  ## check for limit
  if (limit > 0)
    sparql <- paste(sparql," limit ", limit, " \n", sep="")
  
  ## check for print to screen
  if (print.query) { cat(sparql) }
  
  ## return sparql query
  sparql
}

restoration.counts.by.tooth <- function (limit=0, print.query=FALSE) 
{
  ## build sparql query
  file <- "sparql/restoration-count-by-tooth.sparql"
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## get results and convert to dataframe; note: stringsAsFactors must be false
  res <- queryc(query.string)
  df <- as.data.frame(res, stringsAsFactors = F)
  
  ## when res is coverted to a data frame, the data types of the values are chars
  ## so, convert the count column into numeric data type
  mode(df$count) <- "numeric"
  
  ## add a column to the data frame the contains the integer that represents the tooth number
  ## e.g., for tooth type "Tooth 12", this column contains the number 12
  ## this is needed for ordering the data properly (i.e., by tooth number)
  df$toothnum <- match.tooth.position(df$toothtype)
  
  ## build summary table
  info <- tapply(df$count, df$toothnum, sum)
  
  ## create barplot of info
  barplot(info, 
          main="number of restorations per tooth",
          xlab="tooth number of patient", 
          ylab="number of restoration procedures",
          col="blue")
  
  ## return info
  info
}

non.failed.restorations <- function (limit=0, print.query=FALSE) {
  
  ## build sparql query, note: use querystring function to add prefixes
  file <- "sparql/non-failed-restorations.sparql"
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## get results and convert to dataframe; note: stringsAsFactors must be false
  res <- queryc(query.string)
  df <- as.data.frame(res, stringsAsFactors = F)    
}

average.time.to.restoration.failure <- function (limit=0, print.query=FALSE, days=FALSE)
{

  ## build sparql query
  file <- "sparql/first-and-second-restoration.sparql"
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## get results
  res <- queryc(query.string)
  
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

  ## build sparql query
  file <- "sparql/first-and-second-restoration.sparql"
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## get results and convert to dataframe; note: stringsAsFactors must be false
  res <- queryc(query.string)
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

average.time.to.restoration.failure.by.tooth <- function (limit=0, print.query=FALSE, days=FALSE, by.sex=FALSE)
{
  ## build sparql query
  file <- "sparql/first-and-second-restoration.sparql"
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## get results and convert to dataframe; note: stringsAsFactors must be false
  res <- queryc(query.string)
  df <- as.data.frame(res, stringsAsFactors = F)
  
  ## add column the data frame that contains the difference
  ## between the date1 and date2 in days
  df$datediff <- as.numeric(abs(floor(difftime(df$date2, df$date1, units = "days"))))
  
  ## add a column to the data frame the contains the integer that represents the tooth number
  ## e.g., for tooth type "Tooth 12", this column contains the number 12
  ## this is needed for ordering the data properly (i.e., by tooth number)
  df$toothnum <- match.tooth.position(df$toothtype)
  
  ## build table data of the mean time for restoration differences by tooth
  ## determine if results should be differentiated by sex
  if (by.sex==FALSE) {
    ## note: if days is false calculate average in years
    info <- tapply(df$datediff, df$toothnum, 
                   function(x) { ifelse(days==FALSE, mean(x) / 365.25, mean(x)) })
  } else {
    ## get subsets bases on sex
    females <- subset(df, patienttype == "female dental patient")
    males <- subset(df, patienttype == "male dental patient")
    
    ## get means for subsets
    females.mean <- tapply(females$datediff, females$toothnum,
                           function(x) { ifelse(days==FALSE, mean(x) / 365.25, mean(x)) })
    males.mean <- tapply(males$datediff, males$toothnum,
                         function(x) { ifelse(days==FALSE, mean(x) / 365.25, mean(x)) })
    
    ## combine male and female means
    info <- rbind(females.mean, males.mean)
  }
  
  ## set up custome labels for barplot
  x.label <- "tooth number of patient"
  
  if (days==FALSE) {
    y.label <- "mean years until failure"
    main.label <- "mean time in years until restoration failure by tooth"  
  } else {
    y.label <- "mean days until failure"
    main.label <- "mean time in days until restoration failure by tooth"
  }
  
  ## draw barplot of info
  if (by.sex==FALSE) {
    barplot(info, 
            main=main.label,
            xlab=x.label,
            ylab=y.label,
            col="red3")
  } else {
    barplot(info, 
            main=main.label,
            xlab=x.label,
            ylab=y.label,
            col=c("pink", "blue"),
            beside=TRUE)
    legend("topleft", c("female","male"), pch=15, 
          col=c("pink","blue"), bty="n")
  }
          
  ## return info about mean time to failure
  info
}

average.time.to.restoration.failure.by.surface <- function (limit=0, print.query=FALSE, days=FALSE, by.sex=FALSE, by.surface.letter=FALSE)
{
  
  ## build sparql query
  file <- "sparql/first-and-second-restoration.sparql"
  query.string <- read.sparql.file(file, limit, print.query)
  
  ## get results and convert to dataframe; note: stringsAsFactors must be false
  res <- queryc(query.string)
  df <- as.data.frame(res, stringsAsFactors = F)
  
  ## add column the data frame that contains the difference
  ## between the date1 and date2 in days
  df$datediff <- as.numeric(abs(floor(difftime(df$date2, df$date1, units = "days"))))
  
  ## add a column to the data frame the contains the normalized surface
  ## e.g., "Occlusial surface enamel of tooth" -> "occlusal" or "o"
  ## NB: you must call "unlist" for surfacenaem to be an atomic vector
  ##     this necessary for tapply to work (below)
  if (by.surface.letter==FALSE) {
      df$surfacename <- unlist(lapply(df$surfacetype, FUN = function(x) { match.surface.name(x)}))
  } else {
      df$surfacename <- unlist(lapply(df$surfacetype, FUN = function(x) { match.surface.letter(x)}))
  }
  
  ## build table data of the mean time for restoration differences by surface
  ## determine if results should be differentiated by sex
  if (by.sex==FALSE) {
    ## note: if days is false calculate average in years
    #info <- tapply(df$datediff, df$surfacename, 
    #               function(x) { ifelse(days==FALSE, mean(x) / 365.25, mean(x)) })
    print(str(df$surfacename))
    
    info <- tapply(df$datediff, df$surfacename, mean)
  } else {
    ## get subsets bases on sex
    females <- subset(df, patienttype == "female dental patient")
    males <- subset(df, patienttype == "male dental patient")
    
    print(head(females$surfacename))
    print(length(females$datediff))
    
    ## get means for subsets
    females.mean <- tapply(females$datediff, females$surfacename,
                           function(x) { ifelse(days==FALSE, mean(x) / 365.25, mean(x)) })
    
    males.mean <- tapply(males$datediff, males$surfacename,
                         function(x) { ifelse(days==FALSE, mean(x) / 365.25, mean(x)) })
    
    ## combine male and female means
    info <- rbind(females.mean, males.mean)
  }
  
  ## set up custome labels for barplot
  x.label <- "surface of tooth"
  
  if (days==FALSE) {
    y.label <- "mean years until failure"
    main.label <- "mean time in years until restoration failure by tooth"  
  } else {
    y.label <- "mean days until failure"
    main.label <- "mean time in days until restoration failure by tooth"
  }
  
  ## draw barplot of info
  if (by.sex==FALSE) {
    barplot(info, 
            main=main.label,
            xlab=x.label,
            ylab=y.label,
            col="red3")
  } else {
    barplot(info, 
            main=main.label,
            xlab=x.label,
            ylab=y.label,
            col=c("pink", "blue"),
            beside=TRUE)
    legend("topleft", c("female","male"), pch=15, 
           col=c("pink","blue"), bty="n")
  }
  
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
  
  ## get vector of postions for toothtypes
  positions <- match(tolower(toothtype.string), teeth, nomatch = 0)
  
  ## return position
  positions
}

match.surface.letter <- function (surfacetype.string) {
  ## cast to lower case
  surfacetype.string <- tolower(surfacetype.string)
  
  #### ***** Occlusal is MISPELLED
  if (surfacetype.string == "occlusial surface enamel of tooth") {
      surface.letter <- "o"
  } else if (surfacetype.string == "distal surface enamel of tooth") {
      surface.letter <- "d"
  } else if (surfacetype.string == "mesial surface enamel of tooth") {
      surface.letter <- "m"
  } else if (surfacetype.string == "buccal surface enamel of tooth") {
      surface.letter <- "b"
  } else if (surfacetype.string == "labial surface enamel of tooth") {
      surface.letter <- "f"
  } else if (surfacetype.string == "lingual surface enamel of tooth") {
      surface.letter <- "l"
  } else if (surfacetype.string == "incisal surface enamel of tooth") {
      surface.letter <- "i"  
  } else {
      surface.letter <- surfacetype.string
  }
  
  ## return the surface letter
  surface.letter
}

match.surface.name <- function (surfacetype.string) {
  ## cast to lower case
  surfacetype.string <- tolower(surfacetype.string)
  
  #### ***** Occlusal is MISPELLED
  if (surfacetype.string == "occlusial surface enamel of tooth") {
      surface.name <- "occlusal"
  } else if (surfacetype.string == "distal surface enamel of tooth") {
      surface.name <- "distal"
  } else if (surfacetype.string == "mesial surface enamel of tooth") {
      surface.name <- "mesial"
  } else if (surfacetype.string == "buccal surface enamel of tooth") {
      surface.name <- "buccal"
  } else if (surfacetype.string == "labial surface enamel of tooth") {
      surface.name <- "labial"
  } else if (surfacetype.string == "lingual surface enamel of tooth") {
      surface.name <- "lingual"
  } else if (surfacetype.string == "incisal surface enamel of tooth") {
      surface.name <- "incisal"
  } else {
      surface.name <- surfacetype.string
  }
  
  ## return the surface name
  surface.name
}
