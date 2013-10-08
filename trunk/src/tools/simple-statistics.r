# load necessary libraries for sparql
library(rrdf)
library(MASS)

# variables for connecting to triple store
#owlim_se_r21 <- "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2012.12.05/query"
owlim_se_r21 <- "http://localhost:8080/openrdf-workbench/repositories/ohd-r21-nightly/query"
owlim_se_r21_remote <- "http://dungeon.ctde.net:8080/openrdf-sesame/repositories/ohd-r21-nightly"

#current <- owlim_se_r21_remote
current <- owlim_se_r21

queryc <- function(string) { query(querystring(string),current) }

# strings for prefixes
prefixes <- function  ()
{
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX obo: <http://purl.obolibrary.org/obo>
PREFIX dental_patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX birth_date:  <http://purl.obolibrary.org/obo/OHD_0000050>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
"}

querystring <- function(string)
  { paste(prefixes(),"\n",string,"\n") }

# strings for finding patients
# 
patients_query_count <- querystring  (
"SELECT (count(distinct ?patientid) as ?count)
WHERE { 
?patienti rdf:type dentalpatient: . 
?patienti rdfs:label ?patientid . 
} ")


queryRes <- queryc("SELECT ?patient ?date WHERE { ?patienti rdf:type dental_patient: . ?patienti birth_date: ?date}")

birthdates <- as.Date(queryRes[,2])

now <- Sys.Date()

ages <- (now-birthdates)/365.25

ages_histogram <- hist(as.numeric(ages),breaks=50,plot=FALSE)

age_normal_distribution <- fitdistr(ages,"normal")

age_mean = age_normal_distribution$estimate[1]
age_sd = age_normal_distribution$estimate[2]

cat(paste("age mean is ", age_mean, " and age standard deviation is ",age_sd,"\n"))

