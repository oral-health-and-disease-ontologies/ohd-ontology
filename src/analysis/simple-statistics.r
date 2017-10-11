## Author: Alan Ruttenberg
## Project: OHD
## Date: May, 2013
##
## Demonstrates simple statistics on our data set with R. Here draw distribution of age at first procedure.
## 
## Modifed by Bill Duncan Oct. 10, 2017
## Summary of changes:
## After updating to GraphDB SE 8.3 and R 3.4.2, the endpoint url was changed and the rrdf library no
## no longer worked. To fix, I updated the current_endpoint in environment.r and I am using the SPARQL
## library (i.e., current_sparqlr <<- "SPARQL") for queries. 
## Two important side effects:
## 1. In order to get dates to work, I had to cast them as strings; e.g.: bind(str(?birth_date) as ?bdate)
## 2. To count rows in results, use ncol instead of nrows
##
## In the SPARQL query, I also changed the results from returning a sample (i.e., SAMPLE(?birth_date))
## to returning all birth dates. The labels on the plot were updated to reflect this.

years_difference_from_dates <- function(first_date,second_date)
  { start <- as.Date(first_date)
    end <- as.Date(second_date)
    as.numeric((end-start)/365.25)
  }

mean_median_and_sd_from_dates_in_years <- function (first_date,second_date,print=TRUE)
{
  years <- years_difference_from_dates(first_date,second_date);
  if (print)  { cat(paste("mean: ", mean(years),", median: ", median(years),  ", standard deviation: ",sd(years),"\n"))}
  c(as.numeric(mean(years)),as.numeric(median(years)),as.numeric(sd(years)));
}

#  http://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
estimate_beta_distribution_parameters <- function(mean, sd) {
  var <- sd*sd;
  alpha <- ((1 - mean) / var - 1 / mean) * mean ^ 2
  beta <- alpha * (1 / mean - 1)
  return(params = list(shape1 = alpha, shape2 = beta))
}

# show a histogram of date differences with a beta function fit to it
histogram_fit_to_distribution_of_dates_in_years <- function (first_date,second_date,breaks=20,topic="times",subtitle="")
{
  # compute years different
  years <- years_difference_from_dates(first_date,second_date);
  count <- length(years)
  # pick arbitrary bigger than we expect, so we can scale 0-1
  years_max = 110;
  # scale years to fit beta
  years_scaled <- years/years_max;
  # estimate the parameters from the means and sd
  beta_parameter_estimates <- estimate_beta_distribution_parameters(mean(years_scaled),sd(years_scaled));
  # I'm not sure if this is right. How to scale the fit to the counts?
  distscale <- count/breaks;
  # compute the beta distribution
  years_distribution <- fitdistr(years_scaled,"beta",beta_parameter_estimates);
  # histogram it
  hist(as.numeric(years),breaks=breaks,
       main=paste("Distribution of ",topic,", fit to",expression("beta"),"distribution"),
       xlab="Years",
       ylab="Count",
       sub=subtitle
  );
  # draw the function on top of it
  curve(distscale*dbeta(x/years_max,years_distribution$estimate[1],years_distribution$estimate[2]),add=TRUE)
  years_distribution
}

age_to_first_treatment_statistics <- function ()
{
  # retrieve a row for each patient with the birth date first treatment date for each
  queryRes <- queryc("SELECT ?patient (str(sample(?birth_date)) as ?bdate) (str(min(?treatdatei)) as ?treatdate)
                     WHERE
                     { 
                      ?patient rdf:type dental_patient: .
                      ?patient participates_in: ?procedure.
                      ?procedure rdf:type dental_procedure: .
                      ?procedure occurrence_date: ?treatdatei .
                      ?patient birth_date: ?birth_date
                     } group by ?patient");

  # currently the date fields are strings
  # converty them to dates
  queryRes[,"bdate"] <- as.Date(queryRes[,"bdate"]);
  queryRes[,"treatdate"] <- as.Date(queryRes[,"treatdate"]);
  
  # count how many procedures had a patient participate
  query <-  "SELECT distinct ?procedure
             WHERE
             {  ?patient rdf:type dental_patient: .
                ?patient participates_in: ?procedure.
                ?procedure rdf:type dental_procedure: .
                ?procedure occurrence_date: ?treatdatei .
                ?patient birth_date: ?birth_date .
              } "
  if(current_sparqlr == "rrdf")
    { withpatient <- nrow(queryc(query)) }
  else
    { withpatient <- ncol(queryc(query)) }
  
  ## count the total procedures
  if(current_sparqlr == "rrdf")
    { total <- nrow(queryc("SELECT distinct ?procedure WHERE { ?procedure rdf:type dental_procedure:  . } "));}
  else # use when current_sparqlr == "SPARQL"
    { total <- ncol(queryc("SELECT distinct ?procedure WHERE { ?procedure rdf:type dental_procedure:  . } "));}
  
  # compute mean, median, sd
  meansd<-mean_median_and_sd_from_dates_in_years(queryRes[,"bdate"],queryRes[,"treatdate"]);
  
  # visualize
  histogram_fit_to_distribution_of_dates_in_years(
    queryRes[,"bdate"],queryRes[,"treatdate"],
    topic="age at first treatment",
    subtitle=paste("median=",signif(meansd[2],3),"sd=",signif(meansd[3],3),"  ",
                   # summarize how many first treatments, summarized from how many treatments, from how many total treatments
                   dim(queryRes)[1],"first from",withpatient,"procedures of ",total)
  );
}

age_at_first_dental_procedure_statistics <- function ()
{ # retrieve a row for each patient with the birth date first procedure date for each
  queryRes <- queryc("
     SELECT 
        ?patient 
        (str(?birth_date) as ?bdate) 
        (min(?age) as ?first_procedure_age)
        (str(min(?procdatei)) as ?procdate) 
	   WHERE { 
        ?patient rdf:type dental_patient: .
        ?patient participates_in: ?procedure.
        ?procedure rdf:type dental_procedure: .
        ?procedure occurrence_date: ?procdatei .
        ?patient birth_date: ?birth_date .
        bind(year(?procdatei)-year(?birth_date) as ?age) .
      } 
      group by ?patient ?birth_date");

  values <- c(queryRes[,"first_procedure_age"]);
  hist(
    values
    #, breaks = seq(0,100, by = 10)
    #, labels = TRUE
    , main = "Distribution of patient's age during first dental procedure \n (fit to normal distribution)"
    , sub = paste("N:", format(length(values), big.mark = ","),"patients", 
                  " mean age:", round(mean(values), digits = 2), 
                  " SD age:", round(sd(values), digits = 2))
    , xlab = "Patient age"
    #, ylim = c(0, 200)
    , ylim = c(0, 0.03)
    , xlim = c(0, 100)
    , freq = FALSE
    , col= "lightgreen"
    , font.lab = 2
  )
  
  # lines(density(values), col="darkblue", lwd = 3) 
  curve(dnorm(x, mean=mean(values), sd=sd(values)), add=TRUE, col="darkblue", lwd = 3)
}

which_distribution_for_ages <- function ()
{   queryRes <- queryc("
     SELECT ?patient ?bdate (str(min(?procdatei)) as ?procdate)
	   WHERE
	   { 
       ?patient rdf:type dental_patient: .
	     ?patient participates_in: ?procedure.
       ?procedure rdf:type dental_procedure: .
	     ?procedure occurrence_date: ?procdatei .
   	   ?patient birth_date: ?birth_date
       bind(str(?birth_date) as ?bdate)
      } 
      group by ?patient ?bdate");

    compare_normal_to_beta_distribution(
      years_difference_from_dates(as.Date(queryRes[,"bdate"]),
      as.Date(queryRes[,"procdate"]))/115);
}


# See http://en.wikipedia.org/wiki/Kolmogorov–Smirnov_test - a way of testing goodness of fit of a probability distribution to a sample
compare_normal_to_beta_distribution <- function (data)
{ beta_parameter_estimates <- estimate_beta_distribution_parameters(mean(data),sd(data));
  # compute the beta distribution
  beta_parameters <- fitdistr(data,"beta",beta_parameter_estimates);
  # compute the normal distribution
  normal_parameters <- fitdistr(data,"normal")
  # Now do the KS test for the normal distribution and print
  cat("Normal\n");
  print(ks.test(data,"pnorm",normal_parameters$estimate[1],normal_parameters$estimate[2]))
  # And do the KS test for the beta distribution and print
  cat("Beta\n");
  print(ks.test(data,"pbeta",beta_parameters$estimate[1],beta_parameters$estimate[2]))
}


