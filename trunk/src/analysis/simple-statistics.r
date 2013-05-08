## Author: Alan Ruttenberg
## Project: OHD
## Date: May, 2013
##
## Demonstrates simple statistics on our data set with R. Here draw distribution of age at first treatment.

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
{ # retrieve a row for each patient with the birth date first treatment date for each
  queryRes <- queryc("SELECT ?patient (sample(?birth_date) as ?bdate) (min(?treatdatei) as ?treatdate)
	   WHERE 
	   { ?patient rdf:type dental_patient: . 
	     ?patient participates_in: ?procedure. 
             ?procedure rdf:type dental_procedure: .
	     ?procedure occurrence_date: ?treatdatei .
   	     ?patient birth_date: ?birth_date
           } group by ?patient");
  
  # count how many procedures had a patient participate
  withpatient <- nrow(queryc("SELECT distinct ?procedure
  	   WHERE 
	   { ?patient rdf:type dental_patient: . 
	     ?patient participates_in: ?procedure. 
             ?procedure rdf:type dental_procedure: .
	     ?procedure occurrence_date: ?treatdatei .
   	     ?patient birth_date: ?birth_date
           } "))

  ## count the total procedures
  total <- nrow(queryc("SELECT distinct ?procedure WHERE { ?procedure rdf:type dental_procedure:  . } "));

  # compute mean, median, sd
  meansd<-mean_median_and_sd_from_dates_in_years(queryRes[,"bdate"],queryRes[,"treatdate"])

  # visualize
  histogram_fit_to_distribution_of_dates_in_years(
    queryRes[,"bdate"],queryRes[,"treatdate"],
    topic="age at first treatment",
    subtitle=paste("median=",signif(meansd[2],3),"sd=",signif(meansd[3],3),"  ",
      # summarize how many first treatments, summarized from how many treatments, from how many total treatments
      dim(queryRes)[1],"first from",withpatient,"procedures of ",total)
    );
}
