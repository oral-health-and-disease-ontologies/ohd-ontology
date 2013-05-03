years_difference_from_dates <- function(first_date,second_date)
  {   start <- as.Date(first_date)
      end <- as.Date(second_date)
      (end-start)/365.25
    }

      
mean_and_sd_from_dates_in_years <- function (first_date,second_date,print=TRUE)
{  
  years <- years_difference_from_dates(first_date,second_date);
  if (print)  { cat(paste("mean is ", mean(years), " and standard deviation is ",sd(years),"\n"))}
  c(mean,sd);
}

histogram_fit_to_distribution <- function ()
{
  ages_histogram <- hist(as.numeric(years),breaks=50,plot=FALSE)
  age_normal_distribution <- fitdistr(ages,"normal")

  age_mean = age_normal_distribution$estimate[1]
  age_sd = age_normal_distribution$estimate[2]
}

age_to_first_treatment_statistics <- function ()
{ queryRes <- queryc("SELECT ?patient (sample(?birth_date) as ?bdate) (min(?treatdatei) as ?treatdate)
	   WHERE 
	   { ?patient rdf:type dental_patient: . 
	     ?patient participates_in: ?procedure. 
             ?procedure rdf:type dental_procedure: .
	     ?procedure occurrence_date: ?treatdatei .
   	     ?patient birth_date: ?birth_date
           } group by ?patient");
  mean_and_sd_from_dates_in_years(queryRes[,"bdate"],queryRes[,"treatdate"]);
}
