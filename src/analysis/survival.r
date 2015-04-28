## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##
## WIP survival analysis

library(survival)

rsurv <- function()
    { structure(list(),class="rsurv") }

print.rsurv <- function(object) { cat("<restoration survival analysis>\n") }

survival_posterior_vs_anterior <- function ()
    { s <- rsurv()
      s$all <- collect_all_restorations_and_latest_followup()
      s$fail <- collect_restoration_failures()
      create_surv(s)
  }


## take the set of all resin restorations and subtract out those which we know failed
subset_all_minus_fail <- function(all, fail)
{
  rownames(all)=all[,"proci1"]
  rownames(fail)=fail[,"proci1"]
  keep <- setdiff(rownames(all),rownames(fail));;
  res <- as.matrix(all[keep,]);
}


create_surv <- function (s)
    { 
        lastsa <<- s;
        no_record_of_failure <- NULL;

        difference_in_months <-
            function(start,end)
                { length(seq(as.Date(start),as.Date(end),by="month"))-1 }
        difference_in_years <-
            function(start,end)
                { length(seq(as.Date(start),as.Date(end),by="year"))-1 }
        no_record_of_failure <- subset_all_minus_fail(s$all,s$fail);
        s$have_last_visit <- no_record_of_failure[!is.na(no_record_of_failure[,"latest_date2"]),];
        s$no_record_of_failure_in_months <-
            mapply(difference_in_months,s$have_last_visit[,"date1"],s$have_last_visit[,"latest_date2"])
        s$age_at_last_visit <- mapply(difference_in_years,s$have_last_visit[,"birthdate"],s$have_last_visit[,"date1"])
        
        s$ncensor = length(s$no_record_of_failure_in_months);
        
        # should interval censor
        s$fail_difference_in_months <-
            mapply(difference_in_months,s$fail[,"date1"],s$fail[,"soonest_date2"])
        s$age_at_fail <- mapply(difference_in_years,s$fail[,"birthdate"],s$fail[,"date1"])

        s$nfail <- length(s$fail_difference_in_months);
        
        s$status <- c(rep(1,s$nfail),rep(0,s$ncensor));
        s$months <- c(s$fail_difference_in_months,s$no_record_of_failure_in_months)
        
        s$correlates <- rbind(data.frame(location=s$fail[,"tooth_type"],
                                         gender=s$fail[,"gender"],
                                         age=s$age_at_fail),
                              data.frame(location=s$have_last_visit[,"tooth_type"],
                                         gender=s$have_last_visit[,"gender"],
                                         age=s$age_at_last_visit))

        lastsa <<- s;
        s$correlates[,"age_group"] <- cut(s$correlates[,"age"],breaks=c(0,30,100));
        s$nlastvisit <- dim(s$have_last_visit)[1]

        surv <- Surv(s$months,s$status) ;
        s$surv <- surv;
        s$location_fit <- survfit(surv~location,conf.type="none",data=s$correlates);
        s$gender_fit <- survfit(surv~gender,conf.type="none",data=s$correlates);
        s$age_fit <- survfit(surv~age_group,conf.type="none",data=s$correlates);
        s
    }

plot.rsurv <- function (s,fit=s$location_fit)
    {
        if (file.exists("/tmp/rsvg.svg")) { file.remove("/tmp/rsvg.svg") }
        svg(filename="/tmp/rsvg.svg")
        plot = ggsurv(fit,cens.col='gray',plot.cens=F,back.white=T)
 #+
        #guides(linetype = F) +
         #       scale_colour_discrete(name = 'Location', breaks = c("anterior","posterior"), 
          #                            labels=c('Posterior', 'Anterior'))
        print(plot)
        dev.off()
        browseURL("file:///tmp/rsvg.svg")
    }
