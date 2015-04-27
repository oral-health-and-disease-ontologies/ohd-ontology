## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##
## WIP survival analysis

library(survival)

## take the set of all resin restorations and subtract out those which we know failed
subset_all_minus_fail <- function(all, fail)
{
  rownames(all)=all[,"proci1"]
  rownames(fail)=fail[,"proci1"]
  keep <- setdiff(rownames(all),rownames(fail));;
  res <- as.matrix(all[keep,]);
}

## create a survival object
## discard lets us drop restorations that lasted less that its value's months

survival_posterior_vs_anterior <- function ()
    { all <- collect_all_restorations_and_latest_followup()
      fail <- collect_restoration_failures()
      create_surv(fail,all)
  }

create_surv <- function (fail,all,which="all",discard=0,keep_censor=1000)
    { 
        no_record_of_failure <- NULL;
        fail[is.na(fail[,"is_posterior"]),"is_posterior" ]<-"anterior" # is_posterior is either 1 or NA. Change NA to 0.
        all[is.na(all[,"is_posterior"]),"is_posterior" ]<-"anterior" # is_posterior is either 1 or NA. Change NA to 0.
        all[all[,"is_posterior"]==1,"is_posterior" ]<- "posterior"
        fail[fail[,"is_posterior"]==1,"is_posterior" ]<- "posterior"
        if (!(Reduce("||",which==c("fail","all","censored"))))
            { stop("which needs to be one of: all, censored, or failed") };
        difference_in_months <- function(start,end) { length(seq(as.Date(start),as.Date(end),by="month"))-1 }
                                        # these include if there was a subsequent visit or not.
        if (which=="all" || which=="censored")
            {  no_record_of_failure <- subset_all_minus_fail(all,fail);
               have_last_visit <- no_record_of_failure[!is.na(no_record_of_failure[,"latest_date2"]),];
               no_record_of_failure_in_months <- mapply(difference_in_months,have_last_visit[,"date1"],have_last_visit[,"latest_date2"])
               no_record_of_failure_in_months <- no_record_of_failure_in_months[no_record_of_failure_in_months<keep_censor];
               ncensor = length(no_record_of_failure_in_months);
#               bplot(hist(no_record_of_failure_in_months,breaks=140))
           }
        if (which=="all" || which=="failed")
            {# should interval censor
                fail_difference_in_months <- mapply(difference_in_months,fail[,"date1"],fail[,"soonest_date2"])
                fail_difference_in_months <- fail_difference_in_months[fail_difference_in_months > discard && fail_difference_in_months< keep_censor];
                nfail <- length(fail_difference_in_months);
            }
                                        #      else 
        if (which=="all")
            { status <- c(rep(1,nfail),rep(0,ncensor));
              months <- c(fail_difference_in_months,no_record_of_failure_in_months)
              hlv <<- data.frame(location=have_last_visit[,"is_posterior"])
              survdata <<- rbind(data.frame(location=fail[,"is_posterior"]),data.frame(location=have_last_visit[,"is_posterior"]))
              print(dim(have_last_visit));print(dim(fail));
          } else
              if (which=="failed")
                  { status <- rep(1,nfail)
                    months <- fail_difference_in_months }
              else if (which=="censored")
                  { status <- rep(0,ncensor)
                    months <- no_record_of_failure_in_months }
        surv <- Surv(months,status) ;
        if (file.exists("/tmp/rsvg.svg")) { file.remove("/tmp/rsvg.svg") }
        svg(filename="/tmp/rsvg.svg")
        print(ggsurv(survfit(surv~location,conf.type="none",data=survdata)))
        dev.off()
        browseURL("file:///tmp/rsvg.svg")
        surv}


