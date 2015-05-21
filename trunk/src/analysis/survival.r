## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##
## Survival Analysis: Do it all: resin_restoration_survival_analysis()

library(survival)
library("simPH")

# Create an rsurv "object", mostly so that we can control printing
# Sigh, have to be careful if we want to call by reference - i.e. slot changes persist
# http://www.stat.berkeley.edu/~paciorek/computingTips/Pointers_passing_reference_.html

rsurv <- function()
    { object=new.env(parent=globalenv())  
      class(object)="rsurv"
      lastrsurv <<-  object;
      return(object)
  }

# We have a lot of state - don't print it all
print.rsurv <- function(object) { cat("<restoration survival analysis>\n") }

# Main entry point. Query the database twice - once to fetch all
# restorations, once to fetch all the restorations with a known
# failure.
# Returns an rsurv object. Important methods: plot, summary.

resin_restoration_survival_analysis <- function ()
    { s <- rsurv()
      s$all <- collect_all_restorations_and_latest_encounter_after()
      s$fail <- collect_restoration_failures()
      create_rsurv(s)
  }

# Summary, for now, is the correlates table
summary.rsurv <- function (it)
    { summary(it$correlates);
  }

# Take the set of all resin restorations and subtract out those which
# we know failed. The rest will be censored
# Could do this in Sparql, but hadn't learned enough
# SPARQL 1.1 when this was first written.
subset_all_minus_fail <- function(all, fail)
{
  rownames(all)=all[,"proci1"]
  rownames(fail)=fail[,"proci1"]
  keep <- setdiff(rownames(all),rownames(fail));;
  res <- as.matrix(all[keep,]);
}

# Helper functions.

# Takes two date strings(yyyy-mm-yy) and uses seq.date to return the
# time difference in months. Rounds down.

difference_in_months <- function(start,end)
    { length(seq(as.Date(start),as.Date(end),by="month"))-1 }

# Takes two date strings(yyyy-mm-yy) and uses seq.date to return the
# time difference in years. Rounds down.

difference_in_years <- function(start,end)
    { length(seq(as.Date(start),as.Date(end),by="year"))-1 }

# create arrays of correlates aligned with survival data - first the
# the ones we know fail, then the ones that will be censored.  Takes
# as input the rsurv object, where we have already prepared the
# matrices "fail" and "have_last_visit", each having one row per
# "event".
# Sets the "correlates" attribute to
# the created data frame.

prepare_survival_correlates <- function(s)
    { s$correlates <- rbind(data.frame(location=as.factor(s$fail[,"tooth_type"]),
                                       gender=as.factor(s$fail[,"gender"]),
                                       age=s$age_at_fail,
                                       patient=s$fail[,"patienti"],
                                       fail=TRUE),
                            data.frame(location=as.factor(s$have_last_visit[,"tooth_type"]),
                                       gender=as.factor(s$have_last_visit[,"gender"]),
                                       age=s$age_at_last_visit,
                                       patient=s$have_last_visit[,"patienti"],
                                       fail=FALSE))
      # divide the ages into bins, [0,18) , [18-30), [30,60), over 60
      s$correlates[,"age_group"] <- as.factor(cut(s$correlates[,"age"],breaks=c(0,18,30,60,100)));
  }


# having the failures (s$fail) and all events (s$all), create
# s$have_last_visit as the events where we have a restoration and a
# subsequent visit on record. These will be treated as right censored

prepare_censored_events <- function(s)
    {
        # Compute all restorations without a subsequent failure on record
        no_record_of_failure <- NULL;
        no_record_of_failure <- subset_all_minus_fail(s$all,s$fail);

        # Filter out ones without a subsequent visit - we can't use those at all. Call those $have_last_visit.
        s$have_last_visit <- no_record_of_failure[!is.na(no_record_of_failure[,"latest_date2"]),];

        # Calculate the time in months between the restoration and the last visit date on record
        s$no_record_of_failure_in_months <-
            mapply(difference_in_months,s$have_last_visit[,"date1"],s$have_last_visit[,"latest_date2"])

        # Calculate the age at last visit
        s$age_at_last_visit <- mapply(difference_in_years,s$have_last_visit[,"birthdate"],s$have_last_visit[,"date1"])

        s$ncensor = length(s$no_record_of_failure_in_months);
    }

# Calculate time until failure and the age at failure for those events
# where there was a failure. We consider the failure event, for the
# purpose of survival analysis, to be the date the failure is noticed
# or replaced. For future improvement - treat the failure as interval
# censored.

prepare_failure_events <- function(s)
{
    s$fail_difference_in_months <-
        mapply(difference_in_months,s$fail[,"date1"],s$fail[,"soonest_date2"])
    s$age_at_fail <- mapply(difference_in_years,s$fail[,"birthdate"],s$fail[,"date1"])
    s$nfail <- length(s$fail_difference_in_months);
}

# Main analysis
create_rsurv <- function (s)
    {
        prepare_censored_events(s)

        prepare_failure_events(s)

        # Now compute the main inputs to the survival analysis. 
        # s$status is 1 if the event was a failure, 0 if right censored.
        # combine the fail time and last visit times to make a vector
        # of time in months, and divide by 12 to get years.
        s$status <- c(rep(1,s$nfail),rep(0,s$ncensor));
        s$months <- c(s$fail_difference_in_months,s$no_record_of_failure_in_months)
        s$years <- c(s$fail_difference_in_months,s$no_record_of_failure_in_months)/12;
        prepare_survival_correlates(s)

        # Do the survival analysis, two argument version.
        surv <- Surv(s$years,s$status) ;
        s$surv <- surv;
        
        # Now do the univariate fits 
        s$fit <- survfit(surv~1,conf.type="none",data=s$correlates);
        s$location_fit <- survfit(surv~location,conf.type="none",data=s$correlates);
        s$gender_fit <- survfit(surv~gender,conf.type="none",data=s$correlates);
        s$age_fit <- survfit(surv~age_group,conf.type="none",data=s$correlates);
        # Make the age groups prettier
        names(s$age_fit$strata)=
            c("age=18 and under", "age=18+ to 30", "age=30+ to 60", "age=60 and over")
        
        # Compute the proportional hazards
        s$cox<-coxph(surv~location+gender+age, method="breslow",data=s$correlates)

        s
    }

# Draw one of the Kaplan-Meier plots
# We use ggplot, and hunt the web for recipes to show the bits we want
# to show.

plot_rsurv_fit <- function (s,fit=s$location_fit,
                            title="All",
                            xlab="Time in years",
                            ylab="Fraction surviving",
                            ci=F,col=c("dark green","dark blue")
                           )
    { print(
        ggsurv(fit,cens.col='gray',plot.cens=F,back.white=T,
               main=title,xlab=xlab,ylab=ylab,CI=ci,surv.col=col)
        # set up the x scale
       + scale_x_continuous(breaks=1:13,expand=c(0,0),limits=c(0,12.5))
        # set up the y scale
       + scale_y_continuous(expand=c(0,.01))
        # axes font bold (tweak position to look better too)
       + theme(axis.title.y=element_text(vjust=1.5,face="bold"))
       + theme(axis.title.x=element_text(vjust=-.7,face="bold"))
        # So is title
       + theme(title=element_text(vjust=2,face="bold"))
        # Place the legend inside the plot 
       + theme(legend.position=c(.9,.5),
               legend.text=element_text(size=12,face="bold"),
               legend.title=element_text(size=14))
        #add guides
        + guides(colour = guide_legend(override.aes = list(size=3))));

  }

# Draw all of the plots. plotWrap prepares the device. bplotf devault
# uses svg device and sends to browser.

plot.rsurv <- function (sa,plotWrap=bplotf)
{ plotWrap(function ()
    {plot_rsurv_fit(sa,title="Resin restoration longevity",
                fit=sa$fit,
                col="dark blue")},
         index="",filebase="overall-longevity")
  plotWrap(function ()
      {plot_rsurv_fit(sa,title="Resin restoration longevity by age at failure",
                  fit=sa$age_fit,
                  col=c("dark gray","dark green","dark blue","dark red"))},
         index="",filebase="longevity-by-age")
  plotWrap(function () 
      {plot_rsurv_fit(sa,title="Resin restoration longevity by gender",
                  fit=sa$gender_fit,
                  col=c("dark blue","dark red"))},
         index="",filebase="longevity-by-gender")
  plotWrap(function ()
      {plot_rsurv_fit(sa,title="Resin restoration longevity by location",
                  fit=sa$location_fit,
                  col=c("dark blue","dark red"))},
         index="",filebase="longevity-by-location")
}


# if factors, number of events for each factor
summary_text <- function(surv,correlate)
    { levels <- levels(as.factor(lastrsurv$correlates[,correlate]))
      c <- surv$correlates;
      factor_summary <- function(value,factor=correlate)
          { patient_count <- length(unique(c[(c[,factor]==value),"patient"]));
            event_count <- nrow(c[(c[,factor]==value),])
            failures_count <- nrow(c[(c[,factor]==value &  c[,"fail"]),]);
            paste0(value,": ",patient_count, " patients", ", ",event_count," events of which ",failures_count," were failures")
        }
      do.call(paste,args=c(lapply(as.list(levels),factor_summary),sep="\n"))
  }




# levels(as.factor(lastrsurv$correlates[,"age_group"]))

## sa$cox$coef["locationposterior"]
## sa$cox$coef["gendermale"]

## bplotf(function(){
## + baseline<-basehaz(coxph(x$gender_fit))
## + plot(baseline$time, baseline$hazard, type='l',main="Hazard rates") 
## + lines(baseline$time, exp(0.8245)*baseline$hazard, col="blue") })

## sa$correlates[,"AgeMed"]<-sa$correlates[,"age"]-40
## Sim1 <- coxsimLinear(M1, b = "AgeMed", nsim = 100,Xj = seq(-30, 30, by=1))
## bplotf(function()  {print(simGG(Sim1))})

## baseline<-basehaz(reg_fit)

## #http://www.unc.edu/courses/2010spring/ecol/562/001/docs/lectures/lecture24.htm#coxzph
## #http://www.uni-kiel.de/psychologie/rexrepos/posts/survivalCoxPH.html
## #http://www.statisticsmentor.com/category/r-survival-analysis/

