## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##
## WIP survival analysis

library(survival)
library("simPH")

rsurv <- function()
    { structure(list(),class="rsurv") }

print.rsurv <- function(object) { cat("<restoration survival analysis>\n") }

resin_restoration_survival_analysis <- function ()
    { s <- rsurv()
      s$all <- collect_all_restorations_and_latest_followup()
      s$fail <- collect_restoration_failures()
      create_rsurv(s)
  }

summary.rsurv <- function (it)
    { summary(it$correlates);
  }

## take the set of all resin restorations and subtract out those which we know failed
subset_all_minus_fail <- function(all, fail)
{
  rownames(all)=all[,"proci1"]
  rownames(fail)=fail[,"proci1"]
  keep <- setdiff(rownames(all),rownames(fail));;
  res <- as.matrix(all[keep,]);
}


create_rsurv <- function (s)
    {   lastsa <<- s;
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
        s$years <- c(s$fail_difference_in_months,s$no_record_of_failure_in_months)/12;
        s$correlates <- rbind(data.frame(location=as.factor(s$fail[,"tooth_type"]),
                                         gender=as.factor(s$fail[,"gender"]),
                                         age=s$age_at_fail),
                              data.frame(location=as.factor(s$have_last_visit[,"tooth_type"]),
                                         gender=as.factor(s$have_last_visit[,"gender"]),
                                         age=s$age_at_last_visit))
        lastsa <<- s;
        s$correlates[,"age_group"] <- as.factor(cut(s$correlates[,"age"],breaks=c(0,18,30,60,100)));
        s$nlastvisit <- dim(s$have_last_visit)[1]
        surv <- Surv(s$years,s$status) ;
        s$surv <- surv;
        s$location_fit <- survfit(surv~location,conf.type="none",data=s$correlates);
        s$gender_fit <- survfit(surv~gender,conf.type="none",data=s$correlates);
        s$age_fit <- survfit(surv~age_group,conf.type="none",data=s$correlates);
        s$fit <- survfit(surv~1,conf.type="none",data=s$correlates);
        s$cox<-coxph(surv~location+gender+age, method="breslow",data=sa$correlates)
        s
    }

#plot.rsurv(sa,title="Resin restoration longevity",fit=sa$fit,col="dark blue")

#plot.rsurv(sa,title="Resin restoration longevity by age at failure",fit=sa$age_fit,col=c("dark gray","dark green","dark blue","dark red"))

#plot.rsurv(sa,title="Resin restoration longevity by gender",fit=sa$gender_fit,col=c("dark blue","dark red"))

#plot.rsurv(sa,title="Resin restoration longevity by location",fit=sa$gender_fit,col=c("dark blue","dark red"))


plot.rsurv <- function (s,fit=s$location_fit,title="All",xlab="Time in years",ylab="Fraction surviving",ci=F,col=c("dark green","dark blue"))
    {
        bplotf(function()
            print(ggsurv(fit,cens.col='gray',plot.cens=F,back.white=T,
                         main=title,xlab=xlab,ylab=ylab,CI=ci,surv.col=col)
                  + scale_x_continuous(breaks=1:13,expand=c(0,0),limits=c(0,12.5))
                  + scale_y_continuous(expand=c(0,.01))
                  + theme(axis.title.y=element_text(vjust=1.5,face="bold"))
                  + theme(axis.title.x=element_text(vjust=-.7,face="bold"))
                  + theme(title=element_text(vjust=2,face="bold"))
                  + theme(legend.position=c(.9,.5),
                              legend.text=element_text(size=12,face="bold"),
                          legend.title=element_text(size=14))
                  + guides(colour = guide_legend(override.aes = list(size=3)))
                  ))
    }

#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#        par(mfrow=c(2,2));
#        plot1 = ggsurv(s$fit,cens.col='gray',plot.cens=F,back.white=T,main="All")
        ## plot2 = ggsurv(s$fit_location,cens.col='gray',plot.cens=F,back.white=F,main="Posterial vs anterior")        ## plot3 = ggsurv(s$fit_gender,cens.col='gray',plot.cens=F,back.white=T,main="Male vs Female")
        ## plot4 = ggsurv(s$fit_age,cens.col='gray',plot.cens=F,back.white=T,main="Age group")
        ## grid.arrange(plot1, plot2, plot3,plot4, nrow=2, ncol=2)
        #print(plot1);print(plot2);print(plot3);print(plot4)
        #multiplot(plot1,plot2,cols=2)
    }


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

