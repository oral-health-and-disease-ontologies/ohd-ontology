## WIP

#http://www.rde.ac/Synapse/Data/PDFData/2185RDE/rde-38-11.pdf
#http://reliawiki.org/index.php/Life_Data_Classification
#http://www.talkstats.com/showthread.php/44578-Help-with-Kaplan-Meier-Analysis
#https://www.ctspedia.org/do/view/CTSpedia/CensorData
#Statistical and Methodological Aspects of Oral Health Research
#http://cran.r-project.org/web/packages/survival/survival.pdf
# http://www.ats.ucla.edu/stat/r/examples/asa/asa_ch2_r.htm
# http://anson.ucdavis.edu/~hiwang/teaching/10fall/R_tutorial%201.pdf

## Left censoring – a data point is below a certain value but it is
## unknown by how much.

## Interval censoring – a data point is somewhere on an interval
## between two values.

## Right censoring – a data point is above a certain value but it is
## unknown by how much.

## Type I censoring occurs if an experiment has a set number of
## subjects or items and stops the experiment at a predetermined time,
## at which point any subjects remaining are right-censored.

## Type II censoring occurs if an experiment has a set number of
## subjects or items and stops the experiment when a predetermined
## number are observed to have failed; the remaining subjects are then
## right-censored.

## Random (or non-informative) censoring is when each subject has a
## censoring time that is statistically independent of their failure
## time. The observed value is the minimum of the censoring and
## failure times; subjects whose failure time is greater than their
## censoring time are right-censored.


    ##  "{select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (min(?date2) as ?soonest_date2

    ## res <- data.frame(res,stringsAsFactors = FALSE);

end_of_practice <- function()
  {
    queryc("select  (max(?date) as ?latest)",
           "where {",    
           " ?encounter a outpatient_encounter:.",
           " ?encounter occurrence_date: ?date.",
           "}")
  }

health_encounters_for_patient <- function (number)
  { queryc("select ?encounter ?date where {",
           "<http://purl.obolibrary.org/obo/ohd/individuals/I_9456fb701a0262a9b262f903acf69f34> participates_in: ?encounteri.",
           "?encounteri rdfs:label ?encounter.",
           "?encounteri occurrence_date: ?date}",
           "order by ?date}")
  }

subset_all_minus_fail <- function(all, fail)
{
  rownames(all)=paste0(all[,"proci1"],all[,"surface"]);
  rownames(fail)=paste0(fail[,"proci1"],fail[,"surface"]);
  keep <- setdiff(rownames(all),rownames(fail));;
  res <- as.matrix(all[keep,]);
}



create_surv <- function (fail,all)
  { npoint <- nrow(fail);
    difference_in_months <- function(start,end) { length(seq(as.Date(start),as.Date(end),by="month"))-1 }

    # should interval censor
    fail_difference_in_months <- mapply(difference_in_months,fail[,"date1"],m[,"soonest_date2"])

    censored <-  subset_all_minus_fail(all,fail);
    right_censored_from_restoration <- censored[is.na(censored[,"latest_date2"]),]

    right_censored_from_restoration[is.na(censored[,"latest_date2"]),"latest_date2"]
    right_censored_from_restoration[,"latest_date2"]=right_censored_from_restoration[,"date1"]

    right_censorted_from_follup <- censored[!is.na(censored[,"latest_date2"]),]
    
    latest_date <- end_of_practice();
                                            

    not_restored[is.na(not_restored[,"latest_date2"]),] <- latest_date.
    surv <- Surv(mapply(difference_in_months,fail[,"date1"],m[,"soonest_date2"]),rep(1,npoint))
    surv
  }



## Surv(as.Date(m[,"date1"],m[,"soonest_date2"],c(TRUE),c("counting"))

##       Surv(time, time2, event,
##          type=c('right', 'left', 'interval', 'counting', 'interval2', 'mstate'),
##          origin=0)
##      is.Surv(x)


## restoration happens
## either
##   restoration fails
##   there is another later visit and we don't have contradictory information
##   there is no later visit >0 

##      Surv(time, time2, event,
##          type=c('right', 'left', 'interval', 'counting', 'interval2', 'mstate'),
##          origin=0)

##     time: for right censored data, this is the follow up time.  For
##           interval data, the first argument is the starting time for
##           the interval.

##    time2: ending time of the interval for interval censored or counting
##           process data only.  Intervals are assumed to be open on the
##           left and closed on the right, '(start, end]'.  For counting
##           process data, 'event' indicates whether an event occurred at
##           the end of the interval.

## We don't have interval censoring.
## The next event is either a) a failure or b) the last visit to the practice
## If (a) then interval censored: last visit to now
## If (b) right censored at visit

## failure, last visits

## need a query that returns
## date of restoration
## date of failure (or null)
## date of last visit or last visit before failure

## Survival Analysis of Complete Veneer Crowns vs. Multisurface Restorations: A Dental School Patient Population
## http://www.jdentaled.org/content/70/10/1098.full.pdf

## Abstract: The purpose of this study was to compare the longevity of crowns versus large multisurface restorations in posterior teeth. The investigation used the treatment database at Virginia Commonwealth University School of Dentistry. The inclusion criteria for the final data set used for analysis were: only one restored tooth per patient, premolars with three or more restored surfaces, molars with four or more restored surfaces, molars and premolars restored with complete veneer metal crowns, or crowns veneered with metal and porcelain. The Kaplan-Meier approach was used to visualize the survival curves, and the Cox proportional hazards model was used for analysis of predictor variables. The investigation indicates crowns survive longer than large restorations and premolar restorations survive longer than molar restorations. The median survival for crowns exceeded 16.6 years, with the median survival of premolar restorations being 4.4 years and molar restorations 1.3 years. An interaction between age and treatment was discovered, with overall survival decreasing as patient age increases. The doctor supervising the treatment also affected survival with treatment supervised by specialists lasting longer than treatment supervised by nonspecialists.

## http://www.nature.com.gate.lib.buffalo.edu/bdj/journal/v211/n4/pdf/sj.bdj.2011.683.pdf
## Survival analysis of composite Dahl restorations provided to manage localised anterior tooth wear (ten year follow-up)
## Objective To evaluate ten-year survival and clinical performance of resin-based composite restorations placed at increased vertical dimension as a ‘Dahl’ type appliance to manage localised anterior tooth wear. Design A prospective survival analysis of restorations provided at a single centre. Setting UK NHS hospital and postgraduate institute. Methods The clinical performance of 283 composite resin restorations on 26 patients with localised anterior tooth wear was reviewed after a ten year follow-up period. The study used modified United States Public Health Service (USPHS) criteria for assessing the restorations. Survival of the restorations was analysed using Kaplan-Meier survival curves, the log-rank test, and the Cox proportional hazards regression analysis. Results The results indicated that the median survival time for composite resin restorations was 5.8 years and 4.75 years for replacement restorations when all types of failure were considered. The restorations commonly failed as a result of wear, fracture and marginal discoloration. The factors that significantly influenced the survival of these restorations were the incisal relationship, aetiology, material used, and the nature of opposing dentition. The biological complications associated with this treatment regime were rare. Patient satisfaction remained high despite the long term deterioration of the restorations. Conclusion With some degree of maintenance, repeated use of composite resin restorations to treat localised anterior tooth wear at an increased occlusal vertical dimension is a viable treatment option over a ten-year period.

## http://www.aapd.org/assets/1/25/Papathanasiou-16-04.pdf
## The influence of restorative material onthe survival rate of restorations in primarymolars
## Abstract
## The survival rates of restorations in primary molars were calculated after a retrospective examination of patients" dental records from a study population of 1,065 children. A random sample of 128 records showing information for 604 dental restorations was examined,coded,andanalyzed by the life table method of survival analysis. The order of the survival rate of
## restorations from higher to lower success was preformed crowns,amalgam,composite resin, and glass ionomer restorations. A highly statistically significant difference (P = 0.0001) was found among the survival success rates of different material restorations. For preformed crowns and amalgam restorations, the median survival time was more than 5 years. The 5-year survival estimate for preformed crowns was 68% and for amalgam restorations was 60%. For composite resin the median survival time was 32 months and the 4-year survival estimate was 40%.For glass ionomer restorations, the median survival time was 12 months and the 4-year survival estimate was 5%. (Pediatr Dent 16:282-88, 1994)

## http://en.wikipedia.org/wiki/Proportional_hazards_model
## http://en.wikipedia.org/wiki/Censoring_%28statistics%29
## http://en.wikipedia.org/wiki/Kaplan–Meier_estimator

## covariates:
## Time from start of practice
## Age of patient
## Average number of visits per unit time
## Tooth characteristics
## material
## restorations already in place

## http://www.nature.com.gate.lib.buffalo.edu/ebd/journal/v15/n4/pdf/6401065a.pdf
## Caries risk and number of restored surfaces have impact
## on the survival of posterior composite restorations
## Study selection Longitudinal studies of direct class II or classes I and II restorations in permanent dentition of at least five years duration, a minimum of 20 restorations at final recall and the original datasets available were considered. Only English language studies were included. Two reviewers screened titles independently.
## Data extraction and synthesis Multivariate Cox regression method to analyse the variables of interest and hazard ratios with respective 95% confidence intervals were determined. The annual failure rate (AFR) of the investigated restorations and subgroups was calculated.
## Results Twelve studies, nine prospective and three retrospective were included. A total of 2,816 restorations (2,585 Class II and 231 Class I restorations) were included in the analysis. Five hundred and sixty-nine restorations failed during the observation period, and the main reasons for failure were caries and fracture. Regression analyses showed a significantly higher risk of failure for restorations in high- caries-risk individuals and those with a higher number of restored surfaces. The overall annual failure rate at five years and ten years was 1.8% and 2.4% respectively. The rates were higher in high-caries-rate individuals at 3.2% and 4.6% respectively.

## http://www.rde.ac/Synapse/Data/PDFData/2185RDE/rde-38-11.pdf
## The effect of clinical performance on the survival estimates of direct restorations

## Objectives: In most retrospective studies, the clinical performance of restorations had not been considered in survival analysis. This study investigated the effect of including the clinically unacceptable cases according to modified United States Public Health Service (USPHS) criteria into the failed data on the survival analysis of direct restorations as to the longevity and prognostic variables. Materials and Methods: Nine hundred and sixty-seven direct restorations were evaluated. The data of 204 retreated restorations were collected from the records, and clinical performance of 763 restorations in function was evaluated according to modified USPHS criteria by two observers. The longevity and prognostic variables of the restorations were compared with a factor of involving clinically unacceptable cases into the failures using Kaplan- Meier survival analysis and Cox proportional hazard model. Results: The median survival times of amalgam, composite resin and glass ionomer were 11.8, 11.0 and 6.8 years, respectively. Glass ionomer showed significantly lower longevity than composite resin and amalgam. When clinically unacceptable restorations were included into the failure, the median survival times of them decreased to 8.9, 9.7 and 6.4 years, respectively. Conclusions: After considering the clinical performance, composite resin was the only material that showed a difference in the longevity (p < 0.05) and the significantly higher relative risk of student group than professor group disappeared in operator groups. Even in the design of retrospective study, clinical evaluation needs to be included. (Restor Dent Endod 2013;38(1):11-20)

## http://www.people.vcu.edu/~albest/DENS580/pdfs/BogackiSurvival.pdf
## Survival Analysis of Posterior Restorations Using an Insurance Claims Database
