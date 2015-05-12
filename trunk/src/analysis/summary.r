## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-12
##
## Summary reports for Titus, Claudio

## How many health care encounters are there. That should include both
## visits and their parts. It doesn't yet.

how_many_encounters <- function ()
{ queryc("select (count(?visit) as ?count)",
         "where",
         "{ ?visit a health_care_encounter:",
         "}");
}

## how many visits. A visit has parts that are procedures, exams, etc.

how_many_visits <- function()
  {
    queryc("select (count(distinct ?visit) as ?count)",
           " where",
           " { ?visit a outpatient_encounter:.",
           " }")
  }

## The complement - how many visits where we do have specific
## information

how_many_visits_with_procedure <- function()
  {
    queryc("select (count(distinct ?visit) as ?count)",
           " where",
           "  { ?visit a outpatient_encounter:. ",
           "    ?proc is_part_of: ?visit. ",
           "    ?proc a dental_procedure:",
           "  }")
  }

## How many visits don't have parts (procedures)
## These correspond to visits where we haven't yet extracted the particulars.

how_many_visits_without_procedure <- function ()
  { queryc("select (count(distinct ?proc) as ?count)",
           " where",
           "  { ?proc a outpatient_encounter:.",
           "    optional{?part is_part_of: ?proc.}",
           "    FILTER ((?proc=?part) || (!bound(?part)) )",
           "  }")
  }

## Every visit and procedure should have a date
## Sanity check
how_many_things_with_occurence_date <- function ()
  { queryc("select (count(?s) as ?count) where {?s occurrence_date: ?o }") }

## How many procedures.

how_many_procedures <- function ()
  { queryc("select (count(distinct ?proc) as ?count) where { ?proc a dental_procedure: }") }

## I think this is patients that participate in a visit without
## without specific information. It was a first past (but wrong) for
## the next query

how_many_patients_with_visit_without_procedure <- function()
  { queryc("select (count(distinct ?patient) as ?count) where",
           " { optional { ?patient participates_in: ?visit. ",
           "              ?visit a outpatient_encounter:.} ",
           "   ?proc is_part_of: ?visit. ",
           "   ?patient participates_in: ?proc. ",
           "   ?patient a dental_patient:. ",
           "   FILTER ((!bound(?visit)) )}"
           ) 
  }

## how many patients do we not have any specifics about yet.
how_many_patients_with_no_information <- function ()
 {
   queryc("select (count(distinct ?patient) as ?count) where",
          " {",
          " ?patient a dental_patient:. ",
          "  optional {",
          "    ?patient participates_in: ?proc. ",
          "    ?proc a dental_procedure:.",
          "            }",
          " FILTER (!bound(?proc)) ",
          " }")
 }

## How many patients alltogether
how_many_patients <- function()
  { queryc("select (count(distinct ?patient) as ?count)",
           "where ",
           "{ ?patient a dental_patient: }")  }

## How many patients do we have specifics about. This plus next should
## add to total patients

how_many_patients_with_procedure <- function()
  { queryc("select (count(distinct ?patient) as ?count)",
           "where ",
           "{ ?patient participates_in: ?proc.",
           "  ?patient a dental_patient: .",
           "  ?proc a dental_procedure:.",
           "}")
  }

## Sanity check - this should be 0
how_many_procedures_not_part_of_visit <- function ()
  {   queryc("select (count(distinct ?proc) as ?count)",
             "  where",
             "  { ?proc a dental_procedure:.",
             "    optional{?proc is_part_of: ?visit} ",
             "    FILTER (!bound(?visit))",
             "  }")
    }
             

## Put some of the queries together to say something.

visit_summary <- function ()
  { 
    n_encounters <- how_many_encounters();
    n_procedures <- how_many_procedures();
    n_with_dates <- how_many_things_with_occurence_date();
    
    n_visits <-  how_many_visits();
    n_info_visits <- how_many_visits_with_procedure();
    n_noinfo_visits <- how_many_visits_without_procedure();

    n_patients <- how_many_patients();
    n_info_patients <- how_many_patients_with_procedure ();
    n_noinfo_patients <- how_many_patients_with_no_information();
    cat("Summary of the dataset at ",current_endpoint,"\n",

        "There are ",n_encounters," health care encounter instances, of which ",
         n_visits," are visits and ",
        n_procedures," are procedures.\n",

        "As a sanity check, there are ",
        n_with_dates, " encounters with dates associated - all should have dates.\n",

        "Of the visits, we have procedures for ",
        n_info_visits," of the visits and no information(yet) for the other ",n_noinfo_visits,".\n",

        "There are ",n_patients," patients of which ",
        n_info_patients," have at least one recorded procedure, and ",
        n_noinfo_patients," don't.\n",

        sep="");
  }


memo_first_and_last_visits <- NULL
get_first_and_last_visits <- function()
    {  if (!is.null(memo_first_and_last_visits)) {return(memo_first_and_last_visits)}
       res <- queryc("select ?patient (min(?date) as ?earliest) (max(?date) as ?latest)",
                     "       (count(?date) as ?nvisits) where",
                     "{",
                     " ?visit a outpatient_encounter:.",
                     " ?visit occurrence_date: ?date.",
                     " ?patient participates_in: ?visit.",
                     " ?patient a dental_patient:.",
                     "} GROUP BY ?patient"
                     )
       res <- data.frame(res,stringsAsFactors = FALSE);
       res$latest <- as.Date(res$latest)
       res$earliest <- as.Date(res$earliest)
       lastDataTime <- max(res$latest)
       res$nvisits <- as.numeric(res$nvisits)
       res$timeBetweenVisitsAsDays <- as.numeric(res$latest - res$earliest)
       res$firstToEndPracticeDays <- as.numeric(lastDataTime - res$earliest)
       which <-((res$nvisits <= atmostvisits) & (res$nvisits >=atleastvisits));
       res$month <- as.Date(cut(res$earliest,"month") )
       res
   }


plot_number_of_first_visits_by_month <- function(atleastvisits=2,atmostvisits=200,breaks=20)
    {   res <- get_first_and_last_visits()
        print(ggplot(res, aes(month,1))
              + stat_summary(fun.y = sum, geom = "bar") 
              + scale_x_date(minor_breaks="month")
              + labs(title=paste("Number of first visits per month (",length(res$nvisits)," patients)",sep=""),
                               x="Time", 
                               y="Number of patient first visits"))
    }



plot_patient_time_in_practice <- function()
{  res <- get_first_and_last_visits()
   plot(hist(res$firstToEndPracticeDays[which]/365,plot=FALSE,breaks=breaks),
        xlab="Years between first visit and last date of practice data",
        ylab="Number of patients",
        main=paste("Time between patient first visit and end \nof practice data (",
            length(res$firstToEndPracticeDays[which]),
            " patients, at least ",
            atleastvisits," visits)",sep=""))
}

plot_years_between_and_last_visit <- function()
{  res <- get_first_and_last_visits();
   plot(hist(res$timeBetweenVisitsAsDays[which]/365,plot=FALSE,breaks=breaks),
         xlab="Years between first and last visit",
         ylab="Number of patients",
         main=paste("Time between patient first and last visit \n(",length(res$timeBetweenVisitsAsDays[which])," patients, at least ",atleastvisits," visits)",sep="")
        )
}

plot_distribution_of_patient_number_of_visits <- function ()
{  res <- get_first_and_last_visits();
   plot(hist(as.numeric(res$nvisits),plot=FALSE,breaks=breaks),
        xlab="Total number of visits",
        ylab="Number of patients",
        main=paste("Number of visits per patient (",length(res$nvisits)," patients)",sep="")
        )
  }

  
## return an argument list for plot, so it can be easily reused by passing different procedure types.
## type is a prefix. label is what you want to call it - defaults to rdfs:label
distribution_of_procedures_over_time <- function(type,label,breaks=24)
  { res <- queryc("select distinct ?proc ?date",
                  " where ",
                  " {",
                  "  ?proc a ",type,".",
                  "  ?proc occurrence_date: ?date.",
                  " }"
                  );
    if (is.na(label)) label<- queryc("select ?label where {",type," rdfs:label ?label}");
    res <- data.frame(res,stringsAsFactors=FALSE);
    res$date <- as.Date(res$date);
    res$month <- as.Date(cut(res$date,"month") )
    plot <- ggplot(res, aes(month, 1))+ stat_summary(fun.y = sum, geom = "bar") + scale_x_date(minor_breaks="month") +
      labs(title=paste("#  ",label," per month",sep=""),
           x="Date", 
           y=paste("Number of ",label),sep="")
     ;
    plot
  }

plot_4x4 <- function ()
    { bplotf(function()
        { par(mfrow=c(4,4));
          print(plot_number_of_first_visits_by_month())
          pront(plot_patient_time_in_practice())
          print(plot_years_between_and_last_visit())
          print(plot_distribution_of_patient_number_of_visits())
      })
  }
      
## Plot number of restorations vs time (all, crowns, fillings)
distribution_of_restorations_over_time_report <- function(breaks=144)
  { svg(filename="/tmp/rsvg.svg",width=10,height=7.5);
    par(mfrow=c(2,1));
    plot1 <- distribution_of_procedures_over_time("tooth_restoration_procedure:","restoration procedures",breaks=breaks);
    plot2 <- distribution_of_procedures_over_time("intracoronal_restoration:","filling procedures",breaks=breaks);
    plot3 <- distribution_of_procedures_over_time("crown_restoration:","crowns",breaks=breaks)
    grid.arrange(plot1, plot2, plot3, nrow=2, ncol=2)
    dev.off();
    browseURL("file:///tmp/rsvg.svg")
}

count_encounters_by_type <- function()
  { queryc("select ?vl (count(distinct ?visit) ?as count)",
           " where ",
           " { ?visit a health_care_encounter:.",
           "   ?visit a ?vt. ",
           "   ?vt rdfs:label ?vl",
           " } group by ?vl") }

## sorted table of code and count. Includes higher-level CDT categories.
billing_code_counts <- function() 
  {
    queryc("select (count(distinct ?proc) as ?count) ?code",
           " where",
           "  { ?code_instance is_about: ?proc. ",
           "    ?code_instance a ?code_type.",
           "    ?code_instance a cdt_code:.", # restrict to CDT codes
           "    ?code_instance a ?code_type.", # get the type - there is an instance for each billed code
           "    ?code_type rdfs:subClassOf cdt_code:.", # don't show classes above CDT code (such as information content entity)
           "    ?code_type rdfs:label ?code.",
           "    ?proc a dental_procedure:.",
           "  }",
           "group by ?code",
           "order by desc(?count)")
  }


health_encounters_for_patient <- function (number)
    { queryc("select ?encounteri ?encounteriLabel ?encounteriLabel ?surfacet ?surfacetLabel ?date where {",
                         "?patient participates_in: ?encounteri.",
                         "optional {?surfacei asserted_type: ?surfacet. ",
                         "?surfacet rdfs:label ?surfacetLabel.",
                         "?encounteri has_participant: ?surfacei.",
                         "?surfacei a tooth_surface:}",
                  paste0("?patient rdfs:label \"patient ",number,"\"."),
                  "?encounteri rdfs:label ?encounteriLabel.",
                  "?encounteri occurrence_date: ?date}",
                  "order by ?date")
}

