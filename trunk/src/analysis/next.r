## The query to find the next visit is a bit long and not something
## you want to repeat all the time. We materialize it here, asserting,
## for each patient, subsequent visit links so queries can use those
## links for simpler query.

visitPattern <- function ()
{ "{ ?patient a patient:.
  ?first_visit a outpatient_encounter:.
  ?first_visit has_participant: ?patient.
  ?first_visit realizes: ?role_first.
  ?role_first a patient_role:.
  ?role_first inheres_in: ?patient.
  ?first_visit occurrence_date: ?first_date.
  #
  ?next_visit a outpatient_encounter:.
  ?next_visit has_participant: ?patient.
  ?next_visit realizes: ?role_in_next.
  ?role_in_next a patient_role:.
  ?role_in_next inheres_in: ?patient.
  ?next_visit occurrence_date: ?next_date.
  filter (?next_date > ?first_date && ?next_visit != ?first_visit)
  #
  optional
  { ?nonexistent_middle_visit a outpatient_encounter:.
    ?nonexistent_middle_visit has_participant: ?patient.
    ?nonexistent_middle_visit realizes: ?role_in_middle.
    ?role_in_middle a patient_role:.
    ?role_in_middle inheres_in: ?patient.
    ?nonexistent_middle_visit occurrence_date: ?middle_date.
    filter ((?next_date > ?middle_date) && (?middle_date > ?first_date))
  }
  filter(!bound(?nonexistent_middle_visit))
 }"
}

check_next_visit_query <- function ()
queryc("select ?first_visit ?next_visit where", visitPattern(), "limit 20");

assert_next_visit_links <- function()
  { sparqlUpdate("insert { next_visit: a owl:ObjectProperty. next_visit: rdfs:label \"subsequent visit\".}")
    sparqlUpdate("insert { ?first_visit next_visit: ?next_visit } where ",visitPattern())
  }

## test it by replicating one of the summary queries
test_next_link_query <- function()
  { result <-
      queryc("select ?patient (count (?visit) as ?count)",
           "where",
           "{",
           "?visit a outpatient_encounter:.",
           "?visit has_participant: ?patient.",
           "?visit realizes: ?role.",
           "?role a patient_role:.",
           "?role inheres_in: ?patient.",
           "?visit next_visit: ?next.",
           "}",
             "group by ?patient")
    a <<- result;
    svgfn <- "/tmp/rsvg2.svg";
    if(file.exists(svgfn)) { file.remove(svgfn) }
    svg(filename=svgfn)
    plot(hist(as.numeric(as.numeric(result[,2])),plot=FALSE,breaks=20),
         xlab="Total number of visits",
         ylab="Number of patients",
         main=paste("Number of visits per patient (",length(res$nvisits)," patients)",sep=""),
         );
    dev.off()
    browseURL(svgfn)
  }
