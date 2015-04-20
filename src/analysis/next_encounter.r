##Author: Alan Ruttenberg
## Date: 2015-04-17

## The query to find the next visit is a bit long and not something
## you want to repeat all the time. We materialize it here, asserting,
## for each patient, subsequent visit links so queries can use those
## links for simpler query.

encounterPattern <- function ()
{ "{ ?patient a patient:.
  ?first_encounter a health_care_encounter:.
  ?first_encounter has_participant: ?patient.
  ?first_encounter realizes: ?role_first.
  ?role_first a patient_role:.
  ?role_first inheres_in: ?patient.
  ?first_encounter occurrence_date: ?first_date.
  #
  ?next_encounter a health_care_encounter:.
  ?next_encounter has_participant: ?patient.
  ?next_encounter realizes: ?role_in_next.
  ?role_in_next a patient_role:.
  ?role_in_next inheres_in: ?patient.
  ?next_encounter occurrence_date: ?next_date.
  filter (?next_date > ?first_date && ?next_encounter != ?first_encounter)
  #
  optional
  { ?nonexistent_middle_encounter a health_care_encounter:.
    ?nonexistent_middle_encounter has_participant: ?patient.
    ?nonexistent_middle_encounter realizes: ?role_in_middle.
    ?role_in_middle a patient_role:.
    ?role_in_middle inheres_in: ?patient.
    ?nonexistent_middle_encounter occurrence_date: ?middle_date.
    filter ((?next_date > ?middle_date) && (?middle_date > ?first_date))
  }
  filter(!bound(?nonexistent_middle_encounter))
 }"
}


check_next_encounter_query <- function ()
queryc("select ?first_encounter ?next_encounter where", encounterPattern(), "limit 20",cache=F);

assert_occurrence_date_in_parts <- function
  { sparqlUpdate("insert {?p2 occurrence_date: ?date} where {?p occurrence_date ?date. ?p2 is_part_of ?p.}")}


assert_next_encounter_links <- function()
  { sparqlUpdate("insert data { next_encounter: a owl:ObjectProperty. next_encounter: rdfs:label \"subsequent encounter\".}")
    sparqlUpdate("insert { ?first_encounter next_encounter: ?next_encounter } where ",encounterPattern())
  }

assert_transitive_next <- function()
  {sparqlUpdate("insert data ",
                "{",
                "  later_encounter: a owl:ObjectProperty.",
                "  later_encounter: a owl:TransitiveProperty.",
                "  later_encounter: rdfs:label \"subsequent encounters\".",
                "  next_encounter: rdfs:subPropertyOf later_encounter:",
                "}")
 }


## test it by replicating one of the summary queries
test_next_link_query <- function()
  { result <-
      queryc("select ?patient (count (?encounter) as ?count)",
           "where",
           "{",
           "?encounter a outpatient_encounter:.",
           "?encounter has_participant: ?patient.",
           "?encounter realizes: ?role.",
           "?role a patient_role:.",
           "?role inheres_in: ?patient.",
           "?encounter next_encounter: ?next.",
           "}",
             "group by ?patient",cache=F)
    svgfn <- "/tmp/rsvg2.svg";
    if(file.exists(svgfn)) { file.remove(svgfn) }
    svg(filename=svgfn)
    plot(hist(as.numeric(as.numeric(result[,2])),plot=FALSE,breaks=20),
         xlab="Total number of visits",
         ylab="Number of patients",
         main=paste("Number of encounters per patient (",length(res$visits)," patients)",sep=""),
         );
    dev.off()
    browseURL(svgfn)
  }


# limiting to visit there are 86000 triples added. There will be many more for the healthcare encounters generally. Not too bad, now 351167 triples, but an hour run time.
