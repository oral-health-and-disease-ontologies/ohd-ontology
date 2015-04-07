
# How many health care encounters are there. That should include both visits and their parts. It doesn't yet. 
how_many_encounters <- function ()
{ queryc("select (count(?visit) as ?count) { ?visit a health_care_encounter: }");}

# what are the kinds of health care encounters
what_kinds_of_encounters <- function()
  { queryc("select distinct ?vl where { ?visit a health_care_encounter:. ?visit a ?vt. ?vt rdfs:label ?vl }") }

# how to health care enounters relate to other things
how_do_encounters_relate_to_other_things <- function()
 { queryc("select distinct ?pl where { ?proc a health_care_encounter:. ?proc ?p ?visit. ?p rdfs:label ?pl }") }

# how many patient encounters
how_many_visits <- function()
  {
    queryc("select (count(distinct ?visit) as ?count) where { ?visit a outpatient_encounter:.}")
  }

how_many_visits_with_procedure <- function()
  {
    queryc("select (count(distinct ?visit) as ?count) where { ?visit a outpatient_encounter:. ?proc is_part_of: ?visit. ?proc a dental_procedure: }")
  }

# How many visits don't have parts (procedures)
how_many_visits_without_procedure <- function ()
 { queryc("select (count(distinct ?proc) as ?count) where { ?proc a outpatient_encounter:. optional{?part is_part_of: ?proc.} FILTER ((?proc=?part) || (!bound(?part)) )}")}


# Every visit and procedure should have a date
how_many_things_with_occurence_date <- function ()
  { queryc("select (count(?s) as ?count) where {?s occurrence_date: ?o }") }

# How many procedures
how_many_procedures <- function ()
  { queryc("select (count(distinct ?proc) as ?count) where { ?proc a dental_procedure: }") }

## I think this is patients that participate in a visit without 
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

how_many_patients_with_no_information <- function ()
 {
   queryc("select (count(distinct ?patient) as ?count) where",
          " ?patient a dental_patient:. ",
          " { optional {",
          "    ?patient participates_in: ?proc. ",
          "    ?proc a dental_procedure:.",
          "            }",
          " FILTER (!bound(?proc)) ",
          " }")
 }

how_many_patients <- function()
  { queryc("select (count(distinct ?patient) as ?count)",
           "where ",
           "{ ?patient a dental_patient: }")  }

how_many_patients_with_procedure <- function()
  { queryc("select (count(distinct ?patient) as ?count) where { ?patient participates_in: ?proc. ?patient a dental_patient: . ?proc a dental_procedure:.}") }

how_many_procedures_not_part_of_visit <- function ()
  {   queryc("select (count(distinct ?proc) as ?count)  where { ?proc a dental_procedure:. optional{?proc is_part_of: ?visit} FILTER (!bound(?visit))}") }

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
        "There are ",n_encounters," health care encounter instances, of which ",n_visits," are visits and ",n_procedures," are procedures.\n",
        "As a sanity check, there are ",n_with_dates," encounters with dates associated - all should have dates.\n",
        "Of the visits, we have procedures for ",n_info_visits," of the visits and no information(yet) for the other ",n_noinfo_visits,".\n",
        "There are ",n_patients," patients of which ",n_info_patients," have at least one recorded procedure, and ",n_noinfo_patients," don't.\n",
        sep="");
  }



    
