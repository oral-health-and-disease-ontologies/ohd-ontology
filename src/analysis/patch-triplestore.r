## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##

# after the translated files are loaded, the following set of updates
# needs to be made to prepare the database. Call patch_triplestore. Take a nap.

## TODO: For each of these have a function that checks whether it is needed.

### propogate occurrence_date: from encounter downward (+29078)
patch_procedures_have_occurrence_dates <-function ()
{ sparqlUpdate("INSERT",
               "{ ?thing occurrence_date: ?date. }",
               "WHERE",
               "{ ?visit a outpatient_encounter:. ",
               "  ?visit occurrence_date: ?date.",
               "  ?thing is_part_of: ?visit.",
               "  ?thing a ?thingt.",
               "  ?thingt rdfs:label ?tlabel.",
               "  ?thing a process:.",
               "  optional{?thing occurrence_date: ?existing}.",
               "  filter (!bound(?existing))",
               "  }")
}
  
# oral evaluations should have specified outputs too. (+12939)
patch_oral_evaluations_have_findings <- function ()
{ sparqlUpdate("insert { ?eval has_specified_output: ?finding }",
               "where",
               "{ ?exam a dental_exam:.",
               "  ?eval is_part_of: ?exam.",
               "  ?eval a oral_evaluation:.",
               "  ?eval a ?what.",
               "  ?exam has_specified_output: ?finding.",
               "  ?finding a missing_tooth_finding:.",
               "    filter(?what != cdt_code:)",
               "}")
}

patch_bearer_of_role_participates_in_realization <- function ()
{ sparqlUpdate("INSERT",
               "  { ?thingi participates_in: ?processi }",
               "WHERE",
               "",
               "{",
               " # chain. thing inv(inheres_in) role inv(realizes) process",
               "  ?rolei  inheres_in: ?thingi.",
               "  ?processi  realizes: ?rolei.",
               "}")
}

patch_superole <- function ()
  { sparqlUpdate("insert data ",
                 "{ target_of_tooth_procedure: a role:.",
                 "  target_of_tooth_procedure: rdfs:label \"target of single tooth_procedure\".",
                 "  tooth_to_undergo_endodontic_procedure_role: rdfs:subClassOf target_of_tooth_procedure:.",
                 "  tooth_to_be_extracted_role: rdfs:subClassOf target_of_tooth_procedure:.",
                 "  tooth_to_be_restored_role: rdfs:subClassOf target_of_tooth_procedure:.",
                 "}",
                 endpoint=paste0(current_endpoint,"/statements"))
  }

# these are used in the first translation but missing from the current ontology
patch_deprecated_roles <- function ()
    { sparqlUpdate("insert data ",
                   "{ tooth_to_be_filled_role: rdfs:subClassOf tooth_to_be_restored_role: .",
                   "  tooth_to_undergo_endodontic_procedure_role: rdfs:subClassOf tooth_to_be_restored_role: .",
                   "  tooth_to_undergo_onlay_procedure_role: rdfs:subClassOf tooth_to_be_restored_role: .",
                   "  tooth_to_undergo_inlay_procedure_role: rdfs:subClassOf tooth_to_be_restored_role: .",
                   "  tooth_to_undergo_veneer_procedure_role: rdfs:subClassOf tooth_to_be_restored_role: .}")
  }




patch_triplestore <- function ()
    { if (!check_processes_have_occurrence_date())
          {cat("patching occurrence dates...");patch_procedures_have_occurrence_dates();cat("done\n");
           if (!check_processes_have_occurrence_date())
               { stop("patch_processes_have_occurrence_date failed") }};
      
      if (!check_oral_evaluations_have_findings()) 
          {cat("patching oral evaluations..."); patch_oral_evaluations_have_findings(); cat("done\n");
           if (!check_oral_evaluations_have_findings())
               { stop("patch_oral_evaluations_have_findings failed") }};
      
      if (!rdfslabel("tooth_to_be_filled_role:"))
          { cat("fixing deprecated roles...");patch_deprecated_roles();cat("done\n") };

      if (!check_bearer_of_role_participates_in_realization())
          {cat("patching participation in realization");patch_bearer_of_role_participates_in_realization();cat("done\n");
           if (!check_bearer_of_role_participates_in_realization())
               { stop("patch_bearer_of_role_participates_in_realization failed") }};
      
      if (!rdfslabel("target_of_tooth_procedure:"))
          { cat("adding target_of_tooth_procedure:...");patch_superole();cat("done") }

      nextcount <- queryc("select (count(*) as ?count) {?s next_encounter: ?l} ") 
      latercount <- queryc("select (count(*) as ?count) {?s later_encounter: ?l} ") 
      
      if (nextcount == 0)
          { cat("making later_encounter not transitive, while updating...");
            sparqlUpdate("delete data { later_encounter: a owl:TransitiveProperty.}");
            cat("done\n");
        }

      if (nextcount < 1000)
          { assert_next_encounter_links() }

      nextcount <- queryc("select (count(*) as ?count) {?s next_encounter: ?l} ") 
      latercount <- queryc("select (count(*) as ?count) {?s later_encounter: ?l} ") 

      if ((nextcount < 1000) || (latercount != nextcount)) 
          { stop("assert_next_encounter_failed") }
      

      if (latercount==nextcount)
          { cat("making later_encounter transitive again...");
            sparqlUpdate("insert data { later_encounter: a owl:TransitiveProperty.}")
            cat("done\n");
            if (latercount==nextcount) { stop("transitive later_encounter: didn't work") }
        }
  }


