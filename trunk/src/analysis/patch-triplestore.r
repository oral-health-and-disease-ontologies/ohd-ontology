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
               

patch_triplestore <- function ()
  { patch_procedures_have_occurrence_dates();
    patch_oral_evaluations_have_findings();
    patch_bearer_of_role_participates_in_realization();
    patch_superole();
    assert_next_encounter_links();
    assert_transitive_next();
  }
