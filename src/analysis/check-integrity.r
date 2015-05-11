## These queries check for one type of incorrect entries, those where two
## procedures on the same tooth are billed during the same visit. The
## first one counts them by type. The second lets you list all the cases
## of duplication for a type.

## About this query: Was tricky. Are we counting procedures that could be
## deleted leaving one behind? Are we counting groups of procedure?
## In the end chose to count groups of procedures and how many duplicates
## (same procedure) vs conflicts

# works. For each case of duplication/conflict one representative is returned and the count of duplicate/conflicts
count_which_procedures_have_potential_duplicates <- function ()
{ res <- queryc(
    "SELECT  (SAMPLE(?proc1Label) AS ?representative) ?date ?toothi ?toothiLabel ",
    "(COUNT(distinct ?proc1) AS ?count) ",
    "(sample(coalesce(?surface2,?surface1)) as ?surface) ",
    "(sample(coalesce(?surface2Label,?surface1Label)) as ?surfaceLabel)",
    "WHERE { ",
    tooth_or_surface_procedure_pattern(proci="?proc1",surfacei="?surface1",surfaceiLabel="?surface1Label"),
    tooth_or_surface_procedure_pattern(proci="?proc2",surfacei="?surface2",surfaceiLabel="?surface2Label"),
    "?proc1 asserted_type: ?proc1t.",
    "?proc2 asserted_type: ?proc2t.",
    " filter((bound(?surface1) && bound(?surface2) && ?surface1=?surface2) || !bound(?surface1) || !bound(?surface2))",
    " filter(?proc1 != ?proc2) ",
    labels_pattern("?toothi","?proc1"),
    " }",
    "GROUP BY ?patienti ?toothi ?toothiLabel coalesce(?surface2,?surface1) ?date  ",
    "ORDER BY desc(?count)"
    );
  write.table(res,quote=F,row.names=F);
  res <<- res;
  return(res==0||nrow(res) == 0)
}
  

# not quite ready
mark_potential_duplicates <- function ()
    { sparqlUpdate(
        "INSERT { ?proc1 duplicate: \"TRUE\"^^xsd:boolean. ?proc2 duplicate: \"TRUE\"^^xsd:boolean.}",
        "WHERE {",
        tooth_or_surface_procedure_pattern(proci="?proc1",surfacei="?surface1"),
        tooth_or_surface_procedure_pattern(proci="?proc2",surfacei="?surface2"),
        "?proc1 asserted_type: ?proc1t.",
        "?proc2 asserted_type: ?proc2t.",
        " filter((bound(?surface1) && bound(?surface2) && ?surface1=?surface2) || !bound(?surface1) || !bound(?surface2))",
        " filter(?proc1 != ?proc2 && ( str(?proc1) <= str(?proc2) ) && ( str(?proc1t) <= str(?proc2t) ))",
        "}")
  }

#get_potential_duplicates("endodontic procedure","endodontic procedure")

check_processes_have_occurrence_date <- function()
{ result <- queryc(
    "select (count(?thing) as ?should_be_zero) WHERE",
    "{ ?visit a outpatient_encounter:. ",
    "  ?visit occurrence_date: ?date.",
    "  ?thing is_part_of: ?visit.",
    "  ?thing a ?thingt.",
    "  ?thingt rdfs:label ?tlabel.",
    "  ?thing a process:.",
    "  optional{?thing occurrence_date: ?existing}.",
    "  filter (!bound(?existing))",
    "  }");
  if (length(result)==0||result==0) {return(TRUE)}
  else
      {warning("you probably need to run patch_procedures_have_occurrence_dates");
       return(FALSE)}
}

check_oral_evaluations_have_findings <-function()
{ result <- queryc(
    "select (count(?eval) as ?should_be_zero) where",
    "{ ?exam a dental_exam:.",
    "  ?eval is_part_of: ?exam.",
    "  ?eval a oral_evaluation:.",
    "  ?eval a ?what.",
    "  ?exam has_specified_output: ?finding.",
    "  optional {BIND(1 as ?has_it). ?eval has_specified_output: ?finding.  }",
    "  ?finding a missing_tooth_finding:.",
    "  filter(?what != cdt_code:)",
    "filter (!bound(?has_it))",
    "}");
  if (length(result)==0||result==0) {return(TRUE)}
  else
      {warning(paste0("Found ",result,". you probably need to run patch_oral_evaluations_have_findings"));
       return(FALSE)}
}

check_bearer_of_role_participates_in_realization <- function ()
{ result<- queryc(
    "select (count(?bearer) as ?should_be_zero) where {",
    "  ?rolei  inheres_in: ?thingi.",
    "  ?processi  realizes: ?rolei.",
    "  optional {BIND(1 as ?does). ?bearer participates_in: ?processi}",
    "  filter(!bound(?does))",
    "}")
  if (length(result)==0||result==0) {return(TRUE)}
  else
      {warning(paste0("Found ",result,". you probably need to run patch_bearer_of_role_participates_in_realization"));
       return(FALSE)}
}
## This is what the answer looked like for Claudio's set. 
##      proc1tl                                 proc2tl                                 count
##  [1,] "endodontic procedure"                  "crown restoration"                     "608"
##  [2,] "crown restoration"                     "crown restoration"                     "482"
##  [3,] "endodontic procedure"                  "resin filling restoration"             "170"
##  [4,] "crown restoration"                     "resin filling restoration"             "89" 
##  [5,] "endodontic procedure"                  "endodontic procedure"                  "62" 
##  [6,] "endodontic procedure"                  "amalgam filling restoration"           "50" 
##  [7,] "crown restoration"                     "amalgam filling restoration"           "47" 
##  [8,] "crown restoration"                     "tooth extraction procedure"            "34" 
##  [9,] "porcelain laminate veneer restoration" "porcelain laminate veneer restoration" "32" 
## [10,] "endodontic procedure"                  "ceramic onlay restoration"             "25" 
## [11,] "tooth extraction procedure"            "tooth extraction procedure"            "20" 
## [12,] "crown restoration"                     "ceramic onlay restoration"             "18" 
## [13,] "endodontic procedure"                  "metallic onlay restoration"            "9"  
## [14,] "endodontic procedure"                  "porcelain laminate veneer restoration" "4"  
## [15,] "crown restoration"                     "porcelain laminate veneer restoration" "3"  
## [16,] "porcelain laminate veneer restoration" "tooth extraction procedure"            "2"  
## [17,] "endodontic procedure"                  "resin laminate veneer restoration"     "1"  
## [18,] "endodontic procedure"                  "tooth extraction procedure"            "1"
