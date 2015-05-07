## These queries check for one type of incorrect entries, those where two
## procedures on the same tooth are billed during the same visit. The
## first one counts them by type. The second lets you list all the cases
## of duplication for a type.

## About this query: We need to do the nested select as we have to make
## sure we count all unique cases.  An earlier query had
## "distinct ?proc1t ?proc2t ?date ?tootht (sum(1) as ?count)", i.e
## without date. Since the procecure types ignore date we need to include
## it to be unique. But then we get counts by date since projected
## variables, when there is an aggregate, need to appear in both the
## select and the group by. Hence we use the outside select (careful not
## to say distinct this time) to sum up the contributions from the
## different dates.

## Having two surface restorations on the same tooth is ok, so we
## restrict cases to when only one of the procedures is a surface one.
## (str(?proc1t) <= str(?proc2t)) is to ensure that we don't get both
## ab and ba counts.  If both surfaces are bound, we insist they are
## the same. Otherwise, one can be tooth specific and the other
## surface specific.

count_duplicate_procedures <- function ()
    { res <- queryc(
        "SELECT DISTINCT  ?proc1t ?proc1tLabel ?proc2t ?proc2tLabel  (SUM(1) AS ?count) ",
        "    WHERE",
        "      { ?proc1 a dental_procedure: .",
        "        ?proc1 asserted_type: ?proc1t .",
        "		?proc1t rdfs:label ?proc1tLabel.",
        "  		?proc2 a dental_procedure: .",
        "        ?proc2 asserted_type: ?proc2t .",
        "  		?proc2t rdfs:label ?proc2tLabel .",
        "        FILTER ( (?proc1 != ?proc2) && str(?proc1) <= str(?proc2) )",
        "        ?toothi a tooth: .",
        "	    ?proc1 has_participant: ?toothi .",
        "        ?proc2 has_participant: ?toothi .",
        "        ?toothi asserted_type: ?tootht .",
        "		?tootht rdfs:label ?toothtLabel .",
        "        ?patient a patient: .",
        "  	    ?proc1 has_participant: ?patient .",
        "        ?proc2 has_participant: ?patient .",
        "  	    ?proc1 occurrence_date: ?date .",
        "        ?proc2 occurrence_date: ?date .",
        "        OPTIONAL",
        "          { ?proc1 has_participant: ?surface1 .",
        "            ?surface1 a tooth_surface:.",
        "     		?surface1 asserted_type: ?surface1t .",
        "     		?surface1t rdfs:label ?surface1tLabel .",
        "          }",
        "        OPTIONAL",
        "          { ?proc2 has_participant: ?surface2 .",
        "            ?surface2 a tooth_surface: .",
        "            ?surface2 asserted_type: ?surface2t .",
        "          }",
        "        FILTER (",
        "        	(bound(?surface1) && bound(?surface2) && ?surface1 = ?surface2 ) ||",
        "  			(!bound(?surface1) && !bound(?surface2)))	",
        "      }",
        "    GROUP BY ?proc1t ?proc2t ?proc1tLabel ?proc2tLabel ",
        "    ORDER BY desc(?count)"
        )
      write.table(res,quote=F,row.names=F);
      return (res == 0);
}

count_potential_duplicates <- function ()
{
    queryc(
        "select ?proc1tl ?proc2tl (sum(?count) as ?total) where{",
        "select distinct ?proc1tl ?proc2tl ?date ?tootht (sum(1) as ?count) where ",
        "{ ?proc1 a dental_procedure:. ?proc1 asserted_type: ?proc1t. ?proc1t rdfs:label ?proc1tl.",
        "  ?proc2 a dental_procedure:. ?proc2 asserted_type: ?proc2t. ?proc2t rdfs:label ?proc2tl.",
        "  ?proc1 has_participant: ?toothi.",
        "  ?proc2 has_participant: ?toothi.",
        "  ?toothi a tooth:. ?toothi asserted_type: ?tootht.",
        "    optional { ?proc1 has_participant: ?surface1. ?surface1 a tooth_surface:}",
        "    optional { ?proc2 has_participant: ?surface2. ?surface2 a tooth_surface:. ",
        "        ?surface2 asserted_type: ?surface2t. ?surface2t rdfs:label ?surface2l}",
        "  ?proc1 has_participant: ?patient.",
        "  ?proc2 has_participant: ?patient.",
        "    ?patient a patient:.        ",
        "  ?proc1 occurrence_date: ?date.",
        "  ?proc2 occurrence_date: ?date.  ",
        "    filter (?proc1 != ?proc2) ",
        "    filter ((!(bound(?surface1) && bound(?surface1) ) || (?surface1 = ?surface2)) && (str(?proc1t) <= str(?proc2t)))",
        "} group by ?proc1tl ?proc2tl ?tootht ?date order by desc(?count)} group by ?proc1tl ?proc2tl order by desc(?total)"
        )
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


show_potential_duplicates <- function (type1,type2)
{   if (type1 > type2)
        { temp <- type1; type1<-type2;type2<-temp}
    queryc(
        "select ?proci1l ?proci2l ?date ?patientil where {",
        "select distinct ?proc1 ?proc2 ?proci1l ?proci2l ?date ?patientil where ",
        "{ ?proc1 a dental_procedure:. ?proc1 asserted_type: ?proc1t. ?proc1t rdfs:label ?proc1tl. ?proc1 rdfs:label ?proci1l.",
        "  ?proc2 a dental_procedure:. ?proc2 asserted_type: ?proc2t. ?proc2t rdfs:label ?proc2tl. ?proc2 rdfs:label ?proci2l.",
        "  ?proc1 has_participant: ?toothi. ?proc1 asserted_type: ?proc1t. ?proc1t rdfs:label ?proc1tl.",
        "  ?proc2 has_participant: ?toothi. ?proc2 asserted_type: ?proc2t. ?proc2t rdfs:label ?proc2tl.",
        "  ?toothi a tooth:.",
        "    optional { ?proc1 has_participant: ?surface1. ?surface1 a tooth_surface:}",
        "    optional { ?proc2 has_participant: ?surface2. ?surface2 a tooth_surface:}",
        "  ?proc1 has_participant: ?patient.",
        "  ?proc2 has_participant: ?patient.",
        "    ?patient a patient:. ?patient rdfs:label ?patientil.",
        "  ?proc1 occurrence_date: ?date.",
        "  ?proc2 occurrence_date: ?date.   ",
        "   filter (?proc1 != ?proc2)",
        "    filter ((!(bound(?surface1) && bound(?surface2)) || ?surface1=?surface2) && (str(?proc1t) <= str(?proc2t)))",
        paste0("    filter(str(?proc1tl) = \"",type1,"\" && str(?proc2tl) = \"",type2,"\")"),
        "} } order by ?patientil"
        )
}

#show_potential_duplicates("endodontic procedure","endodontic procedure")

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
