## These queries check for one type of incorrect entries, those where two
## procedures on the same tooth are billed during the same visit. The
## first one counts them by type. The second lets you list all the cases
## of duplication for a type.

## About this query: Was tricky. Are we counting procedures that could be
## deleted leaving one behind? Are we counting groups of procedure?
## In the end chose to count groups of procedures and how many duplicates
## (same procedure) vs conflicts

# works. For each case of duplication/conflict one representative is returned and the count of duplicate/conflicts

# only.conflict determines whether we will count cases where there is
# duplication, but it is simple replication e.g. two resin
# restorations on the same day on the same tooth on the same surface

# count.total=T returns the count of such procedures. When false, one
# representation of each duplication/conflict group is returned with
# the stats for that group

#+++

which_procedures_have_potential_duplicates_query <- function (only.conflict=F,count.total=F)
{ paste(
    if (count.total) "SELECT (sum(?count) as ?total) where {" else "",
    "SELECT  (SAMPLE(?proc1) AS ?representative) ?date ?toothi ?toothiLabel ",
    "(COUNT(distinct ?proc1) AS ?count) ",
    "(sample(coalesce(?surface2,?surface1)) as ?surface) ",
    "(sample(coalesce(?surface2Label,?surface1Label)) as ?surfaceLabel)",
    "(SUM(if(?proc1t!=?proc2t,1,0)) as ?conflict)",
    "?patienti",
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
    if(only.conflict) {"HAVING (?conflict > 0)"} else {""},
    "ORDER BY desc(?count)",
    if(count.total) "}" else "")
}

count_which_procedures_have_potential_duplicates <- function (only.conflict=F,count.total=F)
{ res <- queryc(which_procedures_have_potential_duplicates_query(only.conflict=T,count.total=count.total))
  write.table(res,quote=F,row.names=F);
  res <<- res;
  return(res==0||nrow(res) == 0)
}

list_procedures_that_have_potential_duplicates <- function(verbose=F)
{ basic <- which_procedures_have_potential_duplicates_query(only.conflict=T)
  res <- queryc(
      "SELECT distinct ?proc2 ?proc2Label ",
      if(verbose) {"?proc1 ?proc1Label ?surface1 ?surface1Label ?surface2 ?surface2Label ?date ?toothi ?toothiLabel"},
      "where",
      "{",basic,"}",
      "BIND(?group_proc as ?proc1)",
      "OPTIONAL",
      "{  ?proc1 has_participant: ?surface1 .",
      "   ?surface1 rdf:type tooth_surface: .",
      "   ?surface1 is_part_of: ?toothi . ",
      "}",
      tooth_or_surface_procedure_pattern(proci="?proc2",surfacei="?surface2",surfaceiLabel="?surface2Label"),
      "FILTER ((((bound(?surface1)&&bound(?surface2))&&(?surface1=?surface2))||(!bound(?surface1)))||(!bound(?surface2)))",
      if(verbose) "?proc1 rdfs:label ?proc1Label.",
      if(verbose) "?proc2 rdfs:label ?proc2Label.",
      "}")
  write.table(res,quote=F,row.names=F);
  res <<- res;
  return(res==0||nrow(res) == 0)
}

get_single_dupe_group <- function (...)
    { sparql_interpolate(
        "BIND('date' AS ?date)",
        "BIND(<proc>as ?proc1)",
        "BIND(<tooth> as ?toothi)",
        "BIND(<patient> AS ?patienti)",
        "OPTIONAL",
        "{  ?proc1 has_participant: ?surface1 .",
        "   ?surface1 rdf:type tooth_surface: .",
        "   ?surface1 is_part_of: ?toothi . ",
        "}",
        tooth_or_surface_procedure_pattern(proci="?proc2",surfacei="?surface2",surfaceiLabel="?surface2Label"),
        "FILTER ((((bound(?surface1)&&bound(?surface2))&&(?surface1=?surface2))||(!bound(?surface1)))||(!bound(?surface2)))",
        #        if(verbose) "?proc1 rdfs:label ?proc1Label.",
        #        if(verbose) "?proc2 rdfs:label ?proc2Label.",
        "}")}

list_procedures_that_have_potential_duplicates_the_hard_way <- function(verbose=F)
{ reset_var_counter();
  basic <- which_procedures_have_potential_duplicates_query(only.conflict=T)
  anchors <- queryc(basic);
  colnames(anchors) # [1] "representative" "date"           "toothi"         "toothiLabel"    "count"          "surface"        "surfaceLabel"   "conflict"
  all <- c();
  for (i  in 1:nrow(anchors))
      { all <- c(queryc("SELECT distinct ?proc2 where {",
                          get_single_dupe_group(patient=paste0("<",anchors[i,"patienti"],">"),
                                                proc=paste0("<",anchors[i,"representative"],">"),
                                                tooth=paste0("<",anchors[i,"toothi"],">"),
                                                date=paste0("\"",anchors[i,"date"],"\"^^xsd:date"))),all)
    }
  all
}

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
