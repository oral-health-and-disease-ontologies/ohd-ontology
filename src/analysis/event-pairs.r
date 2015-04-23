## Under construction, for survival analysis

collect_restoration_failures <-function ()
  { cat(querystring("select distinct ?patient ?patienti ?tooth ?surface ?proci1 ?procedure1 ?date1 ?procedure2 ?one_before_date ?soonest_date2",
           "where {",
           "{select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (min(?date2) as ?soonest_date2)",
           "where {",
           "?patienti a homo_sapiens: .",
           "?toothi rdf:type tooth: .",
           "?toothi asserted_type: ?toothtypei. ",
           "?toothi is_part_of: ?patienti .",
           "?surfacei rdf:type tooth_surface: .",
           "?surfacei asserted_type: ?surfacetypei .",
           "?surfacei is_part_of: ?toothi .",
                                        #
           surface_restoration_pattern("?proci1","?toothi","?surfacei","?date1"),
                                        #
           "?proci1 later_encounter: ?proci2.",
                                        #
           surface_restoration_failure_pattern("?proci2","?toothi","?surfacei","?date2","?patienti"),
                                        #
                                        #     "           "filter  (?patienti=<http://purl.obolibrary.org/obo/ohd/individuals/I_5ae44155bc98eb5243c9daea454b877b>)",
           "} group by ?patienti ?toothi ?surfacei ?proci1 ?date1",
           "}",
                                        #
           surface_restoration_failure_pattern("?proci2","?toothi","?surfacei","?soonest_date2","?patienti"),
                                        #
           "?proc_minus_1 next_encounter: ?proci2.",
           "?proc_minus_1 occurrence_date: ?one_before_date.",

           "?patienti rdfs:label ?patient.",
           "?toothi rdfs:label ?tooth .",
           "?surfacei rdfs:label ?surface .",
           "?proci1 rdfs:label ?procedure1 .",
           "?proci2 rdfs:label ?procedure2 .",
           "} "))
  }



#we have the known pairs.
#we want restorations with no later visits
#and restorations with a later visit of any kind

# all[,"date1"][is.na(all[,"latest_date2"])] works to get the NA - no followup cases.

# pairs of procedure and surface are unique (since one procedure can include several restorations)
collect_all_restorations_and_latest_followup <-function ()
  { queryc("select distinct ?patient ?patienti ?tooth ?surface ?procedure1 ?proci1 ?date1 ?latest_date2",
           "where{",
           "  {select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (max(?date2) as ?latest_date2) ",
           "    where {",
           "      ?patienti a homo_sapiens: .",
           "      ?toothi rdf:type tooth: .",
           "      ?toothi asserted_type: ?toothtypei. ",
           "      ?toothi is_part_of: ?patienti .",
           "      ?surfacei rdf:type tooth_surface: .",
           "      ?surfacei asserted_type: ?surfacetypei .",
           surface_restoration_pattern("?proci1","?toothi","?surfacei","?date1"),
           "      ?surfacei is_part_of: ?toothi .",
           "      ?proci1 a tooth_restoration_procedure: .",
           "      ?role2 a tooth_to_be_restored_role: .",
           "      ?role2 inheres_in: ?toothi .",
           "      ?proci1 realizes: ?role2 .",
           "      ?proci1 occurrence_date: ?date1.",
           "      ?proci1 has_participant: ?surfacei .",
           "      optional",
           "      { ?proci1 later_encounter: ?proci2.",
           "      ?proci2 occurrence_date: ?date2 }",
           "      }",
           "    group by ?proci1 ?patienti ?date1 ?toothi ?surfacei order by ?date1",
           "    }",
           "   optional",
           "  { ?proci1 later_encounter: ?proci2.",
           "    ?proci2 occurrence_date: ?latest_date2.",
           "    ?proci2 rdfs:label ?procedure2 }",
           "  ?patienti rdfs:label ?patient.",
           "  ?toothi rdfs:label ?tooth .",
           "  ?surfacei rdfs:label ?surface .",
           "  ?proci1 rdfs:label ?procedure1 .",
           "} ",
           "order by ?date1")
  }





##           "filter  (?patienti=<http://purl.obolibrary.org/obo/ohd/individuals/I_5ae44155bc98eb5243c9daea454b877b>)",
                            
## Results for PREFIX rdf:... (100  of 5865 ) mine
## Results for PREFIX rdf:... (100  of 5373 ) his??

surface_restoration_pattern <- function (procvar="?proci",toothvar="?toothi",surfacevar="?surfacei",datevar="?date")
  { rolevar <- genvar("role")
    paste(procvar," a tooth_restoration_procedure: .\n",
          rolevar," a tooth_to_be_restored_role: .\n",
          rolevar," inheres_in: ", toothvar," .\n",
          procvar," realizes: ",rolevar," .\n",
          procvar," occurrence_date: ",datevar,".\n", 
          procvar," has_participant: ",surfacevar," .\n",
          sep="");
  }

surface_restoration_failure_pattern <- function (proc,tooth,surface,date,patient)
  { role <- genvar("role")
    sparql_union_pattern(
      paste(sparql_union_pattern(paste0(proc," a tooth_restoration_procedure: .\n"), paste0(proc," a inlay_restoration: .\n")),
            role," a tooth_to_be_restored_role: .\n",
            role," inheres_in: ", tooth," .\n",
            proc," realizes: ",role," .\n",
            proc," occurrence_date: ",date,".\n", 
            proc," has_participant: ",surface," .\n",
            sep=""),
      paste(proc," a crown_restoration: .\n",
            role," a tooth_to_be_restored_role: .\n",
            role," inheres_in: ", tooth," .\n",
            proc," realizes: ",role," .\n",
            proc," occurrence_date: ",date,".\n", 
            sep="")
#      ,missing_tooth_pattern(proc,tooth,date,patient)
      )
  }

specific_person_surface_example <- function()
  { paste("filter(?surfacei = <http://purl.obolibrary.org/obo/ohd/individuals/I_a68e153c890465735df45079bdd51fbf>)",
         "filter(?patienti = <http://purl.obolibrary.org/obo/ohd/individuals/I_b986ac2ec3449a1812641001537c5444>)",
         sep="\n")
  }

#constrained: ?patient,?tooth (which used to exist), 
#constrains: ?proci,?date (when it doesn't exist)

missing_tooth_pattern <- function(exam="?proci",tooth="?toothi",date="?date",patient="?patient")
{
  finding <- genvar("finding");
  tooth_number <- genvar("tooth_number");
  tooth_class <- genvar("tooth_class")
  paste0("?patienti participates_in: ",exam,". \n",
         exam," a oral_evaluation:.",
         exam," occurrence_date: ",date,". \n",
         exam," has_specified_output: ",finding,". \n",
         finding," is_about: ", patient,". \n",
         finding," a missing_tooth_finding:. \n",
         tooth_number," is_part_of: ",finding,". \n",
         tooth," a ", tooth_class, ". \n",
         tooth_number," is_about: ",tooth_class,". \n"
         )
}

         
