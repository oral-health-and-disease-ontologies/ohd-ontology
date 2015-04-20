system.time(queryc
    ("select distinct ?patient ?patienti ?tooth ?surface ?procedure1 ?date1 ?procedure2 ?soonest_date2",
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
     surface_restoration_failure_pattern("?proci2","?toothi","?surfacei","?date2"),

     "} group by ?patienti ?toothi ?surfacei ?proci1 ?date1 }",
                                        #
     surface_restoration_failure_pattern("?proci2","?toothi","?surfacei","?soonest_date2"),
                                        #
     "?patienti rdfs:label ?patient.",
     "?toothi rdfs:label ?tooth .",
     "?surfacei rdfs:label ?surface .",
     "?proci1 rdfs:label ?procedure1 .",
     "?proci2 rdfs:label ?procedure2 .",
     "}"))
                            

## Results for PREFIX rdf:... (100  of 5865 ) mine
## Results for PREFIX rdf:... (100  of 5373 ) his??



surface_restoration_pattern <- function (procvar="?proci",toothvar="?toothi",surfacevar="?surfacei",datevar="?date")
  { rolevar <- paste("?role",gensymcounter,sep="");
    gensymcounter <<- gensymcounter+1;
    paste(procvar," a tooth_restoration_procedure: .\n",
          rolevar," a tooth_to_be_restored_role: .\n",
          rolevar," inheres_in: ", toothvar," .\n",
          procvar," realizes: ",rolevar," .\n",
          procvar," occurrence_date: ",datevar,".\n", 
          procvar," has_participant: ",surfacevar," .\n",
          sep="");
  }

surface_restoration_failure_pattern <- function (procvar="?proci",toothvar="?toothi",surfacevar="?surfacei",datevar="?date")
  { rolevar <- paste("?role",gensymcounter,sep="");
    gensymcounter <<- gensymcounter+1;
    sparql_union_pattern(
      paste(sparql_union_pattern(paste0(procvar," a tooth_restoration_procedure: .\n"), paste0(procvar," a inlay_restoration: .\n")),
            rolevar," a tooth_to_be_restored_role: .\n",
            rolevar," inheres_in: ", toothvar," .\n",
            procvar," realizes: ",rolevar," .\n",
            procvar," occurrence_date: ",datevar,".\n", 
            procvar," has_participant: ",surfacevar," .\n",
            sep=""),
      paste(procvar," a crown_restoration: .\n",
            rolevar," a tooth_to_be_restored_role: .\n",
            rolevar," inheres_in: ", toothvar," .\n",
            procvar," realizes: ",rolevar," .\n",
            procvar," occurrence_date: ",datevar,".\n", 
            sep=""));
  }

specific_person_surface_example <- function()
  { paste("filter(?surfacei = <http://purl.obolibrary.org/obo/ohd/individuals/I_a68e153c890465735df45079bdd51fbf>)",
         "filter(?patienti = <http://purl.obolibrary.org/obo/ohd/individuals/I_b986ac2ec3449a1812641001537c5444>)",
         sep="\n")
  }

gensymcounter <- 1;

sparql_union_pattern <- function(...)
{   paste0("{{",paste(...,sep="} UNION {"),"}}")
}
  
