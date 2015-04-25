## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##
## WIP survival analysis


## Querying for pairs of events for survival analysis
## Todo - bring in correlates - age, gender, tooth type

## collect all known resin restoration failures - cases where another restoration or procedure indicates failure
## ?patient - Label of patient
## ?proci1  - The initial resin restoration
## ?date1   - The date of the initial restoration
## ?proci2  - The restoration that indicates failure
## ?soonest_date2" - The date of the restoration that indicates failure
## ?previous_visit_date - The date of the visit just prior to date of the 
##   restoration that indicates failure. For later use with interval censoring.

## The query strategy is slightly odd. The job of the inner select is
## to find pairs of restoration and subsequent restoration. However We
## only want the soonest one, so we aggregate to get date2 which is
## the least date.

## However if we projected ?date2 or ?proci2 out of the inner select
## the grouping wouldn't have anything to max over. So we bind proci2
## and date2 again given the results.

##  surface_restoration_pattern() defines the contraints for the initial restoration
##  surface_restoration_failure_pattern() defines the constraints for considering it a failure

collect_restoration_failures <-function ()
  { queryc("select distinct ?patient ?proci1 ?date1 ?proci2 (max(?one_before_date) as ?previous_visit_date) ?soonest_date2",
           "where {",
           "{select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (min(?date2) as ?soonest_date2)",
           "where {",
           "?patienti a homo_sapiens: .",
           "?toothi rdf:type tooth: .",
           "?toothi asserted_type: ?toothtypei. ",
           "?toothi is_part_of: ?patienti .",
           "?surfacei rdf:type tooth_surface: .",
           ##" {{?toothi a pre_molar:}  UNION {?toothi a molar:}}",
           ##" {{?toothi a incisor:}  UNION {?toothi a canine:}}",
           "?surfacei asserted_type: ?surfacetypei .",
           "?surfacei is_part_of: ?toothi .",
           surface_restoration_pattern("?proci1","?toothi","?surfacei","?date1"),
           "?proci1 later_encounter: ?proci2.",
           surface_restoration_failure_pattern("?proci2","?toothi","?surfacei","?date2","?patienti"),
           ##  "filter  (?patienti=ohdi:I_5ae44155bc98eb5243c9daea454b877b)",
           "} group by ?patienti ?toothi ?surfacei ?proci1 ?date1",
           "}",
           surface_restoration_failure_pattern("?proci2","?toothi","?surfacei","?soonest_date2","?patienti"),
           "?proc_minus_1 next_encounter: ?proci2.",
           "?proc_minus_1 occurrence_date: ?one_before_date.",
           "?patienti rdfs:label ?patient.",
           "?toothi rdfs:label ?tooth .",
           "?surfacei rdfs:label ?surface .",
           "?proci1 rdfs:label ?procedure1 .",
           "?proci2 rdfs:label ?procedure2 .",
           "} group by ?patient ?proci1 ?date1 ?proci2 ?soonest_date2")
  }




## Note: pairs of procedure and surface are not unique (since one procedure can be multi-surface)

## For each of the initial restorations, find the latest date that there was a visit.
## ?proci1  - The initial resin restoration
## ?date1   - The date of the initial restoration
## ?latest_date2" - The date of the last visit of any kind the patient had

## The structure of the query is very similar to the above, except we
## omit the constraint on the second visit.

collect_all_restorations_and_latest_followup <-function (whichtype="{{?toothi a pre_molar:} UNION {?toothi a molar:}})")
  { queryc("select distinct ?proci1 ?date1 ?latest_date2",
           "where{",
           "  {select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (max(?date2) as ?latest_date2) ",
           "    where {",
           "      ?patienti a homo_sapiens: .",
           "      ?toothi rdf:type tooth: .",
           "      ?toothi asserted_type: ?toothtypei. ",
           ##" {{?toothi a incisor:}  UNION {?toothi a canine:}}",
           ##" {{?toothi a pre_molar:}  UNION {?toothi a molar:}}",
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

surface_restoration_pattern <- function (procvar="?proci",toothvar="?toothi",surfacevar="?surfacei",datevar="?date")
  { rolevar <- genvar("role")
    paste(procvar," a resin_filling_restoration: .\n",
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
            sep=""),
        paste(proc," a tooth_extraction: .\n",
              role," a tooth_to_be_extracted_role: .\n",
            role," inheres_in: ", tooth," .\n",
            proc," realizes: ",role," .\n",
            proc," occurrence_date: ",date,".\n", 
            sep=""),
        paste(proc," a endodontic_procedure: .\n",
              role," a tooth_to_be_extracted_role: .\n",
              role," inheres_in: ", tooth," .\n",
              proc," realizes: ",role," .\n",
              proc," occurrence_date: ",date,".\n", 
              sep="")
#      ,missing_tooth_pattern(proc,tooth,date,patient)
      )
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
         

## debugging
specific_person_surface_example <- function()
  { paste("filter(?surfacei = <http://purl.obolibrary.org/obo/ohd/individuals/I_a68e153c890465735df45079bdd51fbf>)",
         "filter(?patienti = <http://purl.obolibrary.org/obo/ohd/individuals/I_b986ac2ec3449a1812641001537c5444>)",
         sep="\n")
  }


##           "filter  (?patienti=<http://purl.obolibrary.org/obo/ohd/individuals/I_5ae44155bc98eb5243c9daea454b877b>)",
