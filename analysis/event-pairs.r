# Author: Alan Ruttenberg
# Project: OHD
# Date: 2015-04-24
#
# WIP survival analysis


# Querying for pairs of events for survival analysis
# Todo - bring in correlates - age, gender, tooth type

# collect all known resin restoration failures - cases where another restoration or procedure indicates failure
# ?patient - Label of patient
# ?proci1  - The initial resin restoration
# ?date1   - The date of the initial restoration
# ?proci2  - The restoration that indicates failure
# ?soonest_date2" - The date of the restoration that indicates failure
# ?previous_visit_date - The date of the visit just prior to date of the 
#   restoration that indicates failure. For later use with interval censoring.

# The query strategy is slightly odd. The job of the inner select is
# to find pairs of restoration and subsequent restoration. However We
# only want the soonest one, so we aggregate to get date2 which is
# the least date.

# However if we projected ?date2 or ?proci2 out of the inner select
# the grouping wouldn't have anything to max over. So we bind proci2
# and date2 again given the results.

#  surface_restoration_pattern() defines the contraints for the initial restoration
#  surface_restoration_failure_pattern() defines the constraints for considering it a failure

collect_restoration_failures <-function ()
    { queryc("select distinct ?patienti ?proci1 ?date1 ?birthdate ?proci2 ?soonest_date2",
             "  (max(?one_before_date) as ?previous_visit_date)",
             "  (coalesce(?is_male,?is_female,\"unrecorded\") as ?gender)",
             "  (coalesce(?is_anterior,?is_posterior,\"dunno\") as ?tooth_type)",
             "where {",
             "{select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (min(?date2) as ?soonest_date2)",
             "where {",
             patient_tooth_surface_pattern(),
             surface_restoration_pattern(proci="?proci1",date="?date1",procedure_type="resin_filling_restoration:"),
             "?proci1 later_encounter: ?proci2.",
             surface_restoration_failure_pattern(proci="?proci2",date="?date2"),
             "} group by ?patienti ?toothi ?surfacei ?proci1 ?date1",
             "}",
             surface_restoration_failure_pattern(proci="?proci2",date="?soonest_date2"),
             "?proc_minus_1 next_encounter: ?proci2.",
             "?proc_minus_1 occurrence_date: ?one_before_date.",
             gender_pattern(personi="?patienti"),
             posterior_anterior_pattern(),
             "optional {?patienti birth_date: ?birthdate.}",
             "} group by ?patienti ?proci1 ?date1 ?birthdate ?proci2 ?soonest_date2 ?is_male ?is_female ?is_anterior ?is_posterior")
  }

# Note: pairs of procedure and surface are not unique (since one procedure can be multi-surface)

# For each of the initial restorations, find the latest date that there was a visit.
# ?proci1  - The initial resin restoration
# ?date1   - The date of the initial restoration
# ?latest_date2" - The date of the last visit of any kind the patient had

# The structure of the query is very similar to the above, except we
# omit the constraint on the second visit.


collect_all_restorations_and_latest_encounter_after <-function ()
    { queryc("select distinct ?proci1 ?date1 ?birthdate ?latest_date2 ",
             "  (coalesce(?is_male,?is_female,\"unrecorded\") as ?gender)",
             "  (coalesce(?is_anterior,?is_posterior) as ?tooth_type)",
             " ?patienti",
           "where{",
           "  {select distinct ?patienti ?proci1 ?date1 ?toothi ?surfacei (max(?date2) as ?latest_date2) ",
           "    where {",
           patient_tooth_surface_pattern(),
           surface_restoration_pattern(proci="?proci1",date="?date1",procedure_type="resin_filling_restoration:"),
           "      optional",
           "      { ?proci1 later_encounter: ?proci2.",
           "      ?proci2 occurrence_date: ?date2 }",
           "      }",
           "    group by ?proci1 ?patienti ?date1 ?toothi ?surfacei ",
           "    }",
           posterior_anterior_pattern(),
           gender_pattern(personi="?patienti"),
           "optional{?patienti birth_date: ?birthdate.}",
           "} ",
           "order by ?date1")
  }

collect_restorations_followed_by_endodontic_procedure <- function ()
{ queryc("select distinct ?restoration ?restoration_date ?toothi ?root_canal ?root_canal_date ?restoration_type",
         "  (max(?one_before_date) as ?previous_visit_date)",
         "  (coalesce(?is_male,?is_female,\"unrecorded\") as ?gender)",
         "  (coalesce(?is_anterior,?is_posterior,\"dunno\") as ?tooth_type)",
         " where{",
         "{select distinct ?patienti ?restoration ?restoration_type ?toothi (min(?date2) as ?root_canal_date)",
         "where {",
         patient_tooth_pattern(),
         tooth_restoration_pattern(proci="?restoration",date="?restoration_date",procedure_type="tooth_restoration_procedure:"),
         "?restoration asserted_type: ?restoration_type.",
         "?restoration later_encounter: ?root_canal.",
         root_canal_failure_pattern(proci="?root_canal",date="?date2"),
         "} group by ?patienti ?toothi ?surfacei ?root_canal ?restoration_date ?restoration ?restoration_type",
         "}",
         root_canal_failure_pattern(proci="?root_canal",date="?root_canal_date"),
         "?proc_minus_1 next_encounter: ?root_canal.",
         "?proc_minus_1 occurrence_date: ?one_before_date.",
         gender_pattern(personi="?patienti"),
         "optional {?patienti birth_date: ?birthdate.}",
         "} group by ?patienti ?restoration m?restoration_date ?toothi ?root_canal ?root_canal_date ?restoration_type ?birthdate ?is_male ?is_female ?is_anterior ?is_posterior")
}


## The issue here is that the restoration that precedes a root canal is
## not always single-values. A simple case is that there are two single
## surface restorations, and the next thing that happens to the tooth is
## is a root canal.

## This makes it tricky to count. Does one give split credit to the two
## surface restorations? Before I realized this I was trying to get a
## single row per endo, and then count by restoration type. That
## doesn't work.

## So what is desirable?

## - In the unique cases the restoration, specified to the surfaces that were restored.?
## - Perhaps the characterization of the state of the tooth just before - something about every surface.
## - Perhaps, when there is more than one restoration choose randomly (with sparql sample)
## -

count_restorations_followed_by_endodontic_procedure <- function()
{ queryc("select distinct ?restoration_type (count(?restoration_type) as ?count)",
         " where{",
         "{select distinct ?patienti  (max(?restoration_date) as ?latest_restoration_date) ?restoration_type ?toothi (min(?date2) as ?root_canal_date)",
         "where {",
         patient_tooth_pattern(),
         tooth_restoration_pattern(proci="?restoration",date="?restoration_date",procedure_type="tooth_restoration_procedure:"),
         "?restoration asserted_type: ?restoration_typec.",
         "?restoration_typec rdfs:label ?restoration_type.",
         "?restoration later_encounter: ?root_canal.",
         "?restoration a tooth_restoration_procedure:.",
         root_canal_failure_pattern(proci="?root_canal",date="?date2"),
         "} group by ?patienti ?toothi ?surfacei ?root_canal ?restoration_date ?restoration_type",
         "}",
         "?restoration occurrence_date: ?latest_restoration_date.",
         "?restoration a tooth_restoration_procedure:.",
         "?restoration has_participant: ?toothi.",
         root_canal_failure_pattern(proci="?root_canal",date="?root_canal_date"),
         "?proc_minus_1 next_encounter: ?root_canal.",
         "?proc_minus_1 occurrence_date: ?one_before_date.",
         gender_pattern(personi="?patienti"),
         "optional {?patienti birth_date: ?birthdate.}",
         "} group by ?restoration_type order by desc(?count)"
         )
}
         

role_inheres_realizes_pattern <- function (...) #role_type, bearer, procedure)
  { print("hello")
    bgp = tb(
      "      ?proci a proc_type: .",
      "      _:role a role_type: .",
      "      _:role inheres_in: ?bearer .",
      "      ?proci realizes: _:role .")
    sparql_interpolate(bgp);
  }

surface_restoration_pattern <- function (...)
    {  sparql_interpolate(
        "?proci a procedure_type:.",
        "_:role a tooth_to_be_restored_role:.",
        "_:role  inheres_in: ?toothi.",
        "?proci realizes: _:role.",
        "?proci occurrence_date: ?date.", 
        "?proci has_participant: ?surfacei."
        );
  }

tooth_restoration_pattern <- function (...)
    {  sparql_interpolate(
        "?proci a procedure_type:.",
        "_:role a tooth_to_be_restored_role:.",
        "_:role  inheres_in: ?toothi.",
        "?proci realizes: _:role.",
        "?proci occurrence_date: ?date.", 
        "?proci has_participant: ?toothi."
        );
  }


surface_restoration_failure_pattern <- function (...)
    { sparql_interpolate(
        sparql_union(
          tb(
            sparql_union("?proci a tooth_restoration_procedure:.",
                         "?proci a inlay_restoration:."
                         ),
            "_:role a tooth_to_be_restored_role: .",
            "_:role inheres_in: ?toothi.",
            "?proci realizes: _:role .",
            "?proci occurrence_date: ?date .", 
            "?proci has_participant: ?surfacei ."),
          tb(sparql_union("?proci a crown_restoration: .",
                          "?proci a tooth_extraction: .",
                          "?proci a endodontic_procedure: ."
                          ),
             "_:role1 a target_of_tooth_procedure: .",
             "_:role1 inheres_in: ?toothi.",
             "?proci realizes: _:role1 .",
             "?proci occurrence_date: ?date.")))
       #      ,missing_tooth_pattern(exam="?proci",tooth="toothi",date,patient)
  }

patient_tooth_surface_pattern <- function (...)
    {
        sparql_interpolate(
            "?patienti a homo_sapiens: .",
            "?toothi rdf:type tooth: .",
            "?toothi is_part_of: ?patienti .",
            "?surfacei rdf:type tooth_surface: .",
            "?surfacei is_part_of: ?toothi .")
    }

patient_tooth_pattern <- function (...)
    {
        sparql_interpolate(
            "?patienti a homo_sapiens: .",
            "?toothi rdf:type tooth: .",
            "?toothi is_part_of: ?patienti ."
            )
    }

root_canal_failure_pattern <- function(...)
    { sparql_interpolate(
        "?proci a endodontic_procedure: .",
        "_:role1 a tooth_to_undergo_endodontic_procedure_role:.",
        "_:role1 inheres_in: ?toothi.",
        "?proci realizes: _:role1 .",
        "?proci has_participant: ?toothi.",
        "?proci occurrence_date: ?date.")
  }


#constrained: ?patient,?tooth (which used to exist), 
#constrains: ?proci,?date (when it doesn't exist)

missing_tooth_pattern <- function(...){
    sparql_interpolate(
        "?patienti participates_in: ?exam.",
        "?exam a oral_evaluation:.",
        "?exam occurrence_date: ?date.",
        "?exam has_specified_output: _:finding",
        "_:finding is_about: ?patient.",
        "_:finding a missing_tooth_finding:.",
        "?tooth_number is_part_of: _:finding.",
        "?toothi a _:tooth_class.",
        "?tooth_number is_about: _:tooth_class."
        )
}

posterior_anterior_pattern <- function(...) 
    { sparql_interpolate(
        "optional { BIND(\"posterior\" as ?is_posterior). {{?toothi a pre_molar:.} UNION {?toothi a molar:}} }",
        "optional { BIND(\"anterior\" as ?is_anterior). {{?toothi a canine:.} UNION {?toothi a incisor:}} }"
        )
  }
        #"BIND(coalesce(?is_anterior,?is_posterior) as ?tooth_type)}"


gender_pattern <- function(...)
    { sparql_interpolate(
        "optional { BIND(\"male\" as ?is_male). ?personi a male:.}",
        "optional { BIND(\"female\" as ?is_female). ?personi a female:.}");
  }
        #"BIND(coalesce(?is_male,?is_female,\"unrecorded\") as ?gender)}"

