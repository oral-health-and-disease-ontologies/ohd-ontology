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

tooth_or_surface_procedure_pattern <- function (...)
    {
        sparql_interpolate(
            "# tooth and surface restoration template",
            "?proci a dental_procedure:.",
            "?patienti a homo_sapiens: .",
            "?patienti participates_in: ?proci.",
            "?proci realizes:  _:role.",
            "_:role a tooth_to_be_restored_role:.",
            "_:role  inheres_in: ?toothi.",
            "?proci occurrence_date: ?date.", 
            "?patienti a homo_sapiens: .",
            "?toothi rdf:type tooth: .",
            "?toothi is_part_of: ?patienti .",
            "optional { ?surfacei rdf:type tooth_surface:.",         
            "           ?proci has_participant: ?surfacei.",
            "           ?surfacei is_part_of: ?toothi .",
            "           ?surfacei rdfs:label ?surfaceiLabel.",
            "         }"
            );
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

missing_tooth_pattern <- function(...)
{ sparql_interpolate(
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

labels_pattern <-function (...)
    { vars <- lapply(list(...),function(el){ paste0(el," rdfs:label ",el,"Label.")}) ;
      do.call(paste,c(vars,sep="\n"))
  }
