## variables used for getting Caplan data

## column names in Caplan spreadsheet
._caplan.column.names <- c("patientid", "sex", "birthdate", "tthnum", "procdate","procclass", "proccode",
                          "matm", "mato", "matd", "matf", "matl","dxm", "dxo", "dxd", "dxf", "dxl", "provider")
## string for test query
._test_query <-
"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
select ?s ?l where {
?s rdfs:label ?l .
} limit 10"

## string fo prefixes
._prefixes <-
"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX obo: <http://purl.obolibrary.org/obo>
PREFIX assertedtype: <http://purl.obolibrary.org/obo/OHD_0000092>
PREFIX dentalpatient: <http://purl.obolibrary.org/obo/OHD_0000012> "

## strings for finding patients
._patients_query_count <- 
"SELECT (count(distinct ?patientid) as ?count)
WHERE { 
?patienttype rdfs:subClassOf dentalpatient: . 
?patienti assertedtype: ?patienttype . 
?patienti rdfs:label ?patientid . 
} "
._patients_query_count <- paste(._prefixes,._patients_query_count)

._patient_get_count_query <- 
"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT (count(distinct ?patientid) as ?count)
WHERE { 
?patienttype rdfs:subClassOf <http://purl.obolibrary.org/obo/OHD_0000012> . 
?patienti <http://purl.obolibrary.org/obo/OHD_0000092> ?patienttype . 
?patienti rdfs:label ?patientid . 
} "

._patient_list_query <- 
"SELECT distinct ?patientid
WHERE { 
?patienttype rdfs:subClassOf dentalpatient: . 
?patienti assertedtype: ?patienttype . 
?patienti rdfs:label ?patientid . 
}
ORDER BY ?patientid
LIMIT 10 "
._patient_list_query <- paste(._prefixes, ._patient_list_query)


## this query is used to retrieve data for Caplan spreadsheet
## NB: it is syntactically not complete. the missing bits at
##     the end are filled in by get.caplan.query()
._caplan.query <-
"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?patientid ?sex ?birthdate ?tthnum ?procdate ?procclass ?proccode ?matm ?mato ?matd ?matf ?matl ?dxm ?dxo ?dxd ?dxf ?dxl ?provider
WHERE { 
?patienttype rdfs:subClassOf obo:OHD_0000012 . 
?patienti obo:OHD_0000092 ?patienttype . 
?patienti rdfs:label ?patientid . 
?patienttype rdfs:label ?sex . 
?patienti obo:OHD_0000050 ?birthdate . 
?toothtype rdfs:subClassOf obo:FMA_12516 . 
?toothtype obo:OHD_0000065 ?tthnum . 
 { 
?toothi obo:OHD_0000092 ?toothtype . 
?toothi obo:BFO_0000050 ?patienti . 
?proceduretype rdfs:subClassOf obo:OHD_0000002 . 
?procedurei obo:OHD_0000092 ?proceduretype . 
?procedurei obo:BFO_0000057 ?patienti . 
?procedurei obo:BFO_0000057 ?toothi . 
?proceduretype rdfs:label ?procclass . 
?procedurei obo:OHD_0000015 ?procdate . 
 { 
?toothroletype rdfs:subClassOf obo:OHD_0000007 . } UNION 
 { 
?toothroletype rdfs:subClassOf obo:OHD_0000058 . } UNION 
 { 
?toothroletype rdfs:subClassOf obo:OHD_0000056 . }.
?toothrolei obo:OHD_0000092 ?toothroletype . 
?toothrolei obo:BFO_0000052 ?toothi . 
?procedurei obo:BFO_0000055 ?toothrolei . 
OPTIONAL { 
?codetype rdfs:subClassOf obo:CDT_1000001 . 
?codei obo:OHD_0000092 ?codetype . 
?codei obo:IAO_0000136 ?procedurei . 
?codetype rdfs:label ?proccode . }.
OPTIONAL { 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?procedurei obo:BFO_0000057 ?provideri . 
?provideri rdfs:label ?provider . }.
OPTIONAL { 
?surfacemi obo:OHD_0000092 obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth . 
?surfacemi obo:BFO_0000050 ?toothi . 
?surfacemi rdfs:label ?surfacem . 
?materialtypem rdfs:subClassOf obo:OHD_0000000 . 
?matmi obo:OHD_0000092 ?materialtypem . 
?matmi obo:BFO_0000082 ?toothi . 
?matmi obo:OHD_0000091 ?surfacemi . 
?procedurei obo:BFO_0000057 ?surfacemi . 
?procedurei obo:BFO_0000057 ?matmi . 
?matmi rdfs:label ?matm . }.
OPTIONAL { 
?surfaceoi obo:OHD_0000092 obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth . 
?surfaceoi obo:BFO_0000050 ?toothi . 
?surfaceoi rdfs:label ?surfaceo . 
?materialtypeo rdfs:subClassOf obo:OHD_0000000 . 
?matoi obo:OHD_0000092 ?materialtypeo . 
?matoi obo:BFO_0000082 ?toothi . 
?matoi obo:OHD_0000091 ?surfaceoi . 
?procedurei obo:BFO_0000057 ?surfaceoi . 
?procedurei obo:BFO_0000057 ?matoi . 
?matoi rdfs:label ?mato . }.
OPTIONAL { 
?surfacedi obo:OHD_0000092 obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth . 
?surfacedi obo:BFO_0000050 ?toothi . 
?surfacedi rdfs:label ?surfaced . 
?materialtyped rdfs:subClassOf obo:OHD_0000000 . 
?matdi obo:OHD_0000092 ?materialtyped . 
?matdi obo:BFO_0000082 ?toothi . 
?matdi obo:OHD_0000091 ?surfacedi . 
?procedurei obo:BFO_0000057 ?surfacedi . 
?procedurei obo:BFO_0000057 ?matdi . 
?matdi rdfs:label ?matd . }.
OPTIONAL { 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth . } UNION 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth . }.
?surfacefi obo:BFO_0000050 ?toothi . 
?surfacefi rdfs:label ?surfacef . 
?materialtypef rdfs:subClassOf obo:OHD_0000000 . 
?matfi obo:OHD_0000092 ?materialtypef . 
?matfi obo:BFO_0000082 ?toothi . 
?matfi obo:OHD_0000091 ?surfacefi . 
?procedurei obo:BFO_0000057 ?surfacefi . 
?procedurei obo:BFO_0000057 ?matfi . 
?matfi rdfs:label ?matf . }.
OPTIONAL { 
?surfaceli obo:OHD_0000092 obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth . 
?surfaceli obo:BFO_0000050 ?toothi . 
?surfaceli rdfs:label ?surfacel . 
?materialtypel rdfs:subClassOf obo:OHD_0000000 . 
?matli obo:OHD_0000092 ?materialtypel . 
?matli obo:BFO_0000082 ?toothi . 
?matli obo:OHD_0000091 ?surfaceli . 
?procedurei obo:BFO_0000057 ?surfaceli . 
?procedurei obo:BFO_0000057 ?matli . 
?matli rdfs:label ?matl . }.} UNION 
 { 
?utoothi obo:OHD_0000092 ?toothtype . 
?utoothi obo:BFO_0000050 ?patienti . 
?untoothfxi rdfs:label ?procclass . 
?untoothfxi obo:OHD_0000092 obo:OHD_0000168 . 
?untoothfxi obo:IAO_0000136 ?utoothi . 
?untoothfxi obo:OHD_0000015 ?procdate . 
OPTIONAL { 
?exami obo:OHD_0000092 obo:OHD_0000019 . 
?exami obo:OBI_0000299 ?untoothfxi . 
?providerroletype rdfs:subClassOf obo:OHD_0000052 . 
?providerrolei obo:OHD_0000092 ?providerroletype . 
?exami obo:BFO_0000055 ?providerrolei . 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?providerrolei obo:BFO_0000052 ?provideri . 
?provideri rdfs:label ?provider . }.} UNION 
 { 
?cariesfxi rdfs:label ?procclass . 
?cariestoothi obo:OHD_0000092 ?toothtype . 
?cariestoothi obo:BFO_0000050 ?patienti . 
?lesionontoothi obo:OHD_0000092 obo:OHD_0000021 . 
?lesionontoothi obo:BFO_0000050 ?cariestoothi . 
?cariesfxi obo:OHD_0000092 obo:OHD_0000024 . 
?cariesfxi obo:IAO_0000136 ?lesionontoothi . 
?cariesfxi obo:OHD_0000015 ?procdate . 
OPTIONAL { 
?exami obo:OHD_0000092 obo:OHD_0000019 . 
?exami obo:OBI_0000299 ?cariesfxi . 
?providerroletype rdfs:subClassOf obo:OHD_0000052 . 
?providerrolei obo:OHD_0000092 ?providerroletype . 
?exami obo:BFO_0000055 ?providerrolei . 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?providerrolei obo:BFO_0000052 ?provideri . 
?provideri rdfs:label ?provider . }.
OPTIONAL { 
?surfacemi obo:OHD_0000092 obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth . 
?surfacemi obo:BFO_0000050 ?cariestoothi . 
?surfacemi rdfs:label ?surfacem . 
?lesionontoothi obo:BFO_0000050 ?surfacemi . 
?lesionontoothi rdfs:label ?dxm . }.
OPTIONAL { 
?surfaceoi obo:OHD_0000092 obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth . 
?surfaceoi obo:BFO_0000050 ?cariestoothi . 
?surfaceoi rdfs:label ?surfaceo . 
?lesionontoothi obo:BFO_0000050 ?surfaceoi . 
?lesionontoothi rdfs:label ?dxo . }.
OPTIONAL { 
?surfacedi obo:OHD_0000092 obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth . 
?surfacedi obo:BFO_0000050 ?cariestoothi . 
?surfacedi rdfs:label ?surfaced . 
?lesionontoothi obo:BFO_0000050 ?surfacedi . 
?lesionontoothi rdfs:label ?dxd . }.
OPTIONAL { 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth . } UNION 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth . }.
?surfacefi obo:BFO_0000050 ?cariestoothi . 
?surfacefi rdfs:label ?surfacef . 
?lesionontoothi obo:BFO_0000050 ?surfacefi . 
?lesionontoothi rdfs:label ?dxf . }.
OPTIONAL { 
?surfaceli obo:OHD_0000092 obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth . 
?surfaceli obo:BFO_0000050 ?cariestoothi . 
?surfaceli rdfs:label ?surfacel . 
?lesionontoothi obo:BFO_0000050 ?surfaceli . 
?lesionontoothi rdfs:label ?dxl . }.} UNION 
 { 
?mstoothfxi rdfs:label ?procclass . 
?mstoothfxtype rdfs:subClassOf obo:OHD_0000026 . 
?mstoothfxi obo:OHD_0000092 ?mstoothfxtype . 
?mstoothfxi obo:OHD_0000015 ?procdate . 
?dentitioni obo:OHD_0000092 obo:FMA_75152 . 
?dentitioni obo:BFO_0000050 ?patienti . 
?mstoothfxi obo:IAO_0000136 ?dentitioni . 
?utoothnumi rdf:type obo:OHD_0000100 . 
?utoothnumi obo:IAO_0000136 ?toothtype . 
?mstoothfxi obo:BFO_0000051 ?utoothnumi . 
OPTIONAL { 
?exami obo:OHD_0000092 obo:OHD_0000019 . 
?exami obo:OBI_0000299 ?mstoothfxi . 
?providerroletype rdfs:subClassOf obo:OHD_0000052 . 
?providerrolei obo:OHD_0000092 ?providerroletype . 
?exami obo:BFO_0000055 ?providerrolei . 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?providerrolei obo:BFO_0000052 ?provideri . 
?provideri rdfs:label ?provider . }.} "

## string for materials query:
._materials_query <-
"PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?patientid ?sex ?birthdate ?tthnum ?procdate ?procclass ?proccode ?matm ?mato ?matd ?matf ?matl ?dxm ?dxo ?dxd ?dxf ?dxl ?provider
WHERE { 
?patienttype rdfs:subClassOf obo:OHD_0000012 . 
?patienti obo:OHD_0000092 ?patienttype . 
?patienti rdfs:label ?patientid . 
?patienttype rdfs:label ?sex . 
?patienti obo:OHD_0000050 ?birthdate . 
?toothtype rdfs:subClassOf obo:FMA_12516 . 
?toothtype obo:OHD_0000065 ?tthnum . 
 { 
?toothi obo:OHD_0000092 ?toothtype . 
?toothi obo:BFO_0000050 ?patienti . 
?proceduretype rdfs:subClassOf obo:OHD_0000002 . 
?procedurei obo:OHD_0000092 ?proceduretype . 
?procedurei obo:BFO_0000057 ?patienti . 
?procedurei obo:BFO_0000057 ?toothi . 
?proceduretype rdfs:label ?procclass . 
?procedurei obo:OHD_0000015 ?procdate . 
 { 
?toothroletype rdfs:subClassOf obo:OHD_0000007 . } UNION 
 { 
?toothroletype rdfs:subClassOf obo:OHD_0000058 . } UNION 
 { 
?toothroletype rdfs:subClassOf obo:OHD_0000056 . }.
?toothrolei obo:OHD_0000092 ?toothroletype . 
?toothrolei obo:BFO_0000052 ?toothi . 
?procedurei obo:BFO_0000055 ?toothrolei . 
OPTIONAL { 
?codetype rdfs:subClassOf obo:CDT_1000001 . 
?codei obo:OHD_0000092 ?codetype . 
?codei obo:IAO_0000136 ?procedurei . 
?codetype rdfs:label ?proccode . }.
OPTIONAL { 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?procedurei obo:BFO_0000057 ?provideri . 
?provideri rdfs:label ?provider . }.
OPTIONAL { 
?surfacemi obo:OHD_0000092 obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth . 
?surfacemi obo:BFO_0000050 ?toothi . 
?surfacemi rdfs:label ?surfacem . 
?materialtypem rdfs:subClassOf obo:OHD_0000000 . 
?matmi obo:OHD_0000092 ?materialtypem . 
?matmi obo:BFO_0000082 ?toothi . 
?matmi obo:OHD_0000091 ?surfacemi . 
?procedurei obo:BFO_0000057 ?surfacemi . 
?procedurei obo:BFO_0000057 ?matmi . 
?matmi rdfs:label ?matm . }.
OPTIONAL { 
?surfaceoi obo:OHD_0000092 obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth . 
?surfaceoi obo:BFO_0000050 ?toothi . 
?surfaceoi rdfs:label ?surfaceo . 
?materialtypeo rdfs:subClassOf obo:OHD_0000000 . 
?matoi obo:OHD_0000092 ?materialtypeo . 
?matoi obo:BFO_0000082 ?toothi . 
?matoi obo:OHD_0000091 ?surfaceoi . 
?procedurei obo:BFO_0000057 ?surfaceoi . 
?procedurei obo:BFO_0000057 ?matoi . 
?matoi rdfs:label ?mato . }.
OPTIONAL { 
?surfacedi obo:OHD_0000092 obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth . 
?surfacedi obo:BFO_0000050 ?toothi . 
?surfacedi rdfs:label ?surfaced . 
?materialtyped rdfs:subClassOf obo:OHD_0000000 . 
?matdi obo:OHD_0000092 ?materialtyped . 
?matdi obo:BFO_0000082 ?toothi . 
?matdi obo:OHD_0000091 ?surfacedi . 
?procedurei obo:BFO_0000057 ?surfacedi . 
?procedurei obo:BFO_0000057 ?matdi . 
?matdi rdfs:label ?matd . }.
OPTIONAL { 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth . } UNION 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth . }.
?surfacefi obo:BFO_0000050 ?toothi . 
?surfacefi rdfs:label ?surfacef . 
?materialtypef rdfs:subClassOf obo:OHD_0000000 . 
?matfi obo:OHD_0000092 ?materialtypef . 
?matfi obo:BFO_0000082 ?toothi . 
?matfi obo:OHD_0000091 ?surfacefi . 
?procedurei obo:BFO_0000057 ?surfacefi . 
?procedurei obo:BFO_0000057 ?matfi . 
?matfi rdfs:label ?matf . }.
OPTIONAL { 
?surfaceli obo:OHD_0000092 obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth . 
?surfaceli obo:BFO_0000050 ?toothi . 
?surfaceli rdfs:label ?surfacel . 
?materialtypel rdfs:subClassOf obo:OHD_0000000 . 
?matli obo:OHD_0000092 ?materialtypel . 
?matli obo:BFO_0000082 ?toothi . 
?matli obo:OHD_0000091 ?surfaceli . 
?procedurei obo:BFO_0000057 ?surfaceli . 
?procedurei obo:BFO_0000057 ?matli . 
?matli rdfs:label ?matl . }.}.} LIMIT 30 "

## string for unerupted query
._unerupted_query <-
"PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?patientid ?sex ?birthdate ?tthnum ?procdate ?procclass ?proccode ?matm ?mato ?matd ?matf ?matl ?dxm ?dxo ?dxd ?dxf ?dxl ?provider
WHERE { 
?patienttype rdfs:subClassOf obo:OHD_0000012 . 
?patienti obo:OHD_0000092 ?patienttype . 
?patienti rdfs:label ?patientid . 
?patienttype rdfs:label ?sex . 
?patienti obo:OHD_0000050 ?birthdate . 
?toothtype rdfs:subClassOf obo:FMA_12516 . 
?toothtype obo:OHD_0000065 ?tthnum . 
 { 
?utoothi obo:OHD_0000092 ?toothtype . 
?utoothi obo:BFO_0000050 ?patienti . 
?untoothfxi rdfs:label ?procclass . 
?untoothfxi obo:OHD_0000092 obo:OHD_0000168 . 
?untoothfxi obo:IAO_0000136 ?utoothi . 
?untoothfxi obo:OHD_0000015 ?procdate . 
OPTIONAL { 
?exami obo:OHD_0000092 obo:OHD_0000019 . 
?exami obo:OBI_0000299 ?untoothfxi . 
?providerroletype rdfs:subClassOf obo:OHD_0000052 . 
?providerrolei obo:OHD_0000092 ?providerroletype . 
?exami obo:BFO_0000055 ?providerrolei . 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?providerrolei obo:BFO_0000052 ?provideri . 
?provideri rdfs:label ?provider . }.}.} LIMIT 30 "

## string for caries query
._caries_query <-
"PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?patientid ?sex ?birthdate ?tthnum ?procdate ?procclass ?proccode ?matm ?mato ?matd ?matf ?matl ?dxm ?dxo ?dxd ?dxf ?dxl ?provider
WHERE { 
?patienttype rdfs:subClassOf obo:OHD_0000012 . 
?patienti obo:OHD_0000092 ?patienttype . 
?patienti rdfs:label ?patientid . 
?patienttype rdfs:label ?sex . 
?patienti obo:OHD_0000050 ?birthdate . 
?toothtype rdfs:subClassOf obo:FMA_12516 . 
?toothtype obo:OHD_0000065 ?tthnum . 
 { 
?cariesfxi rdfs:label ?procclass . 
?cariestoothi obo:OHD_0000092 ?toothtype . 
?cariestoothi obo:BFO_0000050 ?patienti . 
?lesionontoothi obo:OHD_0000092 obo:OHD_0000021 . 
?lesionontoothi obo:BFO_0000050 ?cariestoothi . 
?cariesfxi obo:OHD_0000092 obo:OHD_0000024 . 
?cariesfxi obo:IAO_0000136 ?lesionontoothi . 
?cariesfxi obo:OHD_0000015 ?procdate . 
OPTIONAL { 
?exami obo:OHD_0000092 obo:OHD_0000019 . 
?exami obo:OBI_0000299 ?cariesfxi . 
?providerroletype rdfs:subClassOf obo:OHD_0000052 . 
?providerrolei obo:OHD_0000092 ?providerroletype . 
?exami obo:BFO_0000055 ?providerrolei . 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?providerrolei obo:BFO_0000052 ?provideri . 
?provideri rdfs:label ?provider . }.
OPTIONAL { 
?surfacemi obo:OHD_0000092 obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth . 
?surfacemi obo:BFO_0000050 ?cariestoothi . 
?surfacemi rdfs:label ?surfacem . 
?lesionontoothi obo:BFO_0000050 ?surfacemi . 
?lesionontoothi rdfs:label ?dxm . }.
OPTIONAL { 
?surfaceoi obo:OHD_0000092 obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth . 
?surfaceoi obo:BFO_0000050 ?cariestoothi . 
?surfaceoi rdfs:label ?surfaceo . 
?lesionontoothi obo:BFO_0000050 ?surfaceoi . 
?lesionontoothi rdfs:label ?dxo . }.
OPTIONAL { 
?surfacedi obo:OHD_0000092 obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth . 
?surfacedi obo:BFO_0000050 ?cariestoothi . 
?surfacedi rdfs:label ?surfaced . 
?lesionontoothi obo:BFO_0000050 ?surfacedi . 
?lesionontoothi rdfs:label ?dxd . }.
OPTIONAL { 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth . } UNION 
 { 
?surfacefi obo:OHD_0000092 obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth . }.
?surfacefi obo:BFO_0000050 ?cariestoothi . 
?surfacefi rdfs:label ?surfacef . 
?lesionontoothi obo:BFO_0000050 ?surfacefi . 
?lesionontoothi rdfs:label ?dxf . }.
OPTIONAL { 
?surfaceli obo:OHD_0000092 obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth . 
?surfaceli obo:BFO_0000050 ?cariestoothi . 
?surfaceli rdfs:label ?surfacel . 
?lesionontoothi obo:BFO_0000050 ?surfaceli . 
?lesionontoothi rdfs:label ?dxl . }.}.} LIMIT 30 "

## query for missing teeth:
._missing_query <- 
"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?patientid ?sex ?birthdate ?tthnum ?procdate ?procclass ?proccode ?matm ?mato ?matd ?matf ?matl ?dxm ?dxo ?dxd ?dxf ?dxl ?provider
WHERE { 
?patienttype rdfs:subClassOf obo:OHD_0000012 . 
?patienti obo:OHD_0000092 ?patienttype . 
?patienti rdfs:label ?patientid . 
?patienttype rdfs:label ?sex . 
?patienti obo:OHD_0000050 ?birthdate . 
?toothtype rdfs:subClassOf obo:FMA_12516 . 
?toothtype obo:OHD_0000065 ?tthnum . 
 { 
?mstoothfxi rdfs:label ?procclass . 
?mstoothfxtype rdfs:subClassOf obo:OHD_0000026 . 
?mstoothfxi obo:OHD_0000092 ?mstoothfxtype . 
?mstoothfxi obo:OHD_0000015 ?procdate . 
?dentitioni obo:OHD_0000092 obo:FMA_75152 . 
?dentitioni obo:BFO_0000050 ?patienti . 
?mstoothfxi obo:IAO_0000136 ?dentitioni . 
?utoothnumi rdf:type obo:OHD_0000100 . 
?utoothnumi obo:IAO_0000136 ?toothtype . 
?mstoothfxi obo:BFO_0000051 ?utoothnumi . 
OPTIONAL { 
?exami obo:OHD_0000092 obo:OHD_0000019 . 
?exami obo:OBI_0000299 ?mstoothfxi . 
?providerroletype rdfs:subClassOf obo:OHD_0000052 . 
?providerrolei obo:OHD_0000092 ?providerroletype . 
?exami obo:BFO_0000055 ?providerrolei . 
?providertype rdfs:subClassOf obo:OHD_0000051 . 
?provideri obo:OHD_0000092 ?providertype . 
?providerrolei obo:BFO_0000052 ?provideri . 
?provideri rdfs:label ?provider . }.}.} LIMIT 30 "

