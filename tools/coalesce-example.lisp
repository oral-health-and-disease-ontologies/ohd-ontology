from http://answers.semanticweb.com/questions/3427/sparql-return-value-if-variable-not-defined

(sparql-endpoint-query "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2012.12.05/query" "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT (COALESCE(?patientid,\"*\") AS ?patientid_Q) (COALESCE(?sex,\"*\") AS ?sex_Q) (COALESCE(?birthdate,\"*\") AS ?birthdate_Q) (COALESCE(?tthnum,\"*\") AS ?tthnum_Q) (COALESCE(?procdate,\"*\") AS ?procdate_Q) (COALESCE(?procclass,\"*\") AS ?procclass_Q) (COALESCE(?proccode,\"*\") AS ?proccode_Q) (COALESCE(?matm, \"*\") AS ?matm_Q)  (COALESCE(?mato,\"*\") AS ?mato_Q) (COALESCE(?matd,\"*\") AS ?matd_Q) (COALESCE(?matf,\"*\") AS ?matf_Q) (COALESCE(?matl,\"*\") AS ?matl_Q) (COALESCE(?dxm,\"*\") AS ?dxm_Q) (COALESCE(?dxo,\"*\") AS ?dxo_Q) (COALESCE(?dxd,\"*\") AS ?dxd_Q) (COALESCE(?dxf,\"*\") AS ?dxf_Q) (COALESCE(?dxl,\"*\") AS ?dxl_Q) (COALESCE(?provider,\"*\") AS ?provider_Q)
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
?provideri rdfs:label ?provider . }.}.
FILTER (?patientid = \"patient 1001\")} LIMIT 30")

(("patient 1001" "male dental patient" "1970-07-21" "Tooth 1"
  "1999-12-17" "missing tooth 1 finding for patient 1001"
  "*" "*" "*" "*"
  "*" "*" "*" "*"
  "*" "*" "*"
  "dental provider 43")
 ("patient 1001" "male dental patient" "1970-07-21" "Tooth 16"
  "1999-12-17" "missing tooth 16 finding for patient 1001"
  "*" "*" "*" "*"
  "*" "*" "*" "*"
  "*" "*" "*"
  "dental provider 43")
 ("patient 1001" "male dental patient" "1970-07-21" "Tooth 17"
  "1999-12-17" "missing tooth 17 finding for patient 1001"
  "*" "*" "*" "*"
  "*" "*" "*" "*"
  "*" "*" "*"
  "dental provider 43")
 ("patient 1001" "male dental patient" "1970-07-21" "Tooth 32"
  "1999-12-17" "missing tooth 32 finding for patient 1001"
  "*" "*" "*" "*"
  "*" "*" "*" "*"
  "*" "*" "*"
  "dental provider 43"))
