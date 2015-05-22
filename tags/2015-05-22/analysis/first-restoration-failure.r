This gets all intervals between restorations on the same surface. It does the "there can't be a procedure in-between" trick.

Further modification: 
1. Use SPARQL 1.1 function to pick the first one, using group by and min on the date
2. Modify to allow the subsequent events to be other than defined by a surface (tooth level)

We should be able to use the results of this query to develop further
downstream code in R. If anything it will over-estimate the longevity.


PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX dental_patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX birth_date:  <http://purl.obolibrary.org/obo/OHD_0000050>
PREFIX occurrence_date:  <http://purl.obolibrary.org/obo/OHD_0000015>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
PREFIX participates_in: <http://purl.obolibrary.org/obo/BFO_0000056>
PREFIX dental_procedure: <http://purl.obolibrary.org/obo/OHD_0000002>
PREFIX crown_restoration: <http://purl.obolibrary.org/obo/OHD_0000033>
PREFIX tooth_restoration_procedure: <http://purl.obolibrary.org/obo/OHD_0000004>
PREFIX intracoronal_restoration: <http://purl.obolibrary.org/obo/OHD_0000006>
PREFIX veneer_restoration: <http://purl.obolibrary.org/obo/OHD_0000027>
PREFIX inlay_restoration: <http://purl.obolibrary.org/obo/OHD_0000133>
PREFIX onlay_restoration: <http://purl.obolibrary.org/obo/OHD_0000134>
PREFIX surgical_procedure: <http://purl.obolibrary.org/obo/OHD_0000044>
PREFIX endodontic_procedure: <http://purl.obolibrary.org/obo/OHD_0000003>
PREFIX tooth_to_be_restored_role: <http://purl.obolibrary.org/obo/OHD_0000007>
PREFIX realizes: <http://purl.obolibrary.org/obo/BFO_0000055>
PREFIX tooth: <http://purl.obolibrary.org/obo/FMA_12516>
PREFIX is_part_of: <http://purl.obolibrary.org/obo/BFO_0000050>
PREFIX tooth_surface: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Surface_enamel_of_tooth>
PREFIX mesial: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Mesial_surface_enamel_of_tooth>
PREFIX distal: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Distal_surface_enamel_of_tooth>
PREFIX occlusal: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Occlusial_surface_enamel_of_tooth>
PREFIX buccal: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Buccal_surface_enamel_of_tooth>
PREFIX labial: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Labial_surface_enamel_of_tooth>
PREFIX lingual: <http://purl.obolibrary.org/obo/FMA_no_fmaid_Lingual_surface_enamel_of_tooth>
PREFIX is_dental_restoration_of: <http://purl.obolibrary.org/obo/OHD_0000091>
PREFIX dental_restoration_material: <http://purl.obolibrary.org/obo/OHD_0000000>
PREFIX has_specified_input: <http://purl.obolibrary.org/obo/OBI_0000293>
PREFIX has_specified_output: <http://purl.obolibrary.org/obo/OBI_0000299>
PREFIX has_participant: <http://purl.obolibrary.org/obo/BFO_0000057>

select distinct ?tooth  ?surface ?procedure ?date ?procedure2  ?date2 

## look for surface specific for now. Find two procedures and an optional third
where
{
    ## patient's tooth
    ?toothi rdf:type tooth: .
    ?toothi rdfs:label ?tooth .

    ## tooth to be restored role that inheres in tooth
    ?rolei rdf:type tooth_to_be_restored_role: .
    ?rolei inheres_in: ?toothi .

    ## restoration procedure that realizes role
    ?proci rdf:type tooth_restoration_procedure: .
    ?proci realizes: ?rolei .
    ?proci occurrence_date: ?date .
    ?proci rdfs:label ?procedure .

    ## restored sufaces are determined by
    ## the surface that participates in the restoration procedure
        ?surfacei rdf:type tooth_surface: .
        ?surfacei is_part_of: ?toothi .
        ?proci has_participant: ?surfacei .
        ?surfacei rdfs:label ?surface .

#- second process. Tooth and surface remain the same.
    ?rolei2 rdf:type tooth_to_be_restored_role: .
    ?rolei2 inheres_in: ?toothi .

    ## restoration procedure that realizes role
    ?proci2 rdf:type tooth_restoration_procedure: .
    ?proci2 realizes: ?rolei2 .
    ?proci2 occurrence_date: ?date2 .
    FILTER (?date2 > ?date && ?proci != ?proci2)
    ?proci2 rdfs:label ?procedure .
    ?proci2 has_participant: ?surfacei .
    ?proci rdfs:label ?procedure2 .

#- third process. Tooth and surface remain the same, but the date is between the other2 (we check later that this doesn't succeed)
   optional {
    ?rolei3 rdf:type tooth_to_be_restored_role: .
    ?rolei3 inheres_in: ?toothi .

    ## restoration procedure that realizes role
    ?proci3 rdf:type tooth_restoration_procedure: .
    ?proci3 realizes: ?rolei3 .
    ?proci3 occurrence_date: ?date3 .
    ?proci3 rdfs:label ?procedure .
    ?proci3 has_participant: ?surfacei .
    FILTER (?date3<?date2 && ?date3>?date)}

FILTER (!bound(?date3))} order by ?tooth ?surface ?date


 LIMIT 50


# # || ((?date2) > str(?date)) && !(str(?date2) > str(?date3))  && (str(?date3) > str(?date)) && ?proci != ?proci2 && ?proci != ?proci3 && ?proci2 != ?proci3)
