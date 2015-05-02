## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-24
##
## prefixes are represented as an environment with the key the prefix
## (terminated with ":") and the value the URI. 

prefixes <- new.env(hash=TRUE)
assign("rdf:", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>",prefixes)
assign("rdfs:", "<http://www.w3.org/2000/01/rdf-schema#>",envir=prefixes)
assign("owl:", "<http://www.w3.org/2002/07/owl#>",envir=prefixes)
assign("xsd:", "<http://www.w3.org/2001/XMLSchema#>",envir=prefixes)
assign("obo:", "<http://purl.obolibrary.org/obo/>",envir=prefixes)
assign("dental_patient:", "<http://purl.obolibrary.org/obo/OHD_0000012>",envir=prefixes)
assign("birth_date:", " <http://purl.obolibrary.org/obo/OHD_0000050>",envir=prefixes)
assign("occurrence_date:", " <http://purl.obolibrary.org/obo/OHD_0000015>",envir=prefixes)
assign("inheres_in:", "<http://purl.obolibrary.org/obo/BFO_0000052>",envir=prefixes)
assign("participates_in:", "<http://purl.obolibrary.org/obo/BFO_0000056>",envir=prefixes)
assign("has_participant:", "<http://purl.obolibrary.org/obo/BFO_0000057>",envir=prefixes)
assign("dental_procedure:", "<http://purl.obolibrary.org/obo/OHD_0000002>",envir=prefixes)
assign("crown_restoration:", "<http://purl.obolibrary.org/obo/OHD_0000033>",envir=prefixes)
assign("tooth_restoration_procedure:", "<http://purl.obolibrary.org/obo/OHD_0000004>",envir=prefixes)
assign("intracoronal_restoration:", "<http://purl.obolibrary.org/obo/OHD_0000006>",envir=prefixes)
assign("veneer_restoration:", "<http://purl.obolibrary.org/obo/OHD_0000027>",envir=prefixes)
assign("inlay_restoration:", "<http://purl.obolibrary.org/obo/OHD_0000133>",envir=prefixes)
assign("onlay_restoration:", "<http://purl.obolibrary.org/obo/OHD_0000134>",envir=prefixes)
assign("surgical_procedure:", "<http://purl.obolibrary.org/obo/OHD_0000044>",envir=prefixes)
assign("endodontic_procedure:", "<http://purl.obolibrary.org/obo/OHD_0000003>",envir=prefixes)
assign("tooth_to_be_restored_role:", "<http://purl.obolibrary.org/obo/OHD_0000007>",envir=prefixes)
assign("dental_patient_role:", "<http://purl.obolibrary.org/obo/OHD_0000190>",envir=prefixes)
assign("patient_role:", "<http://purl.obolibrary.org/obo/OBI_0000093>",envir=prefixes)
assign("dental_healthcare_provider_role:", "<http://purl.obolibrary.org/obo/OHD_0000052>",envir=prefixes)
assign("tooth_to_be_filled_role:", "<http://purl.obolibrary.org/obo/OHD_0000008>",envir=prefixes)
assign("realizes:", "<http://purl.obolibrary.org/obo/BFO_0000055>",envir=prefixes)
assign("tooth:", "<http://purl.obolibrary.org/obo/FMA_12516>",envir=prefixes)
assign("is_part_of:", "<http://purl.obolibrary.org/obo/BFO_0000050>",envir=prefixes)
assign("tooth_surface:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Surface_enamel_of_tooth>",envir=prefixes)
assign("mesial:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Mesial_surface_enamel_of_tooth>",envir=prefixes)
assign("distal:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Distal_surface_enamel_of_tooth>",envir=prefixes)
assign("occlusal:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Occlusial_surface_enamel_of_tooth>",envir=prefixes)
assign("buccal:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Buccal_surface_enamel_of_tooth>",envir=prefixes)
assign("labial:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Labial_surface_enamel_of_tooth>",envir=prefixes)
assign("lingual:", "<http://purl.obolibrary.org/obo/FMA_no_fmaid_Lingual_surface_enamel_of_tooth>",envir=prefixes)
assign("is_dental_restoration_of:", "<http://purl.obolibrary.org/obo/OHD_0000091>",envir=prefixes)
assign("dental_restoration_material:", "<http://purl.obolibrary.org/obo/OHD_0000000>",envir=prefixes)
assign("has_specified_input:", "<http://purl.obolibrary.org/obo/OBI_0000293>",envir=prefixes)
assign("has_specified_output:", "<http://purl.obolibrary.org/obo/OBI_0000299>",envir=prefixes)
assign("asserted_type:", "<http://purl.obolibrary.org/obo/OHD_0000092>",envir=prefixes)
assign("tooth_number:", "<http://purl.obolibrary.org/obo/OHD_0000065>",envir=prefixes)
assign("female:", "<http://purl.obolibrary.org/obo/OHD_0000049>",envir=prefixes)
assign("male:", "<http://purl.obolibrary.org/obo/OHD_0000054>",envir=prefixes)
assign("patient:", "<http://purl.obolibrary.org/obo/OHD_0000012>",envir=prefixes)
assign("dental_visit:", "<http://purl.obolibrary.org/obo/OHD_0000009>", envir=prefixes)
assign("physical_exam:", "<http://purl.obolibrary.org/obo/OGMS_0000057>", envir=prefixes)
assign("health_care_encounter:", "<http://purl.obolibrary.org/obo/OGMS_0000096>", envir=prefixes)
assign("occurrence_date:", "<http://purl.obolibrary.org/obo/OHD_0000015>", envir=prefixes)
assign("outpatient_encounter:", "<http://purl.obolibrary.org/obo/OGMS_0000099>", envir=prefixes)
assign("process:", "<http://purl.obolibrary.org/obo/BFO_0000007>", envir=prefixes)
assign("owlim:", "<http://www.ontotext.com/trree/owlim#>", envir=prefixes)
assign("sr:", "<http://www.openrdf.org/config/repository/sail#>", envir=prefixes)
assign("rep:", "<http://www.openrdf.org/config/repository#>", envir=prefixes)
assign("is_about:", "<http://purl.obolibrary.org/obo/IAO_0000136>", envir=prefixes)
assign("cdt_code:", "<http://purl.obolibrary.org/obo/CDT_1000001>", envir=prefixes)
assign("homo_sapiens:", "<http://purl.obolibrary.org/obo/NCBITaxon_9606>", envir=prefixes)
assign("information_content_entity:", "<http://purl.obolibrary.org/obo/IAO_0000030>", envir=prefixes)
assign("clinical_finding:", "<http://purl.obolibrary.org/obo/OGMS_0000014>", envir=prefixes)
assign("sesame:", "<http://www.openrdf.org/schema/sesame#>", envir=prefixes)
assign("code_identifier:", "<http://purl.org/dc/elements/1.1/identifier>", envir=prefixes)
assign("next_encounter:", "<http://purl.obolibrary.org/obo/OHD_0000216>", envir=prefixes)
assign("later_encounter:", "<http://purl.obolibrary.org/obo/OHD_0000217>", envir=prefixes)
assign("oral_evaluation:", "<http://purl.obolibrary.org/obo/OHD_0000197>", envir=prefixes)
assign("missing_tooth_finding:", "<http://purl.obolibrary.org/obo/OHD_0000026>", envir=prefixes)
assign("dental_exam:", "<http://purl.obolibrary.org/obo/OHD_0000019>", envir=prefixes)
assign("cdt_code:", "<http://purl.obolibrary.org/obo/CDT_1000001>", envir=prefixes)
assign("molar:","<http://purl.obolibrary.org/obo/FMA_55638>",envir=prefixes)
assign("pre_molar:","<http://purl.obolibrary.org/obo/FMA_55637>",envir=prefixes)
assign("incisor:","<http://purl.obolibrary.org/obo/FMA_55636>",envir=prefixes)
assign("canine:","<http://purl.obolibrary.org/obo/FMA_12823>",envir=prefixes)
assign("tooth_extraction:", "<http://purl.obolibrary.org/obo/OHD_0000057>", envir=prefixes)
assign("tooth_to_undergo_endodontic_procedure_role:","<http://purl.obolibrary.org/obo/OHD_0000058>", envir=prefixes)
assign("tooth_to_be_extracted_role:","<http://purl.obolibrary.org/obo/OHD_0000056>",envir=prefixes)
assign("root_canal:","<http://purl.obolibrary.org/obo/OHD_0000003>",envir=prefixes)
assign("inlay_procedure:","<http://purl.obolibrary.org/obo/OHD_0000133>",envir=prefixes)
assign("resin_filling_restoration:","<http://purl.obolibrary.org/obo/OHD_0000042>",envir=prefixes)
assign("onto","<http://www.ontotext.com/>",envir=prefixes)
assign("ohdi:", "<http://purl.obolibrary.org/obo/ohd/individuals/>",envir=prefixes)
assign("target_of_tooth_procedure:", "<http://purl.obolibrary.org/obo/OHD_0000209>", envir=prefixes)
assign("role:", "<http://purl.obolibrary.org/obo/BFO_0000023>", envir=prefixes)

## backwards compatibility - join these all into a single string.
all_prefixes_as_string <-  function ()
{
  paste(lapply(ls(prefixes),
              function(p) {paste("PREFIX ",p," ",get(p,prefixes),sep="")}),
       collapse ="\n")
}

## oh is this ever ugly, because I'm not versed in R data structures. Maybe I will fix it some time.

## given a list of prefixes, concatenate together the appropriate PREFIX statements for the query
some_prefixes_as_string <- function (which,source="")
{
  paste(do.call(paste,append(cbind(lapply(unique(as.list(which)),
              function(p) {
                if(!(exists(p,prefixes))) { if ((p == "http:") | (p == "_:"))  {return("")} else { cat(paste("Didn't find prefix ",p,"used in:\n ", source)); return("") }};
                paste("PREFIX ",p," ",get(p,prefixes),sep="")})),alist(sep="\n"))),"\n")
}

## given a sparql query (without prefixes) creates the necessary
## PREFIX statements based on which prefixes are used in the query.

prefixes_for_sparql <- function(query)
  { some_prefixes_as_string(as.list(cbind(regmatches(query,gregexpr("([a-zA-Z_.0-9]+:)", query,perl=TRUE))[[1]])),source=query) }

## to remove PREFIX gsub("PREFIX[ ]*[^\n]*\n","",querystring(q))
 
