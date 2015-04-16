## Author: Alan Ruttenberg
## Project: OHD, R21
## Date: 2015-04-16 

coded_procedure_extract <- function(limit=1000,file="~/report.csv") 
  {
    result <-
      queryc("select ?patient ?date ?code ?tooth",
             " where",
             "  { ?proc a dental_procedure:.",

             ## in each procedure get the code, and use the
             ## code_identifier: property to get short code label
             "    ?code_instance is_about: ?proc. ",
             "    ?code_instance a ?code_type.",
             "    ?code_instance a cdt_code:.", # restrict to CDT codes
             "    ?code_instance sesame:directType ?code_type.", # get the most specific type 
             "    ?code_type code_identifier: ?code.",


             ## get the tooth. Use tooth_number: annotation to get
             ## numbered tooth name
             "    ?proc has_participant: ?tooth_instance.",
             "    ?tooth_instance a ?toothc.",
             "    ?tooth_instance a tooth:.",
             "    ?toothc tooth_number: ?tooth.",

             ## get the patient. The patient is the participant that
             ## realizes the patient role
             "    ?proc has_participant: ?patient_instance .",
             "    ?proc realizes: ?role.",
             "    ?role a patient_role:.",
             "    ?role inheres_in: ?patient_instance.",
             "    ?patient_instance rdfs:label ?patient.",

             ## finally get the date
             "    ?proc occurrence_date: ?date.",
             "  }",
             "order by ?patient",
             "limit ", limit
             );
    write.table(result,file=file,sep=",")
}

## Hi Alan,

 
## As discussed on Friday, I would like to get a fairly limited subset
## of our data to play around with with one of our local visualization
## experts, Katy Boerner. Below is what I have written to her - it is
## fairly general since this is the first foray into applying her
## tools for clinical research problems.
 
## The data I thought would make sense for a limited set of data include:
## - patient ID (1, 2, 3, ...)
## - visit date
## - tooth
## - restoration procedure (we could leave it as amalgam or composite)
 
## This would make the data set quite sparse, but I think would be a
## reasonable start. I think something like all data for 200 patients,
## selected at random, might be good.
 
## Let me know what you think. I'm scheduled to talk with Katy on
## Wednesday. If we could have the data set by then, that would be
## great.
 
## Also, if possible, I would like to have the graphs/numbers for
## discussion with Claudio by today. I need to bring printouts (which
## I can most efficiently make it home) since were going to dinner
## tomorrow.
 
## Thanks, Titus

## Clarification
## << Ok, to clarify: You want tooth but not surface. >>
 
## Correct. Surface is too granular at this time.
## …
 
## << For restoration procedure column you want the D code for any of
## the amalgam or composite restorations (1,2,3,4+ surfaces)? >>
 
## Yes, if I could have the ADA code (2140, 2150, etc.) then I can
## collapse these data as needed.

## Alan
