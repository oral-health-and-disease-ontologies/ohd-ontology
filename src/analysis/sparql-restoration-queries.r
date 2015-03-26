restoration.count.by.tooth.query.string <- function (limit=0) {
  query.string <- "
select distinct  ?toothtype (count(?toothtype) as ?count)
where
{
  ## patient's sex and birth date
  ?patienti rdf:type patient: .
  ?patienti asserted_type: ?patienttypei .
  ?patienti birth_date: ?birthdate .
  
  ## patient's tooth & tooth type
  ?toothi rdf:type tooth: .
  ?toothi asserted_type: ?toothtypei .
  ?toothi is_part_of: ?patienti .

  ## restoration procedure and tooth be restored role
  ?proci rdf:type tooth_restoration_procedure: .
  ?rolei rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei inheres_in: ?toothi .
  ?proci realizes: ?rolei .
  
  ## assign labels
  ?toothi rdfs:label ?tooth .
  ?toothtypei tooth_number: ?toothtype .
} 
group by ?toothtype
"
  
  if (limit > 0)
    query.string <- paste(query.string,"limit ", limit, "\n", sep="")
  
  ## return query string
  query.string
}

total.number.of.tooth.restorations.by.type.query.string <- function (limit=0) {
  query.string = "
select ?restoration_procedure (count(*) as ?count)
where 
{
  ## patient and patient's tooth
  ?patienti rdf:type patient: .  
  ?toothi rdf:type tooth: .
  ?toothi is_part_of: ?patienti .
  
  ## surfaces that are part of tooth
  ?surfacei rdf:type tooth_surface: .
  ?surfacei is_part_of: ?toothi .
  
  ## restoration procedure and tooth be restored role
  ?proct rdfs:subClassOf tooth_restoration_procedure: .
  ?proci rdf:type ?proct .
  ?rolei rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei inheres_in: ?toothi .
  ?proci realizes: ?rolei .
  
  ## the tooth's surface participated in the procedure
  ?surfacei participates_in: ?proci .
  
  ## assign labels
  ?proct rdfs:label ?restoration_procedure .
  
}
group by ?restoration_procedure
  "
  if (limit > 0)
    query.string <- paste(query.string,"limit ", limit, "\n", sep="")
  
  ## return query string
  query.string
}

total.number.of.tooth.restorations.per.suface.query.string <- function(limit = 0) {
  query.string <- "
select ?restoration_procedure ?surface (count(*) as ?count)
where 
{
 ## patient and patient's tooth
 ?patienti rdf:type patient: .  
 ?tootht rdfs:subClassOf tooth: .
 ?toothi asserted_type: ?tootht .
 ?toothi is_part_of: ?patienti .

 ## surfaces that are part of tooth
 ?surfacet rdfs:subClassOf tooth_surface: .
 ?surfacei asserted_type: ?surfacet .
 ?surfacei is_part_of: ?toothi .

  ## restoration procedure and tooth be restored role
  ?proct rdfs:subClassOf tooth_restoration_procedure: .
  ?proci asserted_type: ?proct .
  ?rolei rdf:type tooth_to_be_restored_role: .

  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei inheres_in: ?toothi .
  ?proci realizes: ?rolei .

  ## the tooth's surface participated in the procedure
 ?surfacei participates_in: ?proci .

 ## assign labels
 ?proct rdfs:label ?restoration_procedure .
 ?surfacet rdfs:label ?surface .

}
group by ?restoration_procedure ?surface
order by ?restoration_procedure"

if (limit > 0)
  query.string <- paste(query.string,"limit ", limit, "\n", sep="")

## return query string
query.string
}
  
first.and.second.restoration.query.string <- function (limit=0)
{
  query.string <- "
  select distinct ?patienttype ?birthdate ?tooth ?toothtype ?surface ?surfacetype ?procedure1 ?date1 ?procedure2 ?date2
  
  ## look for surface specific for now. Find two procedures and an optional third
  where
{
  ## patient's sex and birth date
  ?patienti rdf:type patient: .
  ?patienti asserted_type: ?patienttypei .
  ?patienti birth_date: ?birthdate .
  
  ## patient's tooth & tooth type
  ?toothi rdf:type tooth: .
  ?toothi asserted_type: ?toothtypei .
  ?toothi is_part_of: ?patienti .
  
  ## surfaces and their types that are part of tooth
  ?surfacei rdf:type tooth_surface: .
  ?surfacei asserted_type: ?surfacetypei .
  ?surfacei is_part_of: ?toothi .
  
  ##- get restoration procedure in general
  ## this is done by finding the procedures that realize
  ## some tooth to be resotred role that is borne by the tooth
  ## the procedures occurrence date (?date) is used to determine
  ## the first restoration date by taking the min of ?date
  ## see having clause below
  ?proci rdf:type tooth_restoration_procedure: .
  ?rolei rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei inheres_in: ?toothi .
  ?proci realizes: ?rolei .
  ?proci occurrence_date: ?date . # date of procedure
  ?proci has_participant: ?surfacei . ## link surface to procedure

  ##- first procedure: the tooth is same as the general procedure above.
  ## the first procedure is determined in a manner similar to the
  ## general procedure above
  ?proci1 rdf:type tooth_restoration_procedure: .
  ?rolei1 rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei1 inheres_in: ?toothi .
  ?proci1 realizes: ?rolei1 .
  ?proci1 occurrence_date: ?date1 . # date of procedure 1
  ?proci1 has_participant: ?surfacei . ## link surface to procedure

  ##- second process: the tooth and surface remain the same as the first
  ## but a new process that realizes a new role is searched for
  ?proci2 rdf:type tooth_restoration_procedure: .
  ?rolei2 rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei2 inheres_in: ?toothi .
  ?proci2 realizes: ?rolei2 .
  ?proci2 occurrence_date: ?date2 . # date fo procedure 2
  ?proci2 has_participant: ?surfacei . ## link surface to procedure

  ## we only those second procedure that are after the first
  filter (?date2 > ?date1 && ?proci1 != ?proci2)
  
  ##- third process: the tooth and surface remain the same,
  ## but the date is between the other two
  ## (we check later that this doesn't succeed)
  optional {
  ?proci3 rdf:type tooth_restoration_procedure: .
  ?rolei3 rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?rolei3 inheres_in: ?toothi .
  ?proci3 realizes: ?rolei3 .
  ?proci3 occurrence_date: ?date3 . # date of procedure 3
  ?proci3 has_participant: ?surfacei . ## link surface to procedure

  ## we want only that procedures that are between
  ## two other procedures
  filter (?date3<?date2 && ?date3>?date1)}
  
  ## assign labels
  ?patienttypei rdfs:label ?patienttype .
  ?toothi rdfs:label ?tooth .
  ?toothtypei tooth_number: ?toothtype .
  ?surfacei rdfs:label ?surface .
  ?surfacetypei rdfs:label ?surfacetype .
  ?proci1 rdfs:label ?procedure1 .
  ?proci2 rdfs:label ?procedure2 .
  
  ## we only want those records where the in between date (?date3)
  ## is not bound, this gives us adjacent dates
  filter (!bound(?date3))
}
  group by ?patienttype ?birthdate ?tooth ?toothtype ?surface ?surfacetype ?procedure1 ?date1 ?procedure2 ?date2
  ## match the min and first procedure dates
  having (?date1 = min(?date))
  order by ?tooth ?surface ?date1
  
  "
  if (limit > 0)
     query.string <- paste(query.string,"limit ", limit, "\n", sep="")

query.string
}

restoration.followed.by.inlay.query.string <- function (limit=0)
{
  query.string <- "
  select distinct 
  ?patient
  ?tooth
  ?procedure
  ?procedure_date
  (group_concat(distinct ?surface1; separator=\", \") as ?surfaces1)
  ?inlay
  ?inlay_date
  (group_concat(distinct ?surface2; separator=\", \") as ?surfaces2)
  
  ## look for surface specific for now. Find two procedures and an optional third
  where
{
  ## patient's sex and birth date
  ?patienti rdf:type patient: .
  ?patienti asserted_type: ?patienttypei .
  ?patienti birth_date: ?birthdate .
  
  ## patient's tooth & tooth type
  ?toothi rdf:type tooth: .
  ?toothi asserted_type: ?toothtypei .
  ?toothi is_part_of: ?patienti .
  
  ## surfaces and their types that are part of tooth
  ?surfacei rdf:type tooth_surface: .
  ?surfacei asserted_type: ?surfacet .
  ?surfacei is_part_of: ?toothi .
  
  ## - restoration procedure and role that it realizes
  ?proci rdf:type tooth_restoration_procedure: .
  ?rolei rdf:type tooth_to_be_restored_role: .
  ?proci realizes: ?rolei .
  ?proci occurrence_date: ?procedure_date . # date of procedure
  ?proci has_participant: ?surfacei . ## link surface to procedure
  
  ## - second process: the tooth and surface remain the same as the first
  ## but a new process that realizes a new role is searched for
  ?inlay_proci rdf:type inlay_restoration: .
  ?inlay_rolei rdf:type tooth_to_be_restored_role: .
  ?inlay_rolei inheres_in: ?toothi .
  ?inlay_proci realizes: ?inlay_rolei .
  ?inlay_proci occurrence_date: ?inlay_date . # date for inlay
  
  ## surface that participates in inlay procedure
  ?inlay_surfacei rdf:type tooth_surface: .
  ?inlay_surfacei asserted_type: ?inlay_surfacet .
  ?inlay_surfacei is_part_of: ?toothi .
  ?inlay_proci has_participant: ?inlay_surfacei . # surfaces of inlay
  
  ## we only those second procedure that are after the first
  filter (?inlay_date > ?procedure_date && ?proci != ?inlay_proci) .
  
  ## assign labels
  ?patienti rdfs:label ?patient .
  ?toothi rdfs:label ?tooth .
  ?proci rdfs:label ?procedure .
  ?inlay_proci rdfs:label ?inlay .
  ?surfacet rdfs:label ?surface1 .
  ?inlay_surfacet rdfs:label ?surface2 .
  
  ## check for procedures that might have occurred
  ## between the first and second procedures
  optional 
{
  ?between_procedure rdf:type tooth_restoration_procedure: .
  ?between_rolei rdf:type tooth_to_be_restored_role: .
  
  ## the tooth to be restored role inheres in the tooth
  ## and is realized by the procedure
  ?between_rolei inheres_in: ?toothi .
  ?between_procedure realizes: ?between_rolei .
  ?between_procedure occurrence_date: ?between_date . # date of procedure 3
  #?between_procedure has_participant: ?surfacei . ## link surface to procedure
  
  ## we want only that procedures that are between
  ## two other procedures
  filter (?between_date > ?procedure_date && ?between_date < ?inlay_date)
}
  
  ## we only want those records where the in between date 
  ## is not bound, this gives us adjacent dates
  filter (!bound(?between_date))
  
}
  
  group by
  ?patient
  ?tooth 
  ?procedure 
  ?procedure_date
  ?inlay 
  ?inlay_date
  
  ## match the min and first procedure dates
  #having (?date1 = min(?date))
  order by
  ?patient
  ?tooth
  ?procedure 
  ?procedure_date
  
  "
  if (limit > 0)
     query.string <- paste(query.string,"limit ", limit, "\n", sep="")

query.string
}


non.failed.restorations.query.string <- function(limit=0) {
  query.string <- ""
  
  if (limit > 0)
    query.string <- paste(query.string," limit ", limit, "\n", sep="")
  
  query.string
}