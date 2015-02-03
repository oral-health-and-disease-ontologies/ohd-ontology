
## Author: Bill Duncan
## Project: OHD
## Date: Dec. 1, 2015
##
## Summary: Statics on the time intervals between restoration proceedures

source("SPARQL.R") ; # patch to SPARQL.R
.GlobalEnv[["interpret_type"]]=interpret_rdf_type;

source("environment.r") # load environment variables

average.time.to.restoration.failure <- function ()  {
    #query.string <- all.tooth.restorations.query.string()
    #query.string <- multiple.restorations.on.same.tooth.query.string()
    #query.string <- test.query.string()
    query.string <- test.query.string2()

  #query.string <- "select ?s ?p ?o where { ?s ?p ?o } limit 10"
  #cat(query.string)
  cat(querystring(query.string))
  #res <- queryc(query.string, current_endpoint)
  res <- queryc(query.string)
  res

 }

all.tooth.restorations.query.string <- function () {
"
select ?tooth ?procedure ?surface ?date
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

    ## restored sufaces(if one exists) are determined by
    ## the surface that participates in the restoration procedure
    optional {
        ?surfacei rdf:type tooth_surface: .
        ?surfacei participates_in: ?proci .
        ?surfacei rdfs:label ?surface .
    }
}
order by ?tooth ?date ?procedure ?surface
limit 10
"
}

multiple.restorations.on.same.tooth.query.string <- function () {
"
select ?tooth ?procedure ?surface ?date (count(?procedure) as ?proc_count)
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

    ## restored sufaces(if one exists) are determined by
    ## the surface that participates in the restoration procedure
    optional {
        ?surfacei rdf:type tooth_surface: .
        ?surfacei is_part_of: ?toothi .
        ?proci has_participant: ?surfacei .
        ?surfacei rdfs:label ?surface .
    }

    #{
    #    select ?date (min(?date) as ?min)
    #    where { ?proci occurrence_date: ?date . }
    #    group by ?date
    #}

    #filter(?min = ?date)

    #{
    #    select ?proci (count(?proci) as ?proc_count_on_tooth)
    #    where { ?proci has_participant: ?toothi . }
    #    group by ?proci
    #}

    #filter (?proc_count_on_tooth > 1)
    filter (?tooth = \"tooth 10 of patient 1091\")
}
group by ?tooth ?date ?procedure ?surface ?date
having (?proc_count > 1)
order by ?tooth ?date ?procedure ?surface ?date
limit 50
"
}

test.query.string <- function () {
"
select distinct ?tooth ?procedure ?surface ?date ?tooth_procedure_count
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

    ## restored sufaces(if one exists) are determined by
    ## the surface that participates in the restoration procedure
    optional {
        ?surfacei rdf:type tooth_surface: .
        ?surfacei is_part_of: ?toothi .
        ?proci has_participant: ?surfacei .
        ?surfacei rdfs:label ?surface .
    }

    {
        select ?toothi (count(?toothi) as ?tooth_procedure_count)
        where { ?proci has_participant: ?toothi . }
        group by ?toothi
    }

    #filter (?tooth_procedure_count > 1)

    filter (?tooth = \"tooth 10 of patient 1091\")
}
#group by ?tooth ?date ?procedure ?surface ?date
#having (?proc_count > 1)
order by ?tooth ?date ?procedure ?surface ?date
limit 50
"
}

test.query.string2 <- function () {
"
select distinct ?tooth ?procedure ?surface ?date (min(?date) as ?min_date)
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

    ## restored sufaces(if one exists) are determined by
    ## the surface that participates in the restoration procedure
    optional {
        ?surfacei rdf:type tooth_surface: .
        ?surfacei is_part_of: ?toothi .
        ?proci has_participant: ?surfacei .
        ?surfacei rdfs:label ?surface .
    }


    {
        select ?toothi (count(?toothi) as ?tooth_procedure_count)
        where
        {
            ## tooth to be restored role that inheres in tooth
            ?rolei2 rdf:type tooth_to_be_restored_role: .
            ?rolei2 inheres_in: ?toothi .

            ## restoration procedure that realizes role
            ?proci2 rdf:type tooth_restoration_procedure: .
            ?proci2 realizes: ?rolei2 .
            ?proci occurrence_date: ?date2 .

            filter(?date2 > ?min_date) .
        }
        group by ?toothi
    }


    filter (?tooth = \"tooth 10 of patient 1091\")
}
group by ?tooth ?date ?procedure ?surface ?date
#having (?proc_count > 1)
order by ?tooth ?date ?procedure ?surface ?date
limit 50
"
}
