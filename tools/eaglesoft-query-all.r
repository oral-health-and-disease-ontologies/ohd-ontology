source("eaglesoft-caplan-functions.r")

## get triples
res <- get.caplan.data(limit="30")

## fill in missing column names
## res <- fill.missing.caplan.columns(res)

## ## dates in res are in form "YYYY-MM-DD^^http://www.w3.org/2001/XMLSchema#date"
## ## so I need to lop off the "^^http://www.w3.org/2001/XMLSchema#date" part
## res <- trim.caplan.dates(res)

## ## order results by patientid, tooth number, procedure / finding date
## res.ordered <- order.caplan.rows(res)

## ## replace all NA values with "."
## res.ordered <- fill.missing.caplan.values(res.ordered)

## ## flip dates from YYYY-MM-DD to MM-DD-YYYY
## ## NB: do this after ordering rows!
## res.ordered <- flip.caplan.dates(res.ordered)

## ## put columns in order to match spreadsheet
## res.ordered <- order.caplan.columns(res.ordered)

## transform data into Caplan format
res <- transform.caplan.data(res)

## write results to file
write.caplan.spreadsheet(res)
write.caplan.matrix(res)
