PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

DELETE WHERE { ?s rdfs:isDefinedBy <http://purl.obolibrary.org/obo/ohd.owl> };
DELETE WHERE { ?s rdfs:isDefinedBy "http://purl.obolibrary.org/obo/ohd.owl"^^<http://www.w3.org/2001/XMLSchema#string> }