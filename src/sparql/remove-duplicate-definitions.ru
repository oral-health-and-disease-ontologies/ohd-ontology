PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

# When an entity has more than one definition (IAO_0000115) and at least one of them
# has a definition source (IAO_0000119) annotated on the definition axiom, remove the
# definitions that do not have a definition source (along with any of their axiom annotations).

DELETE {
  ?s obo:IAO_0000115 ?def .
  ?ax ?axp ?axv .
}
WHERE {
  ?s obo:IAO_0000115 ?def .

  ?keepAx owl:annotatedSource ?s ;
          owl:annotatedProperty obo:IAO_0000115 ;
          owl:annotatedTarget ?keepDef ;
          obo:IAO_0000119 ?source .
  FILTER(?keepDef != ?def)

  FILTER NOT EXISTS {
    ?srcAx owl:annotatedSource ?s ;
           owl:annotatedProperty obo:IAO_0000115 ;
           owl:annotatedTarget ?def ;
           obo:IAO_0000119 ?anySource .
  }

  OPTIONAL {
    ?ax owl:annotatedSource ?s ;
        owl:annotatedProperty obo:IAO_0000115 ;
        owl:annotatedTarget ?def ;
        ?axp ?axv .
  }
}
