DELETE { ?s ?p ?tagged }
INSERT { ?s ?p ?plain }
WHERE {
  ?s ?p ?tagged .
  FILTER(isLiteral(?tagged) && LANG(?tagged) != "")
  BIND(STR(?tagged) AS ?plain)
}