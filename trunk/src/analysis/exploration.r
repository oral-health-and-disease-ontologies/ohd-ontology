## what are the kinds of health care encounters
what_kinds_of_encounters <- function()
  { queryc("select distinct ?vl",
           " where ",
           " { ?visit a health_care_encounter:.",
           "   ?visit a ?vt. ?vt rdfs:label ?vl",
           " }") }


## how to health care enounters relate to other things
how_do_encounters_relate_to_other_things <- function()
  { queryc("select distinct ?pl ",
           " where",
           " { ?proc a health_care_encounter:.",
           "   ?proc ?p ?visit.",
           "   ?p rdfs:label ?pl",
           "  }") 
  }

## how is mereology used
## should modify this to use most specific class
whats_part_of_what <- function ()
{ queryc("select distinct ?atype ?btype ",
       " where",
       "  { ?a is_part_of: ?b.",
       "    ?a a ?atype.",
       "    ?b a ?btype.",
       "  }");
}
