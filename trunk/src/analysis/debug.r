## Author: Alan Ruttenberg
## Project: OHD
## Date: 2015-04-10
##

## This was to check whether CDT were is_about other than procedures. As it turned out it was. We will change that.

queryw(" select distinct ?procl",
       " where",
       "  { ?code_instance is_about: ?proc. ",
       "    ?code_instance a ?code_type.",
       "    ?code_instance a cdt_code:.",
       "    ?code_instance a ?code_type.",
       "    ?code_type rdfs:label ?code.",
       "    ?proc a ?proc_type.",
       "    ?proc_type rdfs:label ?procl.",
       "    ?code_type rdfs:subClassOf cdt_code:.",
       "  }",trace=T);


Find duplicate endos.

PREFIX endodontic_procedure: <http://purl.obolibrary.org/obo/OHD_0000003>
PREFIX has_participant: <http://purl.obolibrary.org/obo/BFO_0000057>
PREFIX occurrence_date: <http://purl.obolibrary.org/obo/OHD_0000015>
PREFIX tooth: <http://purl.obolibrary.org/obo/FMA_12516>
PREFIX pat: <http://purl.org/hpi/patchr#>
PREFIX patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX is_about: <http://purl.obolibrary.org/obo/IAO_0000136>
select distinct ?proc1 ?proc1l ?proc2 ?proc2l  ?code2l ?toothi  where 
{ ?proc1 a endodontic_procedure:.
  ?proc2 a endodontic_procedure:.
  ?proc1 has_participant: ?toothi.
  ?proc2 has_participant: ?toothi.
    ?toothi a tooth:.
  ?proc1 has_participant: ?patient.
  ?proc2 has_participant: ?patient.
    ?patient a patient:.        
  ?proc1 occurrence_date: ?date.
  ?proc2 occurrence_date: ?date.  
   filter (?proc1 != ?proc2)
    ?proc1 rdfs:label ?proc1l.
    ?proc2 rdfs:label ?proc2l.
    ?toothi rdfs:label ?toothl. 
    ?code2 rdfs:label ?code2l.
    ?code2 is_about: ?proc2.
}

## 2015-04-10 10:53:34 alanr
## results. Note that "continuant" is one of the results, so since
## procedures as processes and processes are disjoint from continuant,
## we see that there are case where the object of is_about isn't only
## a process.  "Distal surface enamel of tooth" is a domain example.

## [1,] "crown restoration"                          
##  [2,] "entity"                                     
##  [3,] "occurrent"                                  
##  [4,] "process"                                    
##  [5,] "planned process"                            
##  [6,] "material processing"                        
##  [7,] "adding a material entity into a target"     
##  [8,] "material combination"                       
##  [9,] "health care process"                        
## [10,] "health care encounter"                      
## [11,] "tooth restoration procedure"                
## [12,] "dental procedure"                           
## [13,] "endodontic procedure"                       
## [14,] "Distal surface enamel of tooth"             
## [15,] "continuant"                                 
## [16,] "independent continuant"                     
## [17,] "material entity"                            
## [18,] "Anatomical entity"                          
## [19,] "Sum of right of bilateral anatomical entity"
## [20,] "Anatomical structure"                       
## [21,] "Acellular anatomical structure"             
## [22,] "Enamel"                                     
## [23,] "Material anatomical entity"                 
## [24,] "Surface enamel of tooth"                    
## [25,] "resin filling restoration"                  
## [26,] "intracoronal tooth restoration"             
## [27,] "direct restoration"                         
## [28,] "Labial surface enamel of tooth"             
## [29,] "Buccal surface enamel of tooth"             
## [30,] "Sum of left of bilateral anatomical entity" 
## [31,] "amalgam filling restoration"                
## [32,] "Occlusial surface enamel of tooth"          
## [33,] "Mesial surface enamel of tooth"             
## [34,] "Lingual surface enamel of tooth"            
## [35,] "Incisal surface enamel of tooth"            
## [36,] "gold filling restoration"                   
## [37,] "indirect restoration"                       
## [38,] "ceramic inlay restoration"                  
## [39,] "inlay restoration"                          
## [40,] "resin inlay restoration"                    
## [41,] "metallic inlay restoration"                 
## [42,] "metallic onlay restoration"                 
## [43,] "onlay restoration"                          
## [44,] "ceramic onlay restoration"                  
## [45,] "resin onlay restoration"                    
## [46,] "noble metal onlay restoration"              
## [47,] "tooth extraction procedure"                 
## [48,] "surgical dental procedure"                  
## [49,] "porcelain laminate veneer restoration"      
## [50,] "veneer restoration"                         
## [51,] "resin laminate veneer restoration"          

What kinds of clinical findings do we have

queryc("select distinct ?l where ",
       " { ?i a clinical_finding: . ",
       "   ?i a ?t. ",
       "   ?t rdfs:subClassOf clinical_finding:.",
       "   ?t rdfs:label ?l",
       "}")

## Part whole relations among processes, check they all have occurrence_date
## (they don't as of now)
queryc("select distinct ?l ?examd ?lup  ?thingd ?ldown ?whatd where ",
       " { ?i a clinical_finding: . ",
       "   ?exam has_specified_output: ?i .",
       "   ?exam rdfs:label ?l.",
       "   ?exam is_part_of: ?thing.",
       "   ?what is_part_of: ?exam.",
       "   ?what rdfs:label ?ldown.",
       "   ?thing rdfs:label ?lup.",
       "   optional {?exam occurrence_date: ?examd}.",
       "   optional {?thing occurrence_date: ?thingd}.",
       "   optional {?what occurrence_date: ?whatd}.",
       "} limit 10")


# Check there are no parts of outpatient encounters that don't have occurence date
queryc("select distinct ?thing ?date",
       "WHERE",
       "{ ?visit a outpatient_encounter:. ",
       "  ?visit occurrence_date: ?date.",
       "  ?thing is_part_of: ?visit.",
       "  ?thing a ?thingt.",
       "  ?thingt rdfs:label ?tlabel.",
       "  ?thing a process:.",
       "  optional{?thing occurrence_date: ?existing}.",
       "  filter (!bound(?existing))",
       " }limit 10")

# Even more stong - we shouln't have *any* process instances without an occurrence date.
queryc("select ?p where { ?p a process:. optional {?p occurrence_date: ?d} filter(!bound(?d)) }")


 queryc(" select ?patient (min(?date) as ?earliest) (max(?date) as ?latest)
       (count(?date) as ?nvisits) where
{
 ?visit a outpatient_encounter:.
 ?visit occurrence_date: ?date.
 ?patient participates_in: ?visit.
 ?patient a dental_patient:.
} GROUP BY ?patient LIMIT 10
")



A patient with some lost to followup
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_ceab4f2aefd8b3677f3819687e2109ce metal onlay restoration on tooth 19 in patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_f5629c3b1af358f5531c41488abfd7f6 metal onlay restoration on tooth 18 in patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_3aa34821d00329766210928bee651fd5 metal onlay restoration on tooth 31 in patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_e3e34d956af19fa067b0b56bf7cda287 metal onlay restoration on tooth 30 in patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_5dabe0db47a3c16b5f797b426a2ae449 dental visit for patient 963 on 1999-07-28
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_9d101dd3ad2d755958080a9fc120a023 periodic oral evaluation for patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_b8bf36000e33b1ccd959e8d2cc67f2e2 periodic oral evaluation for patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_2e1824b1a2ddf9452bac3676df915444 periodic oral evaluation for patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_4bb286dc93f86598510d436cd6a9364d periodic oral evaluation for patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_02b6f8b46ac49f0afe918ea7985a2cab dental exam for patient 963 on 2000-09-09
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_618aeb28f48451ebc68f77c8dfd49930 dental visit for patient 963 on 2000-09-09
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_a7f7622007e8c3672c834c6a99b73b3c dental exam for patient 963 on 1999-07-28
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_def691bd238e0f902d08bb1071063685 dental exam for patient 963 on 2000-02-19
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_ba73a03601e9257745825741aa4f3904 dental exam for patient 963 on 2000-06-03
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_5be2665842295df8a320f508e24c49e3 dental visit for patient 963 on 2000-06-03
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_390412cdb02a9bd9d3a2612e3995a1b7 dental visit for patient 963 on 2000-02-19
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_d3606798ca0745ddad542211ddec4631 resin filling restoration on tooth 3 in patient 963
patient 963 http://purl.obolibrary.org/obo/BFO_0000056 participates in http://purl.obolibrary.org/obo/ohd/individuals/I_f25e6520893af527368c274e34b550b5 resin filling restoration on tooth 14 in patient 963

queryc(" select ?process ?date where
{


 ?visit a outpatient_encounter:.
 ?visit occurrence_date: ?date.
 ?patient participates_in: ?visit.
 ?patient a dental_patient:.
} GROUP BY ?patient LIMIT 10
")


## snippets for now

PREFIX translated: <file:///triplestores/r21-owl/>
load translated:r21-eaglesoft-crowns.owl;
load translated:r21-eaglesoft-dental-patients.owl;
load translated:r21-eaglesoft-dental-providers.owl;
load translated:r21-eaglesoft-dental-visits.owl;
load translated:r21-eaglesoft-endodontics.owl;
load translated:r21-eaglesoft-fillings.owl;
load translated:r21-eaglesoft-inlays.owl;
load translated:r21-eaglesoft-onlays.owl;
load translated:r21-eaglesoft-surgical-extractions.owl;
load translated:r21-eaglesoft-veneers.owl;
load translated:r21-eaglesoft-missing-teeth-findings.owl
load translated:r21-eaglesoft-unerupted-teeth-findings.owl
load translated:r21-eaglesoft-caries-findings.owl
load translated:r21-eaglesoft-oral-evaluations.owl
load translated:caro-imports.owl;
load translated:cdt-imports.owl;
load translated:fma-jaws-teeth.owl;
load translated:iao-imports.owl;
load translated:ncbi-imports.owl;
load translated:obi-imports.owl;
load translated:ontology-metadata.owl;
load translated:protege-dc.owl;
load translated:bfo2.owl;
load translated:bfo2-classes.owl;
load translated:bfo2-regions.owl;
load translated:bfo2-relations.owl;
load translated:ogms.owl;
load translated:ohd.owl;

From the configurator. Left entity index at 200000

[] a rep:Repository ;
   rep:repositoryID "ohd-partial-2015-04-08-RL" ;
   rdfs:label "ohd-partial-2015-04-08-RL" ;
   rep:repositoryImpl [
      rep:repositoryType "openrdf:SailRepository" ;
      sr:sailImpl [
        sail:sailType "owlim:Sail" ;
        owlim:ruleset "owl2-rl-optimized" ;
        owlim:storage-folder "storage" ;

        owlim:repository-type "file-repository" ;
        owlim:base-URL "http://example.org/owlim#" ;
        owlim:imports "" ;
        owlim:defaultNS "" ;

        owlim:entity-index-size "200000" ;
        owlim:cache-memory "6976m" ;
        owlim:tuple-index-memory "4186m" ;
        owlim:enable-context-index "false" ;
        owlim:enablePredicateList "true" ;
        owlim:predicate-memory "2790m" ;
        owlim:fts-memory "0" ;
        owlim:ftsIndexPolicy "never" ;
        owlim:ftsLiteralsOnly "true" ;
        owlim:in-memory-literal-properties "false" ;
        owlim:transaction-mode "fast" ;
        owlim:transaction-isolation "false" ;
        ]
    ].


PREFIX owlim: <http://www.ontotext.com/trree/owlim#>
PREFIX sr: <http://www.openrdf.org/config/repository/sail#>
PREFIX rep: <http://www.openrdf.org/config/repository#>

select * where
 {graph ?ctx
 {  _:a rdfs:label "OHD R21 2015-04-08 RL".
    _:a  a rep:Repository.
    _:a ?p ?o.}
    ?ctx ?p1 ?o1.
    ?ctx a rep:RepositoryContext.
#   ?ctx owlim:owlim-license ?license.
#   ?ctx rdfs:label "OHD R21 2015-04-08 RL".
#   ?impl rdfs:label ?v.
#   ?sailimpl sr:sailImpl ?ctx.
   #  ?repoimp rep:repositoryImp ?ctx.
   }

### fix the license
PREFIX owlim: <http://www.ontotext.com/trree/owlim#>
PREFIX sr: <http://www.openrdf.org/config/repository/sail#>
PREFIX rep: <http://www.openrdf.org/config/repository#>

DELETE { graph ?ctx 
   {?place owlim:owlim-license ?license.} } 
INSERT { graph ?ctx 
   {_:t owlim:query-timeout " ?place owlim:owlim-license "/triplestores/UNIVERSITYOF_BUFFALO_OWLIM_SE_latest-30-09-2014_4cores.license". }}
WHERE 
 { ?ctx a rep:RepositoryContext.
   graph ?ctx
   {  _:a rdfs:label "OHD R21 2015-04-08 RL".
      _:a  a rep:Repository.
     _:b owlim:owlim-license ?license.
     }
   }

### change the timeout
PREFIX owlim: <http://www.ontotext.com/trree/owlim#>
PREFIX sr: <http://www.openrdf.org/config/repository/sail#>
PREFIX rep: <http://www.openrdf.org/config/repository#>

DELETE { graph ?ctx 
   {?place owlim:query-timeout ?current.} } 
INSERT { graph ?ctx 
   {?place owlim:query-timeout "-1". }}
WHERE 
 { ?ctx a rep:RepositoryContext.
   graph ?ctx
   {  _:a rdfs:label "OHD R21 2015-04-08 RL".
      _:a  a rep:Repository.
     ?place owlim:query-timeout ?current.
     }
   }
### property chain
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dental_patient: <http://purl.obolibrary.org/obo/OHD_0000012>
PREFIX inheres_in: <http://purl.obolibrary.org/obo/BFO_0000052>
PREFIX participates_in: <http://purl.obolibrary.org/obo/BFO_0000056>
PREFIX dental_procedure: <http://purl.obolibrary.org/obo/OHD_0000002>
PREFIX realizes: <http://purl.obolibrary.org/obo/BFO_0000055>

INSERT
  { ?thingi participates_in: ?processi }
WHERE

{
  # chain. thing inv(inheres_in) role inv(realizes) process
  ?rolei  inheres_in: ?thingi.
  ?processi  realizes: ?rolei.
}

DELETE { graph ?ctx 
   {?place owlim:query-timeout "30^^xsd:int".} } 
INSERT { graph ?ctx 
   {_:t owlim:query-timeout " ?place owlim:owlim-license "/triplestores/UNIVERSITYOF_BUFFALO_OWLIM_SE_latest-30-09-2014_4cores.license". }}
WHERE 
 { ?ctx a rep:RepositoryContext.
   graph ?ctx
   {  _:a rdfs:label "OHD R21 2015-04-08 RL".
      _:a  a rep:Repository.
     _:b owlim:owlim-license ?license.
     }
   }
# queryc("select ?thing ?date",
# "WHERE",
# "{ ?visit a "outpatient_encounter:". ",
# "  ?visit occurrence_date: ?date.",
# "  ?thing is_part_of: ?visit.",
# "  ?thing a process:.",
#   " }limit 10")


# queryc("select distinct ?thingt",
# "WHERE",
# "{ ?visit a outpatient_encounter:. ",
# "  ?visit occurrence_date: ?date.",
# "  ?thing is_part_of: ?visit.",
# "  ?thing a ?thingt.",
# "  ?thingt rdfs:label ?tlabel.",
# "  ?thing a process:.",
# "  optional{?thing occurrence_date: ?existing}.",
#   "  filter (!bound(?existing))",
#   " }limit 10")

# queryc("select distinct ?thing ?date",
# "WHERE",
# "{ ?visit a outpatient_encounter:. ",
# "  ?visit occurrence_date: ?date.",
# "  ?thing is_part_of: ?visit.",
# "  ?thing a ?thingt.",
# "  ?thingt rdfs:label ?tlabel.",
# "  ?thing a process:.",
# "  optional{?thing occurrence_date: ?existing}.",
# "  filter (!bound(?existing))",
# " }limit 10")

set_graphdb_license <-function ()
{ sparqlUpdate("DELETE { graph ?ctx ",
               "   {?place owlim:owlim-license ?license.} } ",
               "INSERT { graph ?ctx ",
               "   {_:t owlim:query-timeout ", ?place owlim:owlim-license \"/triplestores/UNIVERSITYOF_BUFFALO_OWLIM_SE_latest-30-09-2014_4cores.license\". }}",
"WHERE ",
" { ?ctx a rep:RepositoryContext.",
"   graph ?ctx",
"   {  _:a rdfs:label "OHD R21 2015-04-08 RL".",
"      _:a  a rep:Repository.",
"     _:b owlim:owlim-license ?license.",
"     }",
"   }")
}


