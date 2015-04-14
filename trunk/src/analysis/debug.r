## This was to check whether CDT were is_about other than procedures. As it turned out it was. We will change that.

queryc(" select distinct ?procl",
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
