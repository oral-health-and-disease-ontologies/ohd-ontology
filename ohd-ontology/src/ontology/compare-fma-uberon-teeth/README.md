# Compare FMA and UBERON Tooth Class

In order to use UBERON in the OHD, I need at least a complete set of human teeth (i.e., dentition). To find teeth that are in the FMA but missing from UBERON, I did the following.

1. Extracted the tooth classes from the FMA using the script [robot-extract-tooth-classes.sh](robot-extract-tooth-classes.sh) with results in the creation of [fma-teeth.owl](fma-teeth.owl). The [fma-teeth.owl](fma-teeth.owl) ontology is then queried (see [fma-tooth-classes.rq](fma-tooth-classes.rq )) to produce  [fma-tooth-classes.tsv](fma-tooth-classes.tsv) which contains the CURIE, label, and FMA ID/CURIE (e.g., "FMA:84239") for each FMA tooth class.

2. Dowloaded the [human-view.owl](human-view.owl) from UBERON and extracted the tooth hierarchy using the script [robot-extract-uberon-fma-tooth.sh](robot-extract-uberon-fma-tooth.sh) which executes the query [extract-uberon-fma-tooth.rq](extract-uberon-fma-tooth.rq). The output of the query has been saved in the file [uberon-fma-results.tsv](uberon-fma-results.tsv) which contains the CURIE, label, FMA ID/CURIE, and annotation used to link the URI to the FMA ID/CURIE for each UBERON tooth class.

3. Executed the script [fma-uberon-misses.sh](fma-uberon-misses.sh) which compares the FMA IDs/CURIEs in [fma-tooth-classes.tsv](fma-tooth-classes.tsv) and   [uberon-fma-results.tsv](uberon-fma-results.tsv). FMA IDs/CURIEs that exist in [fma-tooth-classes.tsv](fma-tooth-classes.tsv) (i.e., the FMA) but not in [uberon-fma-results.tsv](uberon-fma-results.tsv) (i.e., UBERON) are saved to the file [fma-uberon-misses.tsv](fma-uberon-misses.tsv). The script found `52` such FMA IDs/CURIEs.

