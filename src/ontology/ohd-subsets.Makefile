
# ----------------------------------------
# Subset assets
# ----------------------------------------


SUBSETS = 

SUBSET_ROOTS = $(patsubst %, $(SUBSETDIR)/%, $(SUBSETS))
SUBSET_FILES = $(foreach n,$(SUBSET_ROOTS), $(foreach f,$(FORMATS_INCL_TSV), $(n).$(f)))

.PHONY: all_subsets
all_subsets: $(SUBSET_FILES)

# ----------------------------------------
# Subsets
# ----------------------------------------

$(SUBSETDIR)/odfa.owl: $(ONT).owl $(SPARQLDIR)/odfa-subset.rq $(SUBSETDIR)/odfa-annotations.owl | $(SUBSETDIR)
	@echo "\n** building $@ **"
	$(ROBOT) query -i $< -q $(word 2, $^) $@.tmp.csv &&\
	tail -n +2 $@.tmp.csv > $@.tmp.txt &&\
	rm $@.tmp.csv &&\
	$(ROBOT) extract --method STAR \
			--input $< \
			--term-file $@.tmp.txt \
		annotate \
			--ontology-iri $(URIBASE)/$(ONT)/$(notdir $@) \
			--version-iri $(URIBASE)/$(ONT)/$(VERSION)/$(notdir $@) \
			--annotation owl:versionInfo $(VERSION) \
		--output $@.tmp.owl &&\
	$(ROBOT) merge \
			--include-annotations true \
			--input $(word 3, $^) \
			--input  $@.tmp.owl \
		--output $@ &&\
	rm $@.tmp.owl
.PRECIOUS: $(SUBSETDIR)/odfa.owl

# $(SUBSETDIR)/%.owl: $(ONT).owl | $(SUBSETDIR)
# 	$(OWLTOOLS) $< --extract-ontology-subset --fill-gaps --subset $* -o $@.tmp.owl && mv $@.tmp.owl $@ &&\
# 	$(ROBOT) annotate --input $@ --ontology-iri $(ONTBASE)/$@ $(ANNOTATE_ONTOLOGY_VERSION) -o $@.tmp.owl && mv $@.tmp.owl $@
# .PRECIOUS: $(SUBSETDIR)/%.owl

$(SUBSETDIR)/%.tsv: $(SUBSETDIR)/%.owl
	$(ROBOT) query -f tsv -i $< -s ../sparql/labels.sparql $@
.PRECIOUS: $(SUBSETDIR)/%.tsv

$(SUBSETDIR)/%.obo: $(SUBSETDIR)/%.owl
	$(ROBOT) convert --input $< --check false -f obo $(OBO_FORMAT_OPTIONS) -o $@.tmp.obo && grep -v ^owl-axioms $@.tmp.obo > $@ && rm $@.tmp.obo

$(SUBSETDIR)/%.json: $(SUBSETDIR)/%.owl
	$(ROBOT) convert --input $< --check false -f json -o $@.tmp.json &&\
	mv $@.tmp.json $@

