## Custom subset goals for OHD
##
## If you need to customize your Makefile, make
## changes here rather than in the main Makefile.

# ----------------------------------------
# Subset assets
# ----------------------------------------
# note: this overrides the (empty) SUBSETS list in the main Makefile

SUBSETS = odfa

SUBSET_ROOTS = $(patsubst %, $(SUBSETDIR)/%, $(SUBSETS))
SUBSET_FILES = $(foreach n,$(SUBSET_ROOTS), $(foreach f,$(FORMATS_INCL_TSV), $(n).$(f)))

.PHONY: all_subsets
all_subsets: $(SUBSET_FILES)

# ----------------------------------------
# Subsets
# ----------------------------------------
# To add a subset (e.g., xyz):
#   1. add xyz to SUBSETS above
#   2. create $(SPARQLDIR)/xyz-subset.rq and $(SUBSETDIR)/xyz-annotations.owl
#   3. add a rule like the odfa rule below

.PRECIOUS: $(SUBSETDIR)/%.owl $(SUBSETDIR)/%.tsv

$(SUBSETDIR)/odfa.owl: $(ONT).owl $(SPARQLDIR)/odfa-subset.rq $(SUBSETDIR)/odfa-annotations.owl | $(SUBSETDIR)
	@echo "\n** building $@ **"
	$(call extract-subset,$@,$<,$(word 2, $^),$(word 3, $^))

# ----------------------------------------
# Subset formats
# ----------------------------------------

$(SUBSETDIR)/%.tsv: $(SUBSETDIR)/%.owl
	$(ROBOT) query -f tsv -i $< -s ../sparql/labels.sparql $@

$(SUBSETDIR)/%.obo: $(SUBSETDIR)/%.owl
	$(ROBOT) convert --input $< --check false -f obo $(OBO_FORMAT_OPTIONS) -o $@.tmp.obo && grep -v ^owl-axioms $@.tmp.obo > $@ && rm $@.tmp.obo

$(SUBSETDIR)/%.json: $(SUBSETDIR)/%.owl
	$(ROBOT) convert --input $< --check false -f json -o $@.tmp.json &&\
	mv $@.tmp.json $@

# ----------------------------------------
# Helper functions
# ----------------------------------------

#### Subset Functions ####

# builds a subset: the SPARQL query selects the subset terms, a STAR module
# is extracted for those terms, and the subset annotations ontology is merged in
# parameters: $(1): target subset (.owl); $(2): source ontology; $(3): SPARQL query file;
#             $(4): subset annotations ontology
define extract-subset
	$(ROBOT) query -i $(2) -q $(3) $(1).tmp.csv &&\
	tail -n +2 $(1).tmp.csv > $(1).tmp.txt &&\
	rm $(1).tmp.csv &&\
	$(ROBOT) extract --method STAR \
			--input $(2) \
			--term-file $(1).tmp.txt \
		--output $(1).tmp.owl &&\
	$(ROBOT) merge \
			--include-annotations true \
			--input $(4) \
			--input $(1).tmp.owl \
		annotate \
			--ontology-iri $(URIBASE)/$(ONT)/$(notdir $(1)) \
			--version-iri $(URIBASE)/$(ONT)/releases/$(VERSION)/$(notdir $(1)) \
			--annotation owl:versionInfo $(VERSION) \
		--output $(1) &&\
	rm $(1).tmp.owl &&\
	rm $(1).tmp.txt
endef
