# CUSTOM MAKE GOALS FOR OHD

# empty target used for forcing a rule to run
FORCE:

# ----------------------------------------
# Release artifacts
# ----------------------------------------

$(ONT).owl: $(SRC)
	@echo "\n** building $@ **"
	$(ROBOT) \
		merge -i $< \
	    reason --reasoner hermit --annotate-inferred-axioms true --exclude-duplicate-axioms true \
	    annotate \
	        --ontology-iri $(URIBASE)/$@ \
	        --version-iri $(ONTBASE)/releases/$(VERSION)/$@ \
	        --annotation owl:versionInfo $(VERSION) \
	    reduce \
	    convert -o $@.tmp.owl && mv $@.tmp.owl $@
	    

# ----------------------------------------
# Ontology imports
# ----------------------------------------

.PRECIOUS: $(IMPORTDIR)/omo_import.owl
$(IMPORTDIR)/omo_import.owl: $(MIRRORDIR)/omo.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        remove \
            --select classes \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/ro_import.owl
$(IMPORTDIR)/ro_import.owl: $(MIRRORDIR)/ro.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
            --select classes \
        extract \
            --method MIREOT \
            --lower-terms $(IMPORTDIR)/ro_terms.txt \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/iao_import.owl
$(IMPORTDIR)/iao_import.owl: $(MIRRORDIR)/iao.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method MIREOT \
            --upper-term BFO:0000031 \
            --lower-terms $(IMPORTDIR)/iao_terms.txt \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/caro_import.owl
$(IMPORTDIR)/caro_import.owl: $(MIRRORDIR)/caro.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method MIREOT \
            --branch-from-term BFO:0000040 \
        extract \
            --method MIREOT \
            --lower-terms $(IMPORTDIR)/caro_terms.txt \
            --intermediates minimal \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/ecto_import.owl
$(IMPORTDIR)/ecto_import.owl: $(MIRRORDIR)/ecto.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method MIREOT \
            --branch-from-term http://purl.obolibrary.org/obo/ExO_0000002 \
        extract \
            --method MIREOT \
            --lower-terms $(IMPORTDIR)/ecto_terms.txt \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/go_import.owl
$(IMPORTDIR)/go_import.owl: $(MIRRORDIR)/go.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method MIREOT \
            --lower-terms $(IMPORTDIR)/go_terms.txt \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/obi_import_test.owl: $(MIRRORDIR)/obi.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method STAR \
            --term-file $(IMPORTDIR)/obi_terms.txt \
        remove \
            --term-file $(IMPORTDIR)/obi_terms.txt \
            --exclude-terms  $(IMPORTDIR)/annotation_terms.txt \
            --select complement \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

# ----------------------------------------
# Manually maintained mirrors and imports
# ----------------------------------------

mirror-bfo2_classes \
mirror-ncbi \
mirror-caro \
mirror-cdt \
mirror-fma-jaws-teeth \
mirror-fma-lymph \
mirror-fma-mouth-mucosa \
mirror-fma-tmj \
mirror-fma-tongue \
mirror/bfo2_classes.owl \
mirror/ncbi.owl \
mirror/caro.owl \
mirror/cdt.owl \
mirror/fma-jaws-teeth.owl \
mirror/fma-lymph.owl \
mirror/fma-mouth-mucosa.owl \
mirror/fma-tmj.owl \
mirror/fma-tongue.owl \
imports/bfo2_classes_import.owl \
imports/caro_import.owl \
imports/cdt_import.owl \
imports/ncbi_import.owl \
imports/fma-jaws-teeth_import.owl \
imports/fma-lymph_import.owl \
imports/fma-mouth-mucosa_import.owl \
imports/fma-tmj_import.owl \
imports/fma-tongue_import.owl:
	if [ $(MIR) = true ]; then echo "$@ is manually maintained"; fi

# ----------------------------------------
# Mirroring upstream ontologies
# ----------------------------------------

IMP=true # Global parameter to bypass import generation
MIR=true # Global parameter to bypass mirror generation
IMP_LARGE=true # Global parameter to bypass handling of large imports

ifeq ($(strip $(MIR)),true)

.PHONY: mirror-%
mirror-%: | $(TMPDIR)
	@echo "*** mirroring $* ***"
	if [ $(MIR) = true ] && [ $(IMP) = true ] && [ $(IMP_LARGE) = true ]; then \
		curl -L $(OBOBASE)/$*.owl \
			--create-dirs -o $(TMPDIR)/mirror-$(notdir $*).temp.owl --retry 4 --max-time 200 && \
		$(ROBOT) convert \
			--input $(TMPDIR)/mirror-$(notdir $*).temp.owl \
			--output $(TMPDIR)/mirror-$(notdir $*).owl && \
		rm  $(TMPDIR)/mirror-$*.temp.owl; fi

.PRECIOUS: $(MIRRORDIR)/%.owl
$(MIRRORDIR)/%.owl: FORCE | $(TMPDIR)
	if [ -f $(TMPDIR)/mirror-$*.owl ]; then \
		if cmp -s $(TMPDIR)/mirror-$*.owl $@ ; then \
            echo "Mirror identical, ignoring. Run mirror-$* to download $*.owl."; \
		else echo "Mirrors different, updating." && \
			cp $(TMPDIR)/mirror-$*.owl $@; fi; fi

else # MIR=false
$(MIRRORDIR)/%.owl:
	@echo "Not refreshing $@ because the mirrorring pipeline is disabled (MIR=$(MIR))."
endif