# CUSTOM MAKE GOALS FOR OHD

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

$(IMPORTDIR)/omo_import.owl: $(MIRRORDIR)/omo.owl
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        remove \
            --select classes \
        annotate \
            --annotate-defined-by true \
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

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
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

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
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

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
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

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
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
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
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

# ----------------------------------------
# Manually maintained mirrors and imports
# ----------------------------------------

mirror-bfo2_classes \
mirror-ncbi \
mirror-caro mirror-cdt \
mirror-fma-jaws-teeth \
mirror-fma-lymph \
mirror-fma-mouth-mucosa mirror-fma-tmj mirror-fma-tongue \
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
