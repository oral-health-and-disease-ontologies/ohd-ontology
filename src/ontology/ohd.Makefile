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

$(IMPORTDIR)/omo_import.owl: $(MIRRORDIR)/omo.owl.gz
	$(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        remove \
            --select classes \
        annotate \
            --annotate-defined-by true \
            --annotate-derived-from true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
        --output $@.tmp.owl && mv $@.tmp.owl $@

$(IMPORTDIR)/ro_import.owl: $(MIRRORDIR)/ro.owl.gz
	$(ROBOT) \
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
        --output $@.tmp.owl && mv $@.tmp.owl $@

$(IMPORTDIR)/iao_import.owl: $(MIRRORDIR)/iao.owl.gz
	$(ROBOT) \
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
        --output $@.tmp.owl && mv $@.tmp.owl $@

$(IMPORTDIR)/caro_import.owl: $(MIRRORDIR)/caro.owl.gz
	$(ROBOT) \
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
        --output $@.tmp.owl && mv $@.tmp.owl $@

$(IMPORTDIR)/ecto_import.owl: $(MIRRORDIR)/ecto.owl.gz
	$(ROBOT) \
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
        --output $@.tmp.owl && mv $@.tmp.owl $@

$(IMPORTDIR)/obi_import_test.owl: $(MIRRORDIR)/obi.owl.gz
	$(ROBOT) \
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
        --output $@.tmp.owl && mv $@.tmp.owl $@
