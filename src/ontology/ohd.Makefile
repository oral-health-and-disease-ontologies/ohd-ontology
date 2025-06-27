# CUSTOM MAKE GOALS FOR OHD

# empty target used for forcing a rule to run
FORCE: ;

prepare_release: test custom_reports all_assets
	rsync -R $(RELEASE_ASSETS) $(RELEASEDIR) &&\
	rm -f $(CLEANFILES) &&\
	echo "Release files are now in $(RELEASEDIR) - now you should commit, push and make a release \
        on your git hosting site such as GitHub or GitLab"

.PHONY: all_assets
all_assets: $(ASSETS)


.PHONY: elk_test
# HERMIT takes a long time to run and causes a heap space error with github actions
# So, setting up a test that uses ELK
elk_test: elk_reason_test sparql_test robot_reports $(REPORTDIR)/validate_profile_owl2dl_$(ONT).owl.txt
	@echo "** Finished running all tests successfully. **"

.PHONY: elk_test_fast
# runs elk_test w/o cheking import checks (i.e., $(MAKE) IMP=false PAT=false COMP=false MIR=false)
elk_test_fast:
	$(MAKE_FAST) elk_test

.PHONY: reason_test
reason_test: $(EDIT_PREPROCESSED)
	$(ROBOT) reason --input $< --reasoner HERMIT --equivalent-classes-allowed asserted-only \
		--exclude-tautologies structural --output test.owl && rm test.owl

.PHONY: elk_reason_test
# run reasoning test using ELK
elk_reason_test: $(EDIT_PREPROCESSED)
	$(ROBOT) reason --input $< --reasoner ELK --equivalent-classes-allowed asserted-only \
		--exclude-tautologies structural --output test.owl && rm test.owl

# ----------------------------------------
# Release artifacts
# ----------------------------------------

$(ONT).owl: $(SRC)
	@echo "\n** building $@ **"
	$(ROBOT) \
		merge -i $< \
		reason --reasoner ELK --annotate-inferred-axioms true --exclude-duplicate-axioms true \
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
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/ro_import.owl
$(IMPORTDIR)/ro_import.owl: $(MIRRORDIR)/ro.owl $(IMPORTDIR)/ro_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
			--select classes \
		extract \
			--method MIREOT \
			--lower-terms $(word 2, $^) \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/iao_import.owl 
$(IMPORTDIR)/iao_import.owl: $(MIRRORDIR)/iao.owl $(IMPORTDIR)/iao_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method MIREOT \
			--upper-term BFO:0000031 \
			--lower-terms $(word 2, $^) \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/caro_import.owl
$(IMPORTDIR)/caro_import.owl: $(MIRRORDIR)/caro.owl $(IMPORTDIR)/caro_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method MIREOT \
			--branch-from-term BFO:0000040 \
		extract \
			--method MIREOT \
			--lower-terms $(word 2, $^) \
			--intermediates minimal \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/envo_import.owl: $(MIRRORDIR)/envo.owl $(IMPORTDIR)/envo_terms.txt 
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method MIREOT \
            --branch-from-term BFO:0000001 \
        extract \
            --method MIREOT \
            --lower-terms $(word 2, $^) \
            --intermediates minimal \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/chebi_import.owl: $(MIRRORDIR)/chebi.owl.gz $(IMPORTDIR)/chebi_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
        remove \
            --input $< \
            --select "owl:deprecated='true'^^xsd:boolean" \
        extract \
            --method MIREOT \
            --branch-from-term CHEBI:24431 \
        extract \
            --method MIREOT \
            --lower-terms  $(word 2, $^) \
            --intermediates minimal \
        annotate \
            --annotate-defined-by true \
            --ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
        --output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/ecto_import.owl: $(MIRRORDIR)/ecto.owl $(IMPORTDIR)/ecto_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method MIREOT \
			--upper-term  http://purl.obolibrary.org/obo/ExO_0000002 \
			--lower-terms $(word 2, $^) \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/go_import.owl
$(IMPORTDIR)/go_import.owl: $(MIRRORDIR)/go.owl $(IMPORTDIR)/go_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method MIREOT \
			--upper-term http://purl.obolibrary.org/obo/GO_0008150 \
			--lower-terms $(word 2, $^) \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Filters out all terms except those in the terms file
.PRECIOUS: $(IMPORTDIR)/ido_import.owl
$(IMPORTDIR)/ido_import.owl: $(MIRRORDIR)/ido.owl $(IMPORTDIR)/ido_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		filter \
			--input $< \
			--term-file $(word 2, $^) \
			--select "annotations self ancestors" \
			--axioms logical \
			--signature true \
			--trim true \
		remove \
			--select "owl:deprecated='true'^^xsd:boolean" \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

.PRECIOUS: $(IMPORTDIR)/ohmi_import.owl
$(IMPORTDIR)/ohmi_import.owl: $(MIRRORDIR)/ohmi.owl $(IMPORTDIR)/ohmi_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
			--select classes \
		extract \
			--method MIREOT \
			--lower-terms  $(word 2, $^) \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Filters out all terms except those in the terms file
.PRECIOUS: $(IMPORTDIR)/pato_import.owl
$(IMPORTDIR)/pato_import.owl: $(MIRRORDIR)/pato.owl $(IMPORTDIR)/pato_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		filter \
			--input $< \
			--term-file $(word 2, $^) \
			--select "annotations self ancestors" \
			--axioms logical \
			--signature true \
			--trim true \
		remove \
			--select "owl:deprecated='true'^^xsd:boolean" \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Filters out all terms except those in the terms file
.PRECIOUS: $(IMPORTDIR)/ogms_import.owl
$(IMPORTDIR)/ogms_import.owl: $(MIRRORDIR)/ogms.owl $(IMPORTDIR)/ogms_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		filter \
			--input $< \
			--term-file $(word 2, $^) \
			--select "annotations self ancestors" \
			--axioms logical \
			--signature true \
			--trim true \
		remove \
			--select "owl:deprecated='true'^^xsd:boolean" \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Filters out all terms except those in the terms file
.PRECIOUS: $(IMPORTDIR)/omrse_import.owl
$(IMPORTDIR)/omrse_import.owl: $(MIRRORDIR)/omrse.owl $(IMPORTDIR)/omrse_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		filter \
			--input $< \
			--term-file $(word 2, $^) \
			--select "annotations self ancestors" \
			--axioms logical \
			--signature true \
			--trim true \
		remove \
			--select "owl:deprecated='true'^^xsd:boolean" \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/obi_import_test.owl: $(MIRRORDIR)/obi.owl $(IMPORTDIR)/obi_terms.txt
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method STAR \
			--term-file $(word 2, $^) \
		remove \
			--term-file $(word 2, $^) \
			--exclude-terms  $(IMPORTDIR)/annotation_terms.txt \
			--select complement \
		annotate \
			--annotate-defined-by true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
			--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $@) \
		convert --format ofn \
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