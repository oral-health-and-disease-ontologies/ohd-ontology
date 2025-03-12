# CUSTOM MAKE GOALS FOR OHD

# ----------------------------------------
# Release artifacts
# ----------------------------------------

$(ONT).owl: $(SRC)
	@echo "*** building ***"
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
	@echo "*** building $@ ***"
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
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/ro_import.owl: $(MIRRORDIR)/ro.owl $(IMPORTDIR)/ro_terms.txt
	@echo "*** building $@ ***"
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
			--annotate-derived-from true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/iao_import.owl: $(MIRRORDIR)/iao.owl $(IMPORTDIR)/iao_terms.txt
	@echo "*** building $@ ***"
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
			--annotate-derived-from true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

$(IMPORTDIR)/caro_import.owl: $(MIRRORDIR)/caro.owl $(IMPORTDIR)/caro_terms.txt
	@echo "*** building $@ ***"
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
			--annotate-derived-from true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Creates import from the occurrent branch
# and removes the classes:
#  - exposure event or process
#  - exposure to environmental quality
#  - exposure to chemical with chemical role
# Note: exposure event or process has bee obsoleted in RO, but it is still in ecto
$(IMPORTDIR)/ecto_import.owl: $(MIRRORDIR)/ecto.owl $(IMPORTDIR)/ecto_terms.txt
	@echo "*** building $@ ***"
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method MIREOT \
			--lower-terms $(word 2, $^) \
		remove \
			--term http://purl.obolibrary.org/obo/RO_0002310 \
			--term http://purl.obolibrary.org/obo/ECTO_0010000 \
			--term http://purl.obolibrary.org/obo/ECTO_0000487 \
			--preserve-structure false \
		annotate \
			--annotate-defined-by true \
			--annotate-derived-from true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Lower terms are determined by ogms_terms.txt
# Note: the '_undefined primitive term' branch is removed
$(IMPORTDIR)/ogms_import.owl: $(MIRRORDIR)/ogms.owl $(IMPORTDIR)/ogms_terms.txt
	@echo "*** building $@ ***"
	if [ $(IMP) = true ]; then $(ROBOT) \
		remove \
			--input $< \
			--select "owl:deprecated='true'^^xsd:boolean" \
		extract \
			--method MIREOT \
			--lower-terms $(word 2, $^) \
		remove \
			--term http://purl.obolibrary.org/obo/OGMS_0000067 \
			--select "self descendants" \
		annotate \
			--annotate-defined-by true \
			--annotate-derived-from true \
			--ontology-iri $(URIBASE)/$(ONT)/$@ \
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@; fi

# Filters out all terms except those in the terms file
$(IMPORTDIR)/omrse_import.owl: $(MIRRORDIR)/omrse.owl $(IMPORTDIR)/omrse_terms.txt
	@echo "*** building $@ ***"
	$(ROBOT) \
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
		convert --format ofn \
		--output $@.tmp.owl && mv $@.tmp.owl $@

# used for testing
# $(IMPORTDIR)/obi_import_test.owl: $(MIRRORDIR)/obi.owl
#   @echo "*** building $@ ***"
# 	if [ $(IMP) = true ]; then $(ROBOT) \
#         remove \
#             --input $< \
#             --select "owl:deprecated='true'^^xsd:boolean" \
#         extract \
#             --method STAR \
#             --term-file $(IMPORTDIR)/obi_terms.txt \
#         remove \
#             --term-file $(IMPORTDIR)/obi_terms.txt \
#             --exclude-terms  $(IMPORTDIR)/annotation_terms.txt \
#             --select complement \
#         annotate \
#             --annotate-defined-by true \
#             --annotate-derived-from true \
#             --ontology-iri $(URIBASE)/$(ONT)/$@ \
#         --output $@.tmp.owl && mv $@.tmp.owl $@; fi

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
imports/cdt_import.owl \
imports/ncbi_import.owl \
imports/fma-jaws-teeth_import.owl \
imports/fma-lymph_import.owl \
imports/fma-mouth-mucosa_import.owl \
imports/fma-tmj_import.owl \
imports/fma-tongue_import.owl:
	if [ $(MIR) = true ]; then echo "$@ is manually maintained"; fi
