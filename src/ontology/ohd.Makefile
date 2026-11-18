## Customize Makefile settings for OHD
##
## If you need to customize your Makefile, make
## changes here rather than in the main Makefile.
##
## Layout:
##   1. Global parameters
##   2. Testing
##   3. Release management / release artefacts
##   4. Ontology imports
##   5. Manually maintained mirrors and imports
##   6. Mirroring upstream ontologies
##   7. Helper functions (import, mirror, release, testing)

# ----------------------------------------
# Global parameters
# ----------------------------------------
# these can be overwritten on the command line (e.g., make IMP=false ...)

IMP=true # Global parameter to bypass import generation
MIR=true # Global parameter to bypass mirror generation
IMP_LARGE=true # Global parameter to bypass handling of large imports

# empty target used for forcing a rule to run
FORCE: ;

# ----------------------------------------
# Testing
# ----------------------------------------

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
# run reasoning test using HermiT
reason_test: $(EDIT_PREPROCESSED)
	$(call reason-test,$<,HERMIT)

.PHONY: elk_reason_test
# run reasoning test using ELK
elk_reason_test: $(EDIT_PREPROCESSED)
	$(call reason-test,$<,ELK)

# ----------------------------------------
# Release management
# ----------------------------------------

prepare_release: custom_reports all_assets
# @tmp_release_assets="$(patsubst %, $(RELEASEDIR)/%, $(RELEASE_ASSETS))"; \
# for f in $$tmp_release_assets; do \
# 	case "$$f" in \
# 		*.tsv) continue ;; \
# 	esac; \
# 	$(call remove-obsolete-class,$$f); \
# done

	$(ROBOT) report -i $(ONT).owl $(REPORT_LABEL) $(REPORT_PROFILE_OPTS) \
					--fail-on $(REPORT_FAIL_ON) \
					--base-iri $(URIBASE)/OHD_ \
					--base-iri $(URIBASE)/ohd \
					--print 5 \
					-o $(REPORTDIR)/$(SRC)-obo-report.tsv

	rsync -R $(RELEASE_ASSETS) $(RELEASEDIR) &&\
	rm -f $(CLEANFILES) &&\
	echo "Release files are now in $(RELEASEDIR) - now you should commit, push and make a release \
        on your git hosting site such as GitHub or GitLab"

.PHONY: all_assets
all_assets: $(ASSETS)
	@echo "\n ** Finished building all assets successfully. $(ASSETS) ** \n"

# ----------------------------------------
# Release artifacts
# ----------------------------------------

# in the ohd.owl release we need to delete the language tags to remove duplicate annotations
# the "rdfs:isDefinedBy" annotation is also removed from the release version of ohd.owl
# (both of these are done when $(EDIT_PREPROCESSED) is built; see the main Makefile)
$(ONT).owl: $(ONT)-full.owl
	@echo "\n** building $@ **"
	$(ROBOT) \
		annotate -i $< \
			--ontology-iri $(URIBASE)/$@ \
			--version-iri $(ONTBASE)/releases/$(VERSION)/$@ \
			--annotation owl:versionInfo $(VERSION) \
		reduce \
		convert -o $@.tmp.owl && \
		$(call remove-obsolete-class,$@.tmp.owl) && \
		mv $@.tmp.owl $@

# ----------------------------------------
# Ontology imports
# ----------------------------------------
# Each import is built by one of the import functions defined in the
# "Helper functions" section at the bottom of this file:
#   filter-import        - ROBOT filter: "annotations self ancestors"
#   bot-import           - ROBOT extract: BOT module
#   mireot-import        - ROBOT extract: MIREOT (optional upper term)
#   branch-mireot-import - ROBOT extract: MIREOT branch, then MIREOT lower terms
# All functions remove deprecated terms, add rdfs:isDefinedBy, set the
# ontology/version IRIs, and write the import in OFN.
# Imports are only rebuilt when IMP=true.

.PRECIOUS: $(IMPORTDIR)/%_import.owl

# ---- filter imports ----

$(IMPORTDIR)/mf_import.owl: $(MIRRORDIR)/mf.owl $(IMPORTDIR)/mf_terms.txt
	$(call filter-import,$@,$<,$(word 2, $^))

$(IMPORTDIR)/ido_import.owl: $(MIRRORDIR)/ido.owl $(IMPORTDIR)/ido_terms.txt
	$(call filter-import,$@,$<,$(word 2, $^))

$(IMPORTDIR)/pato_import.owl: $(MIRRORDIR)/pato.owl $(IMPORTDIR)/pato_terms.txt
	$(call filter-import,$@,$<,$(word 2, $^))

$(IMPORTDIR)/ogms_import.owl: $(MIRRORDIR)/ogms.owl $(IMPORTDIR)/ogms_terms.txt
	$(call filter-import,$@,$<,$(word 2, $^))

$(IMPORTDIR)/omrse_import.owl: $(MIRRORDIR)/omrse.owl $(IMPORTDIR)/omrse_terms.txt
	$(call filter-import,$@,$<,$(word 2, $^))

# uberon: also remove unneeded NCBI Taxon terms
$(IMPORTDIR)/uberon_import.owl: $(MIRRORDIR)/uberon.owl $(IMPORTDIR)/uberon_terms.txt
	$(call filter-import,$@,$<,$(word 2, $^),remove --select "<http://purl.obolibrary.org/obo/NCBITaxon_*>")

# ---- BOT imports ----

$(IMPORTDIR)/emro_import.owl: $(MIRRORDIR)/emro.owl $(IMPORTDIR)/emro_terms.txt
	$(call bot-import,$@,$<,$(word 2, $^))

$(IMPORTDIR)/pain_import.owl: $(MIRRORDIR)/pain.owl $(IMPORTDIR)/pain_terms.txt
	$(call bot-import,$@,$<,$(word 2, $^))

# ---- MIREOT imports ----

# ro and ohmi: only deprecated *classes* are removed before the extract
$(IMPORTDIR)/ro_import.owl: $(MIRRORDIR)/ro.owl $(IMPORTDIR)/ro_terms.txt
	$(call mireot-import,$@,$<,$(word 2, $^),,classes)

$(IMPORTDIR)/ohmi_import.owl: $(MIRRORDIR)/ohmi.owl $(IMPORTDIR)/ohmi_terms.txt
	$(call mireot-import,$@,$<,$(word 2, $^),,classes)

# iao: upper term is BFO:0000031 (generically dependent continuant)
$(IMPORTDIR)/iao_import.owl: $(MIRRORDIR)/iao.owl $(IMPORTDIR)/iao_terms.txt
	$(call mireot-import,$@,$<,$(word 2, $^),BFO:0000031)

# ecto: upper term is ExO_0000002 (exposure stressor)
$(IMPORTDIR)/ecto_import.owl: $(MIRRORDIR)/ecto.owl $(IMPORTDIR)/ecto_terms.txt
	$(call mireot-import,$@,$<,$(word 2, $^),http://purl.obolibrary.org/obo/ExO_0000002)

# go: upper term is GO_0008150 (biological process)
$(IMPORTDIR)/go_import.owl: $(MIRRORDIR)/go.owl $(IMPORTDIR)/go_terms.txt
	$(call mireot-import,$@,$<,$(word 2, $^),http://purl.obolibrary.org/obo/GO_0008150)

# ---- MIREOT branch imports ----
# note: envo and chebi are imported by components/dental_material.owl

# envo: branch from BFO:0000001 (entity)
$(IMPORTDIR)/envo_import.owl: $(MIRRORDIR)/envo.owl $(IMPORTDIR)/envo_terms.txt
	$(call branch-mireot-import,$@,$<,$(word 2, $^),BFO:0000001)

# chebi: branch from CHEBI:24431 (chemical entity); uses the gzipped mirror
$(IMPORTDIR)/chebi_import.owl: $(MIRRORDIR)/chebi.owl.gz $(IMPORTDIR)/chebi_terms.txt
	$(call branch-mireot-import,$@,$<,$(word 2, $^),CHEBI:24431)

# ---- other imports ----

# omo: annotation properties only (all classes are removed)
$(IMPORTDIR)/omo_import.owl: $(MIRRORDIR)/omo.owl
	if [ $(IMP) = true ]; then \
		$(ROBOT) \
			remove \
				--input $< \
				--select "owl:deprecated='true'^^xsd:boolean" \
			remove \
				--select classes \
			$(call annotate-import,$@); fi

# experimental: STAR module of OBI (not part of IMPORTS)
$(IMPORTDIR)/obi_import_test.owl: $(MIRRORDIR)/obi.owl $(IMPORTDIR)/obi_terms.txt
	if [ $(IMP) = true ]; then \
		$(ROBOT) \
			remove \
				--input $< \
				--select "owl:deprecated='true'^^xsd:boolean" \
			extract \
				--method STAR \
				--term-file $(word 2, $^) \
			remove \
				--term-file $(word 2, $^) \
				--exclude-terms $(IMPORTDIR)/annotation_terms.txt \
				--select complement \
			$(call annotate-import,$@); fi

# ----------------------------------------
# Manually maintained mirrors and imports
# ----------------------------------------
# note: caro_import.owl is listed here, so it is NOT regenerated from mirror/caro.owl

MANUAL_MIRRORS = bfo2_classes ncbi caro cdt fma-jaws-teeth fma-lymph fma-mouth-mucosa fma-tmj fma-tongue
MANUAL_IMPORTS = bfo2_classes caro cdt ncbi fma-jaws-teeth fma-lymph fma-mouth-mucosa fma-tmj fma-tongue

$(patsubst %, mirror-%, $(MANUAL_MIRRORS)) \
$(patsubst %, $(MIRRORDIR)/%.owl, $(MANUAL_MIRRORS)) \
$(patsubst %, $(IMPORTDIR)/%_import.owl, $(MANUAL_IMPORTS)):
	if [ $(MIR) = true ]; then echo "$@ is manually maintained"; fi

# ----------------------------------------
# Mirroring upstream ontologies
# ----------------------------------------

ifeq ($(strip $(MIR)),true)

# mirror-% downloads the ontology and updates the mirror only if it has changed
# force-mirror-% updates the mirror regardless of whether the source has changed or not
mirror-% force-mirror-%: | $(TMPDIR) $(MIRRORDIR)
	$(call mirror-ontology,$*,,$(firstword $(subst -, ,$@)))

# $(MIRRORDIR)/%.owl is only built when the mirror file is missing, or when
# make is run with -B (e.g., the refresh-% and refresh-imports goals).
# The mirror directory is only updated if the download differs from the mirror.
.PRECIOUS: $(MIRRORDIR)/%.owl
$(MIRRORDIR)/%.owl: | $(TMPDIR) $(MIRRORDIR)
	$(call mirror-ontology,$*,,)

.PHONY: all-mirrors
all-mirrors:
	$(MAKE) $(patsubst %, $(MIRRORDIR)/%.owl, $(filter-out $(MANUAL_MIRRORS), $(IMPORTS)))

.PHONY: force-all-mirrors
force-all-mirrors:
	$(MAKE) $(patsubst %, force-mirror-%, $(filter-out $(MANUAL_MIRRORS), $(IMPORTS)))

else # MIR=false
$(MIRRORDIR)/%.owl $(MIRRORDIR)/%.owl.gz:
	@echo "Not refreshing $@ because the mirrorring pipeline is disabled (MIR=$(MIR))."
endif

# ----------------------------------------
# Helper functions
# ----------------------------------------

#### Import Functions ####

# common tail of every import pipeline: adds rdfs:isDefinedBy, sets the
# ontology/version IRIs, and writes the import (OFN) via a temp file
# parameters: $(1): target
define annotate-import
			annotate \
				--annotate-defined-by true \
				--ontology-iri $(URIBASE)/$(ONT)/$(1) \
				--version-iri $(URIBASE)/$(ONT)/imports/$(VERSION)/$(notdir $(1)) \
			convert --format ofn \
			--output $(1).tmp.owl && mv $(1).tmp.owl $(1)
endef

# filters out all terms except those in the term file (and their ancestors)
# parameters: $(1): target; $(2): mirror file; $(3): term file;
#             $(4): optional extra ROBOT commands run after deprecated terms are removed
define filter-import
	if [ $(IMP) = true ]; then \
		$(ROBOT) \
			filter \
				--input $(2) \
				--term-file $(3) \
				--select "annotations self ancestors" \
				--axioms logical \
				--signature true \
				--trim true \
			remove \
				--select "owl:deprecated='true'^^xsd:boolean" \
			$(4) \
			$(call annotate-import,$(1)); fi
endef

# extracts a BOT module for the terms in the term file
# parameters: $(1): target; $(2): mirror file; $(3): term file
define bot-import
	if [ $(IMP) = true ]; then \
		$(ROBOT) \
			extract \
				--input $(2) \
				--term-file $(3) \
				--method BOT \
			remove \
				--select "owl:deprecated='true'^^xsd:boolean" \
			$(call annotate-import,$(1)); fi
endef

# extracts a MIREOT module using the term file as the lower terms
# parameters: $(1): target; $(2): mirror file; $(3): term file;
#             $(4): optional upper term;
#             $(5): optional extra selector applied when removing deprecated terms (e.g., classes)
define mireot-import
	if [ $(IMP) = true ]; then \
		$(ROBOT) \
			remove \
				--input $(2) \
				--select "owl:deprecated='true'^^xsd:boolean" \
				$(if $(strip $(5)),--select $(strip $(5))) \
			extract \
				--method MIREOT \
				$(if $(strip $(4)),--upper-term $(strip $(4))) \
				--lower-terms $(3) \
			$(call annotate-import,$(1)); fi
endef

# extracts a MIREOT branch, then a MIREOT module of the term file (minimal intermediates)
# parameters: $(1): target; $(2): mirror file; $(3): term file; $(4): branch-from term
define branch-mireot-import
	if [ $(IMP) = true ]; then \
		$(ROBOT) \
			remove \
				--input $(2) \
				--select "owl:deprecated='true'^^xsd:boolean" \
			extract \
				--method MIREOT \
				--branch-from-term $(4) \
			extract \
				--method MIREOT \
				--lower-terms $(3) \
				--intermediates minimal \
			$(call annotate-import,$(1)); fi
endef

#### Mirror Functions ####

# General function for mirroring an ontology. The ontology is downloaded and
# converted with ROBOT (the conversion also checks the download is a valid ontology).
# The mirror is only updated if the converted download differs from the
# existing mirror, or if the update is forced.
# parameters: $(1): name of the ontology to mirror;
#             $(2): optional base URL to download from (defaults to $(OBOBASE));
#             $(3): "force" to update regardless of whether the source has changed or not
define mirror-ontology
	@if [ "$(strip $(MIR))" = "true" ] && \
			[ "$(strip $(IMP))" = "true" ] && \
			[ "$(strip $(IMP_LARGE))" = "true" ]; then \
		echo "*** mirroring $(strip $(1)) ***"; \
		download_url_base=$(if $(strip $(2)),$(strip $(2)),$(OBOBASE)); \
		echo "url: $$download_url_base/$(strip $(1)).owl"; \
		curl -L $$download_url_base/$(strip $(1)).owl \
			--create-dirs -o $(TMPDIR)/mirror-$(strip $(1)).temp.owl --retry 4 --max-time 200 && \
		$(ROBOT) convert \
			--input $(TMPDIR)/mirror-$(strip $(1)).temp.owl \
			--output $(TMPDIR)/mirror-$(strip $(1)).owl || exit 1; \
		rm -f $(TMPDIR)/mirror-$(strip $(1)).temp.owl; \
		if [ "$(strip $(3))" = "force" ] || \
				! cmp -s $(TMPDIR)/mirror-$(strip $(1)).owl $(MIRRORDIR)/$(strip $(1)).owl ; then \
			echo "Mirrors different or update is forced, !!! UPDATING !!!" && \
			cp $(TMPDIR)/mirror-$(strip $(1)).owl $(MIRRORDIR)/$(strip $(1)).owl; \
		else \
			echo "Mirrors identical, !!! IGNORING !!!"; \
		fi; \
	fi
endef

#### Release Functions ####

# removes obsolete classes (and their descendants) and deprecated entities
# .json and .obo files are regenerated from the corresponding .owl file
# parameters: $(1): file to clean
define remove-obsolete-class
	src=$(1); fmt=owl; \
	case "$$src" in \
		*.json) src="$${src%.json}.owl"; fmt=json ;; \
		*.obo)  src="$${src%.obo}.owl";  fmt=obo ;; \
	esac; \
	echo "\n removing ObsoleteClass from $(1) (input: $$src) \n"; \
	$(ROBOT) \
		remove --input $$src \
			--select "owl:deprecated='true'^^xsd:boolean" \
		remove \
			--select "owl:deprecated='true'" \
		remove \
			--term oboInOwl:ObsoleteClass \
			--select "self descendants" \
		convert --format $$fmt --check false \
			--output $(TMPDIR)/$$(basename $(1)) && \
	mv $(TMPDIR)/$$(basename $(1)) $(1) || exit 1
endef

#### Testing Functions ####

# runs the reasoner over an ontology and discards the output
# parameters: $(1): input ontology; $(2): reasoner (e.g., HERMIT, ELK)
define reason-test
	$(ROBOT) reason --input $(1) --reasoner $(2) --equivalent-classes-allowed asserted-only \
		--exclude-tautologies structural --output test.owl && rm test.owl
endef
