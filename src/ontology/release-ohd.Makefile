MAKEFILE=					release-ohd.Makefile
URIBASE=					http://purl.obolibrary.org/obo
ONT=						ohd
ONTBASE=                    $(URIBASE)/$(ONT)
EDIT_FORMAT=                owl
SRC=                        $(ONT)-edit.$(EDIT_FORMAT)
RELEASEDIR=                 ../..
MIRRORDIR=                  mirror
IMPORTDIR=                  imports
SUBSETDIR=                  subsets
TMPDIR=                     tmp
SCRIPTSDIR=                 ../scripts
SPARQLDIR =                 ../sparql
COMPONENTSDIR =             components
CATALOG=                    catalog-v001.xml
ROBOT=                      robot --catalog $(CATALOG)
RELEASEDIR=                 ../..
TODAY ?=                    $(shell date +%Y-%m-%d)
VERSION=                    $(TODAY)
ANNOTATE_ONTOLOGY_VERSION= 	annotate -V $(ONTBASE)/releases/$(VERSION)/$@ --annotation owl:versionInfo $(VERSION)
OTHER_SRC =
RELEASE_ARTEFACTS=          $(ONT).owl $(ONT)-base.owl $(ONT)-non-classified.owl

$(TMPDIR) $(MIRRORDIR) $(IMPORTDIR):
	mkdir -p $@

# ----------------------------------------
# Release artifacts
# ----------------------------------------

.PHONY: all release clean
all: $(RELEASE_ARTEFACTS)

release: $(RELEASE_ARTEFACTS)
	@echo "\n** releasing $^ **"
	cp $^ $(RELEASEDIR)

clean:
	rm -f $(RELEASE_ARTEFACTS)

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
	    
$(ONT)-base.owl: $(SRC)
	$(ROBOT) \
		remove --input $< --select imports --trim false \
	    annotate \
            --link-annotation http://purl.org/dc/elements/1.1/type http://purl.obolibrary.org/obo/IAO_8000001 \
	        --ontology-iri $(ONTBASE)/$@ \
	        --version-iri $(ONTBASE)/releases/$(VERSION)/$@ \
	        --annotation owl:versionInfo $(VERSION) \
	    --output $@.tmp.owl && mv $@.tmp.owl $@

$(ONT)-non-classified.owl: $(SRC)
	$(ROBOT) merge --input $< \
	    annotate \
            --ontology-iri $(ONTBASE)/$@ \
            --version-iri $(ONTBASE)/releases/$(VERSION)/$@ \
	        --annotation owl:versionInfo $(VERSION) \
	    --output $@.tmp.owl && mv $@.tmp.owl $@

# ----------------------------------------
# Ontology imports
# ----------------------------------------
.PHONY: refresh-imports no-mirror-refresh-imports

IMP=true # Global parameter to bypass import generation
MIR=true # Global parameter to bypass mirror generation
IMP_LARGE=true # Global parameter to bypass handling of large imports

IMPORTS = omo ro iao caro fma ecto obi omrse ogms nbo pato

IMPORT_ROOTS = $(patsubst %, $(IMPORTDIR)/%_import, $(IMPORTS))
IMPORT_OWL_FILES = $(foreach n,$(IMPORT_ROOTS), $(n).owl)
IMPORT_FILES = $(IMPORT_OWL_FILES)

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

# $(IMPORTDIR)/obi_import_test.owl: $(MIRRORDIR)/obi.owl.gz
# 	$(ROBOT) \
#         extract \
#             --input $< \
#             --method STAR \
#             --term-file $(IMPORTDIR)/obi_terms_test.txt \
#         filter \
#             --term OBI:0000274 \
#             --term OBI:0000070 \
#             --term OBI:0200000 \
#             --term OBI:0000067 \
#             --term OBI:0000319 \
#             --term OBI:0000093 \
#             --term OBI:0000444\
#             --term OBI:0000047 \
#             --term OBI:0000441 \
#             --term OBI:0000806 \
#             --term OBI:0000434 \
#             --select "annotations self parents" \
#             --signature false \
#             --trim false \
#         remove \
#             --select "owl:deprecated='true'^^xsd:boolean" \
#         annotate \
#             --annotate-defined-by true \
#             --annotate-derived-from true \
#             --ontology-iri $(URIBASE)/$(ONT)/$@ \
#         --output $@.tmp.owl && mv $@.tmp.owl $@

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

# ----------------------------------------
# Ontology mirrors
# ----------------------------------------
.PHONY: all-mirrors zip-mirrors download-mirrors

all-mirrors: download-mirrors zip-mirrors

zip-mirrors:
	@echo "*** zipping $(patsubst %, $(MIRRORDIR)/%.owl.gz, $(IMPORTS)) ***" # testing
	make -f $(MAKEFILE) $(patsubst %, $(MIRRORDIR)/%.owl.gz, $(IMPORTS)) -B

download-mirrors:
	@echo "*** initiate download $(patsubst %, $(MIRRORDIR)/%.owl, $(IMPORTS)) ***" # testing
	make -f $(MAKEFILE) $(patsubst %, mirror-%, $(IMPORTS))

$(MIRRORDIR)/%.owl: | $(MIRRORDIR)
	curl -L $(URIBASE)/$*.owl --create-dirs -o $@ --retry 4 --max-time 200

mirror-%:
	make -f $(MAKEFILE) $(MIRRORDIR)/$*.owl -B

# --- gzip ontology mirrors ---

$(MIRRORDIR)/omo.owl.gz:
	gzip -fk $(MIRRORDIR)/omo.owl

$(MIRRORDIR)/ro.owl.gz: 
	gzip -fk $(MIRRORDIR)/ro.owl

$(MIRRORDIR)/iao.owl.gz: 
	gzip -fk $(MIRRORDIR)/iao.owl

$(MIRRORDIR)/caro.owl.gz: 
	gzip -fk $(MIRRORDIR)/caro.owl

$(MIRRORDIR)/fma.owl.gz: 
	gzip -fk $(MIRRORDIR)/fma.owl

$(MIRRORDIR)/ecto.owl.gz: 
	gzip -fk $(MIRRORDIR)/ecto.owl

$(MIRRORDIR)/obi.owl.gz: 
	gzip -fk $(MIRRORDIR)/obi.owl

$(MIRRORDIR)/omrse.owl.gz: 
	gzip -fk $(MIRRORDIR)/omrse.owl

$(MIRRORDIR)/ogms.owl.gz: 
	gzip -fk $(MIRRORDIR)/ogms.owl

$(MIRRORDIR)/nbo.owl.gz: 
	gzip -fk $(MIRRORDIR)/nbo.owl

$(MIRRORDIR)/pato.owl.gz: 
	gzip -fk $(MIRRORDIR)/pato.owl