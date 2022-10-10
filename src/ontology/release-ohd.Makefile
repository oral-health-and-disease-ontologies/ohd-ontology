URIBASE=					http://purl.obolibrary.org/obo
ONT=						ohd
ONTBASE=                    $(URIBASE)/$(ONT)
EDIT_FORMAT=                owl
SRC=                        $(ONT)-edit.$(EDIT_FORMAT)
RELEASEDIR=                 ../..
MIRRORDIR=                  mirror
IMPORTDIR=                  imports
SUBSETDIR=                  subsets
CATALOG=                    catalog-v001.xml
ROBOT=                      robot --catalog $(CATALOG)
RELEASEDIR=                 ../..
TODAY ?=                    $(shell date +%Y-%m-%d)
VERSION=                    $(TODAY)
ANNOTATE_ONTOLOGY_VERSION= 	annotate -V $(ONTBASE)/releases/$(VERSION)/$@ --annotation owl:versionInfo $(VERSION)
OTHER_SRC =
RELEASE_ARTEFACTS=          $(ONT).owl $(ONT)-base.owl

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
        annotate --link-annotation http://purl.org/dc/elements/1.1/type http://purl.obolibrary.org/obo/IAO_8000001 \
        --ontology-iri $(ONTBASE)/$@ $(ANNOTATE_ONTOLOGY_VERSION) \
        --output $@.tmp.owl && mv $@.tmp.owl $@