# Repository structure

The main kinds of files in the repository:

1. Release files
2. Imports
3. [Components](#components)

## Release files
Release file are the file that are considered part of the official ontology release and to be used by the community. A detailed description of the release artefacts can be found [here](https://github.com/INCATools/ontology-development-kit/blob/master/docs/ReleaseArtefacts.md).

## Imports
Imports are subsets of external ontologies that contain terms and axioms you would like to re-use in your ontology. These are considered "external", like dependencies in software development, and are not included in your "base" product, which is the [release artefact](https://github.com/INCATools/ontology-development-kit/blob/master/docs/ReleaseArtefacts.md) which contains only those axioms that you personally maintain.

These are the current imports in OHD

| Import | URL | Type |
| ------ | --- | ---- |
| ruttenberg-regions | http://purl.obolibrary.org/obo/bfo/2010-05-25/ruttenberg-regions.owl | None |
| ruttenberg-relations | http://purl.obolibrary.org/obo/bfo/2010-05-25/ruttenberg-relations.owl | None |
| caro | http://purl.obolibrary.org/obo/caro.owl | None |
| cdt | http://purl.obolibrary.org/obo/cdt.owl | None |
| ecto | http://purl.obolibrary.org/obo/ecto.owl | None |
| fma-jaws-teeth | http://purl.obolibrary.org/obo/fma-jaws-teeth.owl | None |
| fma-lymph | http://purl.obolibrary.org/obo/fma-lymph.owl | None |
| fma-mouth-mucosa | http://purl.obolibrary.org/obo/fma-mouth-mucosa.owl | None |
| fma-tmj | http://purl.obolibrary.org/obo/fma-tmj.owl | None |
| fma-tongue | http://purl.obolibrary.org/obo/fma-tongue.owl | None |
| iao | http://purl.obolibrary.org/obo/iao.owl | None |
| ncbi | http://purl.obolibrary.org/obo/ncbi.owl | None |
| obi | http://purl.obolibrary.org/obo/obi.owl | None |
| ogms | http://purl.obolibrary.org/obo/ogms.owl | None |
| omrse | http://purl.obolibrary.org/obo/omrse.owl | None |
| ontology-metadata | http://purl.obolibrary.org/obo/ontology-metadata.owl | None |
| ro | http://purl.obolibrary.org/obo/ro.owl | None |
| omo | http://purl.obolibrary.org/obo/omo/releases/2023-06-09/omo.owl | None |
| ro | http://purl.obolibrary.org/obo/ro/releases/2023-02-22/ro.owl | None |

## Components
Components, in contrast to imports, are considered full members of the ontology. This means that any axiom in a component is also included in the ontology base - which means it is considered _native_ to the ontology. While this sounds complicated, consider this: conceptually, no component should be part of more than one ontology. If that seems to be the case, we are most likely talking about an import. Components are often not needed for ontologies, but there are some use cases:

1. There is an automated process that generates and re-generates a part of the ontology
2. A part of the ontology is managed in ROBOT templates
3. The expressivity of the component is higher than the format of the edit file. For example, people still choose to manage their ontology in OBO format (they should not) missing out on a lot of owl features. They may choose to manage logic that is beyond OBO in a specific OWL component.

These are the components in OHD

| Filename | URL |
| -------- | --- |
| dental_material.owl | None |
| dental_restoration.owl | None |
