# Maintaining the NMDC Ontology

## Robot and Protege

The NMDC Ontology products and subsets are built using [robot](http://robot.obolibrary.org/). Thus, you will need to have [robot](http://robot.obolibrary.org/) installed on your system.

Often, you will need to view the ontology products. For this, it is best use the [Protege ontology editor](https://protege.stanford.edu/).

## Editors' version

The top-level ontology products and subsets are built using a release process (see **Updating Ontology Products and Subsets**). Changes made directly to the ontology, such as editing descriptions and managing imports, are made to the editors of this ontology: [src/ontology/nmdco-edit.owl](src/ontology/nmdco-edit.owl).

## Tracking changes to the NMDC Ontology

In order to track changes made to NMDC Ontology, it is best maintained by following these steps:
1. Submit an [issue](https://github.com/microbiomedata/nmdc-ontology/issues) detailing problem.
2. Create a branch to address this issue that uses the same number as the issue tracker. For example, if the issue is `#50` in the issue tracker, name the branch `issue-50`. This allows other developers to easily know which branch needs to be checked out in order to contribute.
3. Create a pull request that fixes the issue. If possible, create a draft (or WIP) branch early in the process.
4. Merge pull request once all the necessary changes have been made. If needed, tag other developers to review pull request. 
5. Delete the issue branch (e.g., branch `issue-50`).

### Updating Ontology Products and Subsets

From time to time, the [imports](src/ontology/imports/) used by the NMDCO will need be updated and new products (i.e., ontology products and subsets) released. The general process for updating the products is as follows:
1. Create an issue, branch, and pull request using the steps outlined in **Tracking changes to the NMDC Ontology** section.
2. Navigate to the [src/ontology directory](src/ontology/).
3. Excute `make clean`. This removes all imports, ontologies, and subsets from the [src/ontology directory](src/ontology/). Note, the top-level ontology products and subsets still exist; only the files in the [src/ontology directory](src/ontology/) were removed.
4. Execute `make all`. This will create new onotlogy products and subsets in the [src/ontology directory](src/ontology/).
5. Review the new onotlogy products and subsets.
6. Once you are satisfied with the new ontology products and subsets, execute `make release`. This will copy the new products to the top-level directory.
7. Finally, push your changes to the repistory, make a pull request, and merge.


