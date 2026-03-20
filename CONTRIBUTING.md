# Contributing to the Oral Health and Disease Ontology (OHD) Ontology

## Robot and Protégé

The OHD Ontology products and subsets are built using [ROBOT](http://robot.obolibrary.org/). Thus, you will need to
have [ROBOT](http://robot.obolibrary.org/) installed on your system.

Often, you will need to view the ontology products. For this, it is best to use
the [Protégé ontology editor](https://protege.stanford.edu/) (
see [Configuring Protégé for OHD contributions](#configuring-protégé-for-ohd-contributions)).

## Editor's version

The top-level ontology products and subsets are built using a release process (
see [Updating Ontology Products and Subsets](#updating-ontology-products-and-subsets)). Changes made directly to the
ontology, such as editing descriptions and managing imports, are made to the editors of this
ontology: [src/ontology/ohd-edit.owl](src/ontology/ohd-edit.owl).

## Tracking Changes to the OHD Ontology

To track changes made to the OHD Ontology, it is best maintained by following these steps:

1. Submit an [issue](https://github.com/oral-health-and-disease-ontologies/ohd-ontology/issues) detailing the problem.
2. Create a branch to address this issue that uses the same number as the issue tracker. For example, if the issue
   is `#50` in the issue tracker, name the branch `issue-50`. This allows other developers to easily know which branch
   needs to be checked out to contribute.
3. Create a pull request that fixes the issue. If possible, create a draft (or WIP) branch early in the process.
4. Merge pull request once all the necessary changes have been made. If needed, tag other developers to review the pull
   request.
5. Delete the issue branch (e.g., branch `issue-50`).

### Updating Ontology Products and Subsets

From time to time, the [imports](src/ontology/imports/) used by the OHD Ontology will need to be updated and new
products (i.e., ontology products and subsets) released. The general process for updating the products is as follows:

1. Create an issue, branch, and pull request using the steps outlined
   in [Tracking Changes to the OHD Ontology](#tracking-changes-to-the-ohd-ontology) section.
2. Navigate to the [src/ontology](src/ontology/) directory.
3. Execute `make clean`. This removes all imports, ontologies, and subsets from the [src/ontology](src/ontology/)
   directory. Note, the top-level ontology products and subsets still exist; only the files in
   the [src/ontology](src/ontology/) directory were removed.
4. Execute `make all`. This will create new ontology products and subsets in the [src/ontology](src/ontology/)
   directory.
5. Review the new ontology products and subsets.
6. Once you are satisfied with the new ontology products and subsets, execute `make release`. This will copy the new
   products to the top-level directory.
7. Finally, push your changes to the repository, make a pull request, and merge.

## Releasing OHD

### Steps for Creating a Release

1. Create a New Branch for the Release

   Use the current date or a descriptive tag as the branch name:

     ```bash
      git checkout -b release-YYYY-MM-DD
      ```
2. Run the Release Preparation Command

   Execute the following from the src/ontology:

      ```bash
      make prepare_release
      ```

   This command:
      - Runs validation and quality checks.
      - Generates the release products, including:
      - `ohd.owl`
      - `ohd.obo`
      - `ohd.json`
      - `ohd-base.owl`, etc.
      - Updates the version IRIs and metadata.
   
   3. Generating Ontology Subsets (ODFA and ICOP)
   
   The OHD ontology generates subsets automatically using the **OHD subset extraction Makefile**, which builds ontology subsets based on class annotations:

   - **Ontology of Dental care-related Fear, Anxiety, and/or Phobia (ODFA)**: classes annotated with `in_subset "dental fear and anxiety"`.
   - **International Classification of Orofacial Pain (ICOP)**: classes annotated with `in_subset "ICOP"`.
   - Subset-specific annotations are included via `odfa-annotations.owl` and `icop-annotations.owl`.
   - Subsets are generated in multiple formats (`.owl`, `.tsv`, `.obo`, `.json`) by:  
      1. Querying the main ontology for subset-specific terms (`odfa-subset.rq` / `icop-subset.rq`).
      2. Extracting the relevant terms using the BOT method.
      3. Merging in subset-specific annotations.
      4. Annotating the ontology IRI, version IRI, and `owl:versionInfo`.  
   
   After preparing and merging a PHASES release to `main`:

     - Navigate to the ontology source directory:
     ```bash
     cd src/ontology
     ```
     
   - Clean previously generated ontology products and subsets:
     ```bash
     make clean
     ```
     
   - Generate all ontology products and subsets:
     ```bash
     make all_subsets
     ```
     
   - Prepare the official **subset** release:
     ```bash
     make prepare_release
     ```
   
   - Verify that ontology subsets (e.g., **ODFA** and **ICOP**) were generated in the **subset directory** `$(SUBSETDIR)` (default: `src/ontology/subsets`), including:
     - `odfa.owl`
     - `icop.owl`
     - `odfa-annotations.owl`
     - `icop-annotations.owl`
     - `*.obo` 
     - `*.json`
     - `*.tsv`

   - Copy the updated subsets to the root/subsets directory:
     ```bash
     cp src/ontology/subsets/odfa.* subsets/            
     cp src/ontology/subsets/icop.* subsets/
     ```
       
   - Review each subset to confirm:
     - Correct metadata (version IRI, release date, license).
     - Include a brief summary of subset updates in the GitHub release notes when drafting the release.
     

3. Review and Commit the Generated Files

   Run a Hermit reasoner (if possible) locally to check-in on phases.owl

   Inspect the release files. Changes may be minimal (typically just version updates if no content was added).

   Then commit the release:

   ```copy these files to the root
   git commit -am "release for YYYY-MM-DD"
   ```

4. Push the Release Branch

   ```bash
   git push origin release-YYYY-MM-DD
   ```

5. Create a Pull Request

   - Go to GitHub and open a pull request from the release branch into `main`.
   - Use a title like:  `Release: YYYY-MM-DD`.
   - Wait for automated checks (if configured).
   - Merge the PR once checks pass.

6. Draft the GitHub Release

   After merging to `main`:

   - Go to the **Releases** tab in the GitHub UI.
   - Click **Draft a new release**
   - Set the **tag** name: ```text vYYYY-MM-DD```
   - Set the **release title**: ```text Release: YYYY-MM-DD```
   - Add **release notes**.
   - Mark it as the **latest release**.
   - Click **Publish Release**.
  

---

### Notes on Release Artifacts

The `make prepare-release` command generates several formats:

- `.obo`, `.owl`, `.json`, `.tsv`: Standard ontology formats.
- `*-base` versions: Contain only locally defined classes (no imports).
- These are the canonical files used by portals like OLS or OntoBee.

## Configuring Protégé for OHD Contributions

### Rationale

When a new entity is created using Protégé the entities are created using the default settings of Protégé. For example,
using the default settings when a new entity called 'test' is created the IRI for the entity may look like this.

`http://www.semanticweb.org/user/ontologies/2023/6/untitled-ontology-43#test`

Ideally, we want the new entity created in the OHD ontology to look similar to the IRI below based on OBO Foundry
principles:

`http://purl.obolibrary.org/obo/OHD_0000024`

### Prerequisites

#### Protégé

If you do not have installed Protégé ontology editor on your machine, please download and install it from
this [link](https://protege.stanford.edu/).

#### Create ORCID identifier

If you do not have an **ORCID ID** please create one by [registering here](https://orcid.org/). ORCID ID is
a [free-of-charge identifier]((https://info.orcid.org/what-is-orcid/)) for researchers.

### Update Preferences

#### Where can I find Protégé preferences?

- Open Protégé.
- Go to **File > Preferences**.

#### Update user details in Protégé

- Go to Protégé **Preferences**.
- Click on the **User details** tab.

  <img src="src/images/User%20details.png" width="893" height="873"/>
  
- Enable **Use supplied user name**.
- Enter your full name in the field indicated.
- Enable **Use Git user name when available**.
- Enter your ORCID ID in the field for **ORCID**.
- Click **OK** to save.


#### Update configuration for new entities

- Go to Protégé **Preferences**.
- Click the **New entities** tab.

  <img src="src/images/New%20entities.png" width="893" height="873"/>

- Update **Entity IRI**.
    - For **Start with:** enable 'Specified IRI' and enter the following IRI in the field:
        - `http://purl.obolibrary.org/obo`
    - For **Followed by:** enable the forward slash option (/).
    - For **End with:** enable 'Auto-generated ID'.
- Update **Entity Label**.
    - Enable **Same as label renderer**.
- Update **Auto-generated ID**.
    - Enable **Numeric (Iterative)**.
    - For **Prefix:** enter **OHD_** in the field provided.
    - For **Suffix:** leave this blank.
    - For **Digit count:** enter 7.
    - For an ID range, **Start:** and **End:** contact the OHD group if one has not been assigned.
    - Enable **Remember last ID between Protégé sessions**.
- Click **OK** to save.


#### Update configuration for new entities metadata

- Go to Protégé **Preferences**.
- Click the **New entities metadata** tab.

  <img src="src/images/New%20entities%20metadata.png" width="892" height="748"/>
  
- Enable **Annotate new entities with creator (user)**.
- For **Creator property** enter the following IRI the field:
    - `http://purl.org/dc/terms/contributor`
- For **Creator value** enable **Use ORCID**.
- Enable **Annotate new entities with creation date/time**.
- For **Date property** enter the following IRI the field:
    - `http://purl.org/dc/terms/created`
- For **Date property** enable **ISO-8601**.
- Click **OK** to save.

