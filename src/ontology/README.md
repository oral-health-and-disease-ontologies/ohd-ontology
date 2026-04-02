# Ontology source

This directory contains the files for editing the Oral Health and Disease Ontology (OHD).  

## Editing the OHD

To make changes to the OHD, edit `ohd-edit.owl`. **DO NOT** edit the `ohd.owl` file directly. The `ohd.owl` file is produced during the release process.

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
   
   The OHD ontology generates subsets automatically using the **OHD subset extraction Makefile**, which builds ontology subsets based on class annotations:

   - **Ontology of Dental care-related Fear, Anxiety, and/or Phobia (ODFA)**: classes annotated with `in_subset "dental fear and anxiety"`.
   - **International Classification of Orofacial Pain (ICOP)**: classes annotated with `in_subset "ICOP"`.
   - Subset-specific annotations are included via `odfa-annotations.owl` and `icop-annotations.owl`.
   - Subsets are generated in multiple formats (`.owl`, `.tsv`, `.obo`, `.json`) by:  
      1. Querying the main ontology for subset-specific terms (`odfa-subset.rq` / `icop-subset.rq`).
      2. Extracting the relevant terms using the BOT method.
      3. Merging in subset-specific annotations.
      4. Annotating the ontology IRI, version IRI, and `owl:versionInfo`.  
   
   After preparing and merging an OHD release to `main`:

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

   Run a Hermit reasoner (if possible) locally to check-in on ohd.owl

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
   - Set the **tag** name: ```vYYYY-MM-DD```
   - Set the **release title**: ```Release: YYYY-MM-DD```
   - Add **release notes**.
   - Mark it as the **latest release**.
   - Click **Publish Release**.