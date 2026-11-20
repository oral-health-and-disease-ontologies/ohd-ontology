# Ontology source

This directory contains the files for editing the Oral Health and Disease Ontology (OHD).

For an explanation of every `make` command used below, see [README-MAKEFILE.md](README-MAKEFILE.md).

## Editing the OHD

To make changes to the OHD, edit `ohd-edit.owl`. **DO NOT** edit the `ohd.owl` file directly. The `ohd.owl` file is produced during the release process.

To use terms from another ontology (e.g., GO), add the term IRIs to `imports/<name>_terms.txt` (e.g., `imports/go_terms.txt`) and rebuild the import:

```bash
make refresh-go
```

### Checking your edits

Before opening a pull request, run the same tests that GitHub Actions runs:

```bash
make elk_test_fast
```

All `make` commands are run from `src/ontology`. If you do not have ROBOT installed locally, run them in Docker by prefixing them with `sh run.sh` (e.g., `sh run.sh make elk_test_fast`).

## Releasing OHD

### Steps for Creating a Release

1. Create a New Branch for the Release

   Use the current date as the branch name:

   ```bash
   git checkout main
   git pull
   git checkout -b release-YYYY-MM-DD
   ```

2. Test the Ontology with HermiT (recommended)

   The release is built with the HermiT reasoner. HermiT is too memory-hungry for GitHub Actions, so run it locally first:

   ```bash
   cd src/ontology
   make reason_test
   ```

   If it reports unsatisfiable classes, `make explain_unsat` writes an explanation to `tmp/explain_unsat.md`.

3. Run the Release Preparation Command

   From `src/ontology`:

   ```bash
   make prepare_release
   ```

   This command:
   - Rebuilds any imports whose term files or mirrors have changed.
   - Builds the release products: `ohd.owl`, `ohd-base.owl`, `ohd-full.owl`, and `ohd-non-classified.owl`, each also as `.obo` and `.json`.
   - Removes obsolete classes from `ohd.owl`, and sets the version IRIs and `owl:versionInfo` to today's date.
   - Builds the subsets (see step 4).
   - Writes a ROBOT report to `reports/ohd-edit.owl-obo-report.tsv`. The report does not stop the release, so look it over.
   - Copies the release products to the repository root and the subsets to the root `subsets/` directory.

   To run the release without rebuilding imports or downloading mirrors, use:

   ```bash
   make prepare_release_fast
   ```

   If the command fails with `No rule to make target 'mirror/chebi.owl.gz'`, download the ChEBI mirror and run the command again:

   ```bash
   curl -L http://purl.obolibrary.org/obo/chebi.owl.gz -o mirror/chebi.owl.gz
   ```

4. Check the Subsets

   The subsets are built and copied automatically by `make prepare_release`. You do not need to build or copy them by hand. The subset build is defined in `ohd-subsets.Makefile`. The current subset is:

   - **Ontology of Dental care-related Fear, Anxiety, and/or Phobia (ODFA)**: classes selected by `../sparql/odfa-subset.rq`.

   Note: the International Classification of Orofacial Pain (ICOP) subset is no longer part of OHD; it has moved to the [pain ontology](https://github.com/uflcod/pain-ontology).

   Each subset is built by:
   1. Querying `ohd.owl` for the subset's terms (`odfa-subset.rq`).
   2. Extracting those terms using a ROBOT STAR module.
   3. Merging in the subset-specific annotations (`subsets/odfa-annotations.owl`).
   4. Setting the ontology IRI, version IRI, and `owl:versionInfo`.

   Check that the root `subsets/` directory contains updated `odfa.owl`, `odfa.obo`, `odfa.json`, and `odfa.tsv`, and that their metadata (version IRI, release date, license) is correct.

   To rebuild the subsets on their own (without making a release), run `make all_subsets`. This writes them to `src/ontology/subsets/` only.

5. Review and Commit the Generated Files

   Inspect the release files. Changes may be minimal (typically just version updates if no content was added). To see what changed compared to the last published release, run `make release_diff` before step 3 and read `reports/release-diff.md`.

   Then, from the repository root, commit the release:

   ```bash
   git status
   git add ohd*.owl ohd*.obo ohd*.json subsets/
   git commit -am "Release YYYY-MM-DD"
   ```

6. Push the Release Branch

   ```bash
   git push origin release-YYYY-MM-DD
   ```

7. Create a Pull Request

   - Go to GitHub and open a pull request from the release branch into `main`.
   - Use a title like: `Release YYYY-MM-DD`.
   - Wait for the automated checks (`elk_test`) to pass.
   - Merge the PR once checks pass.

8. Draft the GitHub Release

   After merging to `main`:

   - Go to the **Releases** tab in the GitHub UI.
   - Click **Draft a new release**.
   - Set the **tag** name: `vYYYY-MM-DD`
   - Set the **release title**: `Release: YYYY-MM-DD`
   - Add **release notes**, including a brief summary of any subset updates.
   - Mark it as the **latest release**.
   - Click **Publish Release**.
