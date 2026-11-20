# OHD Makefile Guide

This guide explains the `make` commands used to test, build, and release the
Oral Health and Disease Ontology (OHD). It is written for people who are new to
`make` and the [Ontology Development Kit (ODK)](https://github.com/INCATools/ontology-development-kit).

For the step-by-step release checklist, see [README.md](README.md).

---

## 1. The basics

### Where to run commands

Run every command from the `src/ontology` directory:

```bash
cd src/ontology
```

### Two ways to run `make`

| How | Command | When to use it |
|-----|---------|----------------|
| Locally | `make <command>` | You have [ROBOT](http://robot.obolibrary.org/) and GNU make installed. |
| In Docker (ODK) | `sh run.sh make <command>` | You do not want to install tools yourself. Requires Docker. This is the same environment GitHub Actions uses. |

The examples below use `make <command>`. If you use Docker, put `sh run.sh` in
front of them (on Windows, use `run.bat make <command>`).

### How `make` works (in one paragraph)

Each command (called a *target*) either runs a task (e.g., `elk_test`) or builds a
file (e.g., `ohd.owl`). A file target lists the files it is built from. `make` only
rebuilds a file when one of those input files has changed, so running the same
command twice usually does nothing the second time. That is normal.

### Useful options

| Option | What it does | Example |
|--------|--------------|---------|
| `-n` | **Dry run.** Prints the commands that would run, without running them. A safe way to see what a command does. | `make -n prepare_release` |
| `-B` | Forces everything the command needs to be rebuilt, even if nothing changed. | `make -B imports/go_import.owl` |
| `IMP=false` | Do not rebuild the import files (`imports/*_import.owl`). | `make IMP=false prepare_release` |
| `MIR=false` | Do not download new copies of other ontologies (mirrors). | `make MIR=false refresh-go` |
| `IMP_LARGE=false` | Skip downloading mirrors (same effect as `MIR=false` for downloads). | `make IMP_LARGE=false refresh-imports` |
| `COMP=false`, `PAT=false` | Skip components / patterns (standard ODK switches). | |

The commands ending in `_fast` (e.g., `prepare_release_fast`, `elk_test_fast`) are
shortcuts for running a command with `IMP=false PAT=false COMP=false MIR=false`.

### Memory

ROBOT needs a lot of memory for OHD. If you see `OutOfMemoryError` or
`Java heap space` when running locally, give Java more memory first:

```bash
export ROBOT_JAVA_ARGS=-Xmx16G
```

(`run.sh` already sets 16 GB for Docker.)

---

## 2. Quick reference: common tasks

| I want to... | Command |
|--------------|---------|
| Check my edits before opening a pull request | `make elk_test_fast` |
| Run the same check GitHub Actions runs | `make elk_test IMP=false PAT=false MIR=false` |
| Check my edits with the HermiT reasoner (slow, thorough) | `make reason_test` |
| Find out why classes are unsatisfiable | `make explain_unsat` |
| Make a release (refreshing imports that are out of date) | `make prepare_release` |
| Make a release without touching imports | `make prepare_release_fast` |
| Update one import (e.g., GO) with the latest version of GO | `make refresh-go` |
| Update one import without downloading a new GO | `make no-mirror-refresh-go` |
| Update all imports | `make refresh-imports` |
| Download the latest copy of an ontology (e.g., GO) | `make mirror-go` |
| Rebuild the subsets (ODFA) | `make all_subsets` |
| Compare the new `ohd.owl` with the last published release | `make release_diff` |
| See all ODK commands | `make help` |

---

## 3. Testing and quality-control commands

| Command | What it does |
|---------|--------------|
| `elk_test` | Runs the main test suite: an ELK reasoning test (`elk_reason_test`), the SPARQL checks (`sparql_test`), the ROBOT report (`robot_reports`), and an OWL 2 DL profile check of `ohd.owl`. **This is what GitHub Actions runs on every pull request.** Note: the profile check first rebuilds `ohd.owl` if it is out of date, and building `ohd.owl` uses HermiT, so this step can take a while. |
| `elk_test_fast` | Same as `elk_test`, but does not rebuild imports or download mirrors. Recommended before every pull request. |
| `elk_reason_test` | Reasons over the ontology with ELK (fast) and fails if there is an inconsistency or an unsatisfiable class. |
| `reason_test` | Same as `elk_reason_test` but uses HermiT. HermiT is slower and more complete; it is the reasoner used to build the release. Run this locally before a release (it can run out of memory on GitHub Actions). |
| `sparql_test` | Runs the SPARQL "violation" checks in `../sparql/*-violation.sparql` (e.g., labels that contain IRIs, bad ID ranges). Results go in `reports/`. |
| `robot_reports` | Runs `robot report` (missing labels, duplicate definitions, etc.). Results go in `reports/`. |
| `custom_reports` | Exports SPARQL reports (term lists, synonyms, xrefs, obsoletes, etc.) to `reports/*.tsv`. |
| `validate_profile_ohd.owl` | Checks that `ohd.owl` is valid OWL 2 DL. |
| `explain_unsat` | Writes an explanation of unsatisfiable classes to `tmp/explain_unsat.md`. |
| `release_diff` | Downloads the currently published `ohd.owl` and writes a diff with your new `ohd.owl` to `reports/release-diff.md`. |

---

## 4. Release commands

| Command | What it does |
|---------|--------------|
| `prepare_release` | Runs the whole release pipeline (details below). |
| `prepare_release_fast` | Same as `prepare_release` but does not rebuild imports, components, or patterns, and does not download mirrors. |
| `all_assets` | Builds every release file but does **not** copy anything to the repository root. Useful for checking a release before running `prepare_release`. |
| `public_release` | (Optional) Uses the GitHub CLI (`gh`) to create a **draft** GitHub release named `vYYYY-MM-DD` and upload the release files. You can also create the release by hand on GitHub. |

### What `prepare_release` does, step by step

1. **Reports.** Runs `custom_reports`.
2. **Builds every release file (`all_assets`):**
   - updates any import files whose inputs changed (see section 5);
   - `ohd-full.owl`: the edit file with all imports merged, reasoned with HermiT;
   - `ohd-base.owl`: like `ohd-full.owl`, but only OHD's own axioms (imported axioms removed);
   - `ohd-non-classified.owl`: imports merged, not reasoned;
   - `ohd.owl`: built from `ohd-full.owl`, with obsolete/deprecated classes removed;
   - `.obo` and `.json` versions of each of these;
   - the subsets (currently ODFA; see section 6).

   While building these files, language tags and OHD's `rdfs:isDefinedBy`
   annotations are removed, and the version IRI and `owl:versionInfo` are set to
   today's date.
3. **ROBOT report** on `ohd.owl`, saved to `reports/ohd-edit.owl-obo-report.tsv`.
   (It does not stop the release if problems are found, so look at the report.)
4. **Copies** the release files to the repository root (`../../`): `ohd*.owl`,
   `ohd*.obo`, `ohd*.json`, and `subsets/odfa.*`.
5. **Cleans up** the copies left in `src/ontology`.

---

## 5. Import and mirror commands

OHD reuses terms from other ontologies. Two kinds of files are involved:

- **Mirrors** (`mirror/<name>.owl`): a full local copy of another ontology, downloaded
  from `http://purl.obolibrary.org/obo/<name>.owl`. Mirrors are not stored in git.
- **Imports** (`imports/<name>_import.owl`): the small part of that ontology OHD
  needs. Imports are built from the mirror plus a list of terms in
  `imports/<name>_terms.txt`. Imports *are* stored in git.

**To add a term from another ontology:** add its IRI to `imports/<name>_terms.txt`,
then run `make refresh-<name>` (or `make no-mirror-refresh-<name>` to keep the
current mirror).

| Command | What it does |
|---------|--------------|
| `refresh-<name>` | Downloads the latest `<name>` (if it has changed) and rebuilds `imports/<name>_import.owl`. Example: `make refresh-go`. |
| `no-mirror-refresh-<name>` | Rebuilds `imports/<name>_import.owl` from the mirror you already have. |
| `refresh-imports` | Downloads the mirrors and rebuilds every import in the `IMPORTS` list in the main `Makefile`. Can be slow. |
| `no-mirror-refresh-imports` | Rebuilds every import without downloading new mirrors. |
| `refresh-imports-excluding-large` | Rebuilds every import without downloading mirrors. |
| `mirror-<name>` | Downloads `<name>` and replaces `mirror/<name>.owl` only if the download is different. |
| `force-mirror-<name>` | Downloads `<name>` and always replaces `mirror/<name>.owl`. |
| `all-mirrors` | Downloads any mirrors that are missing. |
| `force-all-mirrors` | Re-downloads and replaces every (non-manual) mirror. |
| `imports/<name>_import.owl` | Rebuilds one import only if its mirror or terms file changed. |

### How each import is built

The recipes are in `ohd.Makefile`. Each one calls a function defined at the
bottom of that file:

| Function | Method | Imports |
|----------|--------|---------|
| `filter-import` | Keeps only the listed terms and their ancestors | mf, ido, pato, ogms, omrse, uberon (NCBI Taxon terms also removed) |
| `bot-import` | ROBOT "BOT" module | emro, pain |
| `mireot-import` | ROBOT "MIREOT" (listed terms + their parents up to an optional top term) | ro, ohmi, iao, ecto, go |
| `branch-mireot-import` | MIREOT of a whole branch, then of the listed terms | envo, chebi |
| (custom) | Annotation properties only | omo |

Every import has deprecated terms removed, gets `rdfs:isDefinedBy` annotations,
and its version IRI set to `.../ohd/imports/<date>/<name>_import.owl`.

### Manually maintained imports

These are edited by hand and are **not** rebuilt by any command:
`bfo2_classes`, `caro`, `cdt`, `ncbi`, `fma-jaws-teeth`, `fma-lymph`,
`fma-mouth-mucosa`, `fma-tmj`, `fma-tongue` (listed in `MANUAL_IMPORTS` in
`ohd.Makefile`). `obi_import.owl` has no build rule, so it is also effectively frozen.

### ChEBI

ChEBI is very large, so OHD uses the compressed mirror `mirror/chebi.owl.gz`,
which is not downloaded automatically. If a command fails with
`No rule to make target 'mirror/chebi.owl.gz'`, download it:

```bash
curl -L http://purl.obolibrary.org/obo/chebi.owl.gz -o mirror/chebi.owl.gz
```

---

## 6. Subset commands

Subsets are smaller ontologies made from part of OHD. They are defined in
`ohd-subsets.Makefile`. The current subset is:

- **ODFA** (Ontology of Dental care-related Fear, Anxiety, and/or Phobia):
  classes selected by `../sparql/odfa-subset.rq`.

(The ICOP subset has moved to the [pain ontology](https://github.com/uflcod/pain-ontology).)

| Command | What it does |
|---------|--------------|
| `all_subsets` | Builds every subset in `.owl`, `.obo`, `.json`, and `.tsv` into `src/ontology/subsets/`. |
| `subsets/odfa.owl` | Builds only the ODFA OWL file. |

A subset is built from `ohd.owl` by:
1. running the subset's SPARQL query to find its terms;
2. extracting those terms with a ROBOT STAR module;
3. merging in the subset's own metadata (`subsets/odfa-annotations.owl`);
4. setting the subset's IRI, version IRI, and `owl:versionInfo`.

`prepare_release` builds the subsets and copies them to the root `subsets/` folder,
so you do not need to run `all_subsets` separately during a release.

**To add a new subset `xyz`:** create `../sparql/xyz-subset.rq` and
`subsets/xyz-annotations.owl`, add `xyz` to `SUBSETS` in `ohd-subsets.Makefile`,
and copy the `odfa.owl` rule there, replacing `odfa` with `xyz`.

---

## 7. Other editor commands

| Command | What it does |
|---------|--------------|
| `normalize_src` | Re-saves `ohd-edit.owl` in a standard format. Useful before committing if Protégé changed the formatting. |
| `validate_idranges` | Checks `ohd-idranges.owl` is formatted correctly. |
| `update_repo` | Updates the ODK setup from `ohd-odk.yaml`. **Advanced:** this can overwrite the main `Makefile`, so review the changes carefully. |
| `update_docs` | Publishes the documentation website (mkdocs). |
| `clean` | Deletes temporary files, downloaded mirrors, and built release files in `src/ontology`. Mirrors will need to be downloaded again. |
| `help` | Prints the standard ODK command list. |

---

## 8. Known issues

Some commands listed by `make help` do not currently work in OHD:

| Command | Problem | Use instead |
|---------|---------|-------------|
| `test` | Its recipe is commented out in the main `Makefile`, so it runs nothing and reports success. | `elk_test` or `reason_test` |
| `test_fast` | Calls `test`, so it also runs nothing. | `elk_test_fast` |
| `all`, `odkversion` | Fail with `No rule to make target 'odkversion'`. | `all_assets` |
| `show_assets` | Commented out in the main `Makefile`. | `make -n all_assets` |

---

## 9. Where the commands are defined

| File | Contents | Edit it? |
|------|----------|----------|
| `Makefile` | Standard ODK commands (generated by ODK). | **Avoid.** `make update_repo` may overwrite it. |
| `ohd.Makefile` | OHD's commands: tests, release, imports, mirrors, and helper functions. It is loaded after `Makefile` and overrides it. | Yes, put OHD changes here. |
| `ohd-subsets.Makefile` | The subset commands and the `extract-subset` function. | Yes, for subsets. |
