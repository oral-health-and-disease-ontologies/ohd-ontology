# Ontology source

This directory contains the files for editing the Oral Health and Disease Ontology (OHD).  

## Editing the OHD

To make changes to the OHD, edit `ohd-edit.owl`. **DO NOT** edit the `ohd.owl` file directly. The `ohd.owl` file is produced during the release process.

## Releasing OHD

To make an OHD release, run the shell script `release-ohd.sh`, or run the command:
```
make -f release-ohd.Makefile release
```
 