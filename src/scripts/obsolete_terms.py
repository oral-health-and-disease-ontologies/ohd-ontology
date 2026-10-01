#!/usr/bin/env python3
"""
Obsolete OHD terms that have been replaced by imported terms.

Reads a CSV file with the columns `pain_term` and `ohd_term` (full IRIs) and,
for each ohd_term, edits the functional-syntax ontology file so that the term:
  - is annotated with owl:deprecated "true"^^xsd:boolean
  - is annotated with 'has obsolescence reason' (IAO:0000231) 'term imported' (IAO:0000228)
  - is annotated with 'term replaced by' (IAO:0100001) the corresponding pain_term
  - has all of its asserted SubClassOf axioms removed
  - is made a subclass of oboInOwl:ObsoleteClass
  - has its label prefixed with "obsolete " and its definition prefixed with "OBSOLETE. "
  - has its oboInOwl:inSubset and 'ICOP number' (OHD:0008081) annotations removed

The file is edited as text so that the rest of the file is left untouched and the
output stays in OWL functional syntax. The output file must have an .owl extension.

Usage (from src/ontology):
    python3 ../scripts/obsolete_terms.py tmp/icop-pain-ohd-terms.csv ohd-edit.owl
    python3 ../scripts/obsolete_terms.py tmp/icop-pain-ohd-terms.csv ohd-edit.owl -o ohd-edit-new.owl
"""
import argparse
import csv
import re
import sys

OBO = "http://purl.obolibrary.org/obo/"
OBSOLETE_CLASS = "<http://www.geneontology.org/formats/oboInOwl#ObsoleteClass>"
IN_SUBSET = "<http://www.geneontology.org/formats/oboInOwl#inSubset>"
ICOP_NUMBER = "obo:OHD_0008081"
# annotation properties whose assertions are removed from obsoleted terms
REMOVED_PROPERTIES = {IN_SUBSET, ICOP_NUMBER}
LABEL = "rdfs:label"
DEFINITION = "obo:IAO_0000115"
# literal prefixes added to obsoleted terms (property -> prefix)
PREFIXES = {LABEL: "obsolete ", DEFINITION: "OBSOLETE. "}


def curie(iri):
    """Return the obo: CURIE for an OBO IRI, otherwise the IRI in angle brackets."""
    iri = iri.strip()
    if iri.startswith(OBO) and "/" not in iri[len(OBO):] and "#" not in iri[len(OBO):]:
        return "obo:" + iri[len(OBO):]
    return "<" + iri + ">"


def split_args(body):
    """Split the arguments of a functional-syntax expression on top-level whitespace."""
    args, depth, in_str, cur, i = [], 0, False, "", 0
    while i < len(body):
        c = body[i]
        if in_str:
            cur += c
            if c == "\\" and i + 1 < len(body):
                cur += body[i + 1]
                i += 1
            elif c == '"':
                in_str = False
        elif c == '"':
            in_str = True
            cur += c
        elif c == "(":
            depth += 1
            cur += c
        elif c == ")":
            depth -= 1
            cur += c
        elif c.isspace() and depth == 0:
            if cur:
                args.append(cur)
            cur = ""
        else:
            cur += c
        i += 1
    if cur:
        args.append(cur)
    return args


def subclass_subject(line):
    """If line is a SubClassOf axiom, return its subclass expression, else None."""
    line = line.strip()
    if not (line.startswith("SubClassOf(") and line.endswith(")")):
        return None
    args = [a for a in split_args(line[len("SubClassOf("):-1]) if not a.startswith("Annotation(")]
    return args[0] if args else None


def annotation_parts(line):
    """If line is an AnnotationAssertion, return (all args, property, subject, value index), else None."""
    line = line.strip()
    if not (line.startswith("AnnotationAssertion(") and line.endswith(")")):
        return None
    args = split_args(line[len("AnnotationAssertion("):-1])
    idx = [i for i, a in enumerate(args) if not a.startswith("Annotation(")]
    if len(idx) != 3:
        return None
    return args, args[idx[0]], args[idx[1]], idx[2]


def prefix_literal(literal, prefix):
    """Add prefix to the lexical form of a quoted literal, unless already present."""
    if not literal.startswith('"') or literal[1:].startswith(prefix):
        return literal
    return '"' + prefix + literal[1:]


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("csv_file", help="CSV file with pain_term and ohd_term columns")
    parser.add_argument("ontology", help="functional-syntax ontology file (e.g., ohd-edit.owl)")
    parser.add_argument("-o", "--output", help="output file (default: overwrite the input ontology)")
    args = parser.parse_args()
    if not (args.output or args.ontology).endswith(".owl"):
        parser.error("output file must have an .owl extension")

    with open(args.csv_file, newline="") as f:
        replacements = {}
        for row in csv.DictReader(f):
            ohd, pain = (row.get("ohd_term") or "").strip(), (row.get("pain_term") or "").strip()
            if ohd and pain:
                replacements[curie(ohd)] = curie(pain)

    with open(args.ontology) as f:
        lines = f.read().split("\n")

    deprecated_re = re.compile(r"AnnotationAssertion\((?:Annotation\(.*\) )?owl:deprecated (\S+) ")
    already_deprecated = {m.group(1) for line in lines for m in [deprecated_re.match(line)] if m}

    # Find, for each term, the lines to remove (SubClassOf, inSubset, ICOP number), the
    # label/definition lines to rewrite, and the last line of its axioms.
    removed = {t: [] for t in replacements}
    rewritten = {t: {} for t in replacements}
    last_line = {}
    for i, line in enumerate(lines):
        subj = subclass_subject(line)
        if subj in replacements:
            removed[subj].append(i)
            last_line[subj] = i
            continue
        m = re.match(r"# Class: (\S+) \((?!obsolete )", line)
        if m and m.group(1) in replacements:
            rewritten[m.group(1)][i] = line[:m.end()] + "obsolete " + line[m.end():]
            continue
        parts = annotation_parts(line)
        if parts and parts[2] in replacements:
            a, prop, subj, vi = parts
            last_line[subj] = i
            if prop in REMOVED_PROPERTIES:
                removed[subj].append(i)
            elif prop in PREFIXES:
                a = list(a)
                a[vi] = prefix_literal(a[vi], PREFIXES[prop])
                rewritten[subj][i] = "AnnotationAssertion(" + " ".join(a) + ")"

    to_remove, rewrites, inserts, skipped, missing = set(), {}, {}, [], []
    for term, replacement in replacements.items():
        if term in already_deprecated:
            skipped.append(term)
            continue
        if term not in last_line:
            missing.append(term)
            continue
        to_remove.update(removed[term])
        rewrites.update(rewritten[term])
        inserts[last_line[term]] = [
            f'AnnotationAssertion(owl:deprecated {term} "true"^^xsd:boolean)',
            f"AnnotationAssertion(obo:IAO_0000231 {term} obo:IAO_0000228)",
            f"AnnotationAssertion(obo:IAO_0100001 {term} {replacement})",
            f"SubClassOf({term} {OBSOLETE_CLASS})",
        ]

    out = []
    for i, line in enumerate(lines):
        if i not in to_remove:
            out.append(rewrites.get(i, line))
        out.extend(inserts.get(i, []))

    with open(args.output or args.ontology, "w") as f:
        f.write("\n".join(out))

    print(f"Obsoleted {len(inserts)} terms; removed {len(to_remove)} SubClassOf/inSubset/ICOP number axioms; "
          f"prefixed {len(rewrites)} labels/definitions/class comments.", file=sys.stderr)
    if skipped:
        print(f"Skipped {len(skipped)} already deprecated terms: {' '.join(sorted(skipped))}", file=sys.stderr)
    if missing:
        print(f"WARNING: {len(missing)} terms not found in ontology: {' '.join(sorted(missing))}", file=sys.stderr)

    # Report remaining references to obsoleted terms from non-obsolete axioms.
    obsoleted = set(t for t in replacements if t not in skipped and t not in missing)
    term_re = re.compile(r"obo:OHD_\d+")
    for line in out:
        if line.startswith(("Declaration(", "#", "AnnotationAssertion(")):
            continue
        subj = subclass_subject(line)
        if subj in obsoleted:
            continue
        refs = obsoleted.intersection(term_re.findall(line))
        if refs:
            print(f"NOTE: obsoleted term(s) {' '.join(sorted(refs))} still referenced in: {line}", file=sys.stderr)


if __name__ == "__main__":
    main()
