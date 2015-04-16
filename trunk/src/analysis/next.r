queryc("select ?first_visit ?next_visit
where
{ ?patient a patient:.
  ?first_visit a outpatient_encounter:.
  ?first_visit has_participant: ?patient.
  ?first_visit realizes: ?role_first.
  ?role_first a patient_role:.
  ?role_first inheres_in: ?patient
  ?first_visit occurrence_date: ?first_date.
  { ?next_visit a outpatient_encounter:.
    ?next_visit has_participant: ?patient.
    ?next_visit realizes: ?role_in_next.
    ?role_in_next a patient_role:.
    ?role_in_next inheres_in: ?patient.
    ?next_visit occurrence_date: ?next_date.
    filter (?date_next > ?date && ?next_visit != ?visit)
  }
  optional
  { ?nonexistent_middle_visit a outpatient_encounter:.
    ?nonexistent_middle_visit has_participant: ?patient.
    ?nonexistent_middle_visit realizes: ?role_in_middle.
    ?role_in_middle a patient_role:.
    ?role_in_middle inheres_in: ?patient.
    ?nonexistent_middle_visit occurrence_date: ?middle_date.
    filter ((?next_date > ?middle_date) && (?middle_date > ?first_date))
  }
  filter(!bound(?nonexistent_middle_visit))
}
limit 20
");


