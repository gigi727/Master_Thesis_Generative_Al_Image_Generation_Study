# Public-release checklist

Vor dem Push auf GitHub prüfen:

- [ ] `scripts/Dataset_Coded_Prompts_2026_04_09.csv` ist nicht enthalten.
- [ ] `data_raw/` enthält keine Qualtrics-Rohdaten.
- [ ] `data_processed/` enthält keine Zwischenstände.
- [ ] Keine hochgeladenen Teilnehmerbilder sind enthalten.
- [ ] `data_final/` enthält nur `final_analysis_dataset_anonymized.csv`, `.rds` und optional die Export-Summary.
- [ ] Der finale Datensatz enthält keine Spalten oder Werte mit E-Mail, IP, ResponseId, Case_ID, Namen, Koordinaten oder sonstigen direkten Identifikatoren.
- [ ] `data_output/project_master_index/Output for Research/` enthält nur `.html`, `.rtf` und `.png`.
- [ ] Keine CSV-, XLSX- oder TXT-Outputs sind in `data_output/` gestaged.
- [ ] `data_output/project_master_index/00_master_export_index.html` lässt sich lokal öffnen.
- [ ] Links im Master-Index funktionieren relativ im Repository.
- [ ] `git status --ignored` wurde geprüft, um versehentlich ignorierte oder sensible Dateien sichtbar zu machen.
