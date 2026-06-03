# Chicago gang territory, gun violence, and domestic violence — data and code

A community-area × year dataset for Chicago (2008–2024) linking gang-territory
coverage, shooting victims, domestic-violence incidents, homicides, and Census
demographics, together with the R scripts that build the panel and run the
analyses. Every input comes from a public source. The pipeline runs offline from
the files shipped here.

---

## Layout

```
.
├── scripts/
│   ├── 00_setup.R                       install the R packages
│   ├── 47_painel_chicago_consolidado.R  build the community-area × year panel
│   ├── 48_regressoes_chicago.R          cross-sectional OLS, balance table, Moran's I
│   ├── 49_event_study_chicago.R         event study and difference-in-differences
│   ├── 50_robustez_matching_chicago.R   entropy-balanced DiD
│   ├── 55_mechanism_tests_chicago.R     post-2015 DiD breakdowns; gang-change event study
│   └── 56_consistency_check.R           recomputes key statistics and checks them
├── data/
│   ├── processed/                       analysis-ready panel and ACS controls
│   ├── intermediate/                    incident-level DV, gang-matched shootings
│   └── raw_boundaries/                  community-area polygons
├── results/                             tables produced by the scripts (regenerated each run)
├── data_dictionary.md                   column-by-column description of every file
├── LICENSE
└── README.md
```

---

## Requirements

R ≥ 4.3. Install the packages once:

```bash
Rscript scripts/00_setup.R
```

Packages used: dplyr, tidyr, readr, stringr, fixest, sandwich, lmtest, broom,
sf, spdep, spatialreg, WeightIt, cobalt, ggplot2, scales.

---

## Running

Run from the repository root, in order. The scripts resolve their own location,
so the working directory does not matter.

```bash
Rscript scripts/47_painel_chicago_consolidado.R     # builds data/processed/chicago_painel_completo.csv
Rscript scripts/48_regressoes_chicago.R             # ~1 min (9,999-rep wild bootstrap)
Rscript scripts/49_event_study_chicago.R
Rscript scripts/50_robustez_matching_chicago.R
Rscript scripts/55_mechanism_tests_chicago.R
Rscript scripts/56_consistency_check.R              # recomputes & checks key statistics
```

Script 47 builds the panel that 48, 49, 50, and 55 all read. Script 56 reads
outputs from 48 and 50, so it runs last. It recomputes a set of headline
statistics from the data, compares each to its reference value within a
tolerance, writes `results/consistency_check.csv`, and exits non-zero if any
item fails — so it can also serve as a CI / pre-release gate.

---

## Data sources (all public)

| Series | Source | Identifier | Records | Coverage |
|---|---|---|---|---|
| Domestic-violence incidents | City of Chicago, "Crimes 2001 to Present" (domestic flag) | `ijzp-q8t2` | 855,387 | 2008–2024 |
| Homicides | Same crimes dataset, homicide offense | `ijzp-q8t2` | 9,882 | 2008–2024 |
| Shooting victims | "Violence Reduction: Victims of Homicides and Non-Fatal Shootings" | `vqmv-zqjm` | 49,291 | 2008–2024 |
| Gang-territory polygons | Chicago Police Department CLEARMAP gang-territory maps (GIS portal) | — | annual maps | 2007–2024 (2011, 2013 absent) |
| Demographics | U.S. Census Bureau, American Community Survey (5-year) | — | — | 5-year estimates |

Identifiers refer to the [Chicago Data Portal](https://data.cityofchicago.org).
The incident-level domestic-violence file shipped here keeps the six fields the
scripts use (`community_area`, `date`, `location_description`, `primary_type`,
`arrest`, `year`); the full 22-field export is recoverable from the portal under
the identifier above. Victim-name fields were removed from the shooting-victim
file; all other fields are as published by the City of Chicago. The analysis
operates at the community-area × year level, not at the individual-record level.

See `data_dictionary.md` for a column-by-column description of every file.

---

## Construction notes

- The temporal unit is the calendar year; the spatial unit is the Chicago
  community area (77 areas).
- **2015 is dropped from the analyses.** Its public crimes extract holds 7,507
  domestic-violence records against a period median near 51,000, consistent with
  an incomplete export. `results/diagnostico_n_por_ano_chicago.csv` documents the
  per-year counts.
- Annual gang coverage is highly stable across years (within-area inter-year
  correlation above 0.97), and two map years are missing, so each community area
  is assigned its **modal dominant gang** across all mapped years. The 2011 and
  2013 gaps use a nearest-year fallback, flagged in the panel.
- The DV **arrest rate** is administrative: the share of domestic-violence
  incidents recorded as resulting in an arrest. The CPD arrests dataset
  (`dpt3-jri9`) begins in 2014, so arrest counts before 2014 are zero.

---

## On the use of AI tools

A statement on the use of AI tools in building this package is in `AI_USE.md`.

## License

Code: MIT (see `LICENSE`). Data files are redistributed from the public sources
listed above and remain subject to their providers' terms.
