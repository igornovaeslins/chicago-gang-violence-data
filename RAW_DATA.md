# Raw data — sources, freeze, and checksums

The analysis runs offline from the intermediate files shipped in
`data/intermediate/` and `data/processed/`. This file documents the raw inputs
those intermediates were built from, so the build scripts in `scripts/build/`
can be audited and re-run.

## Why the raw data is frozen, not re-downloaded

The Chicago Data Portal is a live system. Records are revised, recoded, and
added retroactively, so a download today returns different counts than the
extract used here. To keep the build exactly reproducible, the raw inputs were
frozen on the access date below and are identified by SHA-256. Re-running
`scripts/build/03_download_chicago.R` against the live portal will *not*
reproduce these checksums or the published counts; it is included for
provenance. To rebuild the intermediates exactly, use the frozen files.

- **Access date (download):** 2026-04-01 / 2026-04-02
- **Portal:** https://data.cityofchicago.org

## Sources and dataset identifiers

| Raw file | Source dataset | Socrata ID |
|---|---|---|
| `chicago_domestic_violence_2001_present.csv`, `chicago_domestic_violence_2008_2014.csv` | Crimes — 2001 to Present (filtered `domestic='true'`) | `ijzp-q8t2` |
| `chicago_homicides_2001_present.csv` | Crimes — 2001 to Present (filtered `primary_type='HOMICIDE'`) | `ijzp-q8t2` |
| `chicago_shooting_victims.csv` | Violence Reduction — Victims of Homicides and Non-Fatal Shootings | `gumc-mgzr` |
| `chicago_community_areas.geojson` | Boundaries — Community Areas | `cauq-8yn6` |
| `arrests_2008_2024.csv` | Arrests | `dpt3-jri9` |
| `gang_boundaries_YYYY.geojson` (2007–2010, 2012, 2014–2025) | Chicago Police Department CLEARMAP gang-territory maps (GIS portal) | — |

## Where to get the frozen raw data

The frozen raw set is too large for the Git tree, so it ships as a release
asset on this repository:

> **Download:** https://github.com/igornovaeslins/chicago-gang-violence-data/releases/download/raw-data-v1/chicago_frozen_raw_v1.zip
> (release `raw-data-v1`; ~125 MB zipped, ~504 MB unzipped, 23 files)
> Archive SHA-256: `f1f3e131e515adcce2da82bf8becf442a8259b36266b9636dbade14d95a81a81`
>
> The package as a whole is archived on Zenodo with a permanent DOI:
> **https://doi.org/10.5281/zenodo.20532940**

Unzip it, verify the per-file checksums below (a `raw_checksums.txt` is included
in the archive), and stage the files under `data/raw/chicago/` in this layout
before running the build scripts:

```
data/raw/chicago/
├── crimes/
│   ├── chicago_domestic_violence_2008_2014.csv
│   ├── chicago_domestic_violence_2001_present.csv
│   └── chicago_homicides_2001_present.csv
├── shootings/
│   └── chicago_shooting_victims.csv
├── police_operations/
│   └── arrests_2008_2024.csv
└── boundaries/
    ├── chicago_community_areas.geojson
    └── gang_territories/
        └── gang_boundaries_{2007,2008,2009,2010,2012,2014..2025}.geojson
```

## SHA-256 checksums (verify after download)

```
08131f32d2370d988a647841e9d2c93b15386d4faf7129bbdf9cc9d68c551106  chicago_domestic_violence_2008_2014.csv
fae241048470756e2122c35ede7f63e00d190915a88a65471ba546bc4d9bd328  chicago_domestic_violence_2001_present.csv
b78b08fe8dfce61c0e2eb86d3e859b9c49efc8a469cd7ff19e790089dcd0b24f  chicago_homicides_2001_present.csv
0bdb837bd6776f19c64d485c165b17a6868dcd646fcd334b4d582885fc1b8e7d  chicago_shooting_victims.csv
0916636d0bc86d32a175cf3369d98508efc8033ca1b40a5909dccbc8c94751af  arrests_2008_2024.csv
91fe4f629f74dbea236640853deb2f5287a7b28071ded729f848758a49c5c18f  chicago_community_areas.geojson
c8d3ebbb9aa2971a2c519eca2e1aa4ac29a5a28ad41d117c65458addca2c38cf  gang_boundaries_2007.geojson
70130b317f70adc1a9c506bc605135220d787bf15e9f46bf3a012b32d4b1eb25  gang_boundaries_2008.geojson
6ea727b6cdf9da4d5ccbc2ef2bd266e9a157e2bbc99e5ea0adf7657a7b2498b2  gang_boundaries_2009.geojson
11d495a4f04f809127d071064d71d4d29f2f94bfbf967b1dafa49552a9a64af4  gang_boundaries_2010.geojson
15e40ec6e253ce5edaab37dc483e6dfdd5ceb08caccf97c2805ae3d95dc908b4  gang_boundaries_2012.geojson
90f46e2e50339cf6b9a1ff0d37019d4ea07efd8c0b9a71a6a6adce38163028c3  gang_boundaries_2014.geojson
77a6ffa6a8da6e42b8dd20aad668baa3718825d0f751e913d773cdd1ff2f5d90  gang_boundaries_2015.geojson
f5dbadc62dd437a714555d6252bf0a2d2d44243f120a9c19c14d0d1e98dc4072  gang_boundaries_2016.geojson
a0372cedb311b0b7c6f5106046f3e2fa131c1b32c033a964119acd252073d176  gang_boundaries_2017.geojson
8b32495b6f7be2c395f21edd17ee07e3355e63635f2f64863443e4330b019187  gang_boundaries_2018.geojson
116f1d8f60a0ce40aeec4e3d6a6803300ed6f2ab48ca2e0f3a1fb98e435abd47  gang_boundaries_2019.geojson
b8b2e30ed2a583e9ea516a9133fa171d05f4ec78158243c9585204d7872647b9  gang_boundaries_2020.geojson
2e23387ebc4a46302360d0cf9011404a6e07a5ba15c31f59be4ff2c4c8037b58  gang_boundaries_2021.geojson
34b5245d15197b1e03268a854ee89454ecee2ba46cb6baf9453fdf89ca99c9be  gang_boundaries_2022.geojson
0086dee79b12b7be0d01e8a515af26526eb6efb243fd581bfbedca634902f4ce  gang_boundaries_2023.geojson
4f0022fc3b71b9e8fd9be6ef51ab9448411c8775cb18fea064cb2cfd0ea5dfa0  gang_boundaries_2024.geojson
196e44ba2dc2302a5de9ef8e96183752f969c3e77c7d2fd4261c388ec773bb65  gang_boundaries_2025.geojson
```

Verify with `shasum -a 256 -c` against this list (save the block above to
`raw_checksums.txt` first).

## Note on the arrests file

`arrests_2008_2024.csv` is used only to populate `n_arrests` in the panel, which
the analysis uses for the arrest-rate test. Arrests are linked to
domestic-violence cases by `case_number`; about 10% of arrests match a DV case
and inherit its community area. The file is large (~200 MB) and is part of the
release archive, not the Git tree.

## Cook County State's Attorney — Felony Cases: Initiation (added July 2026)

- Portal: https://datacatalog.cookcountyil.gov/ · dataset id `7mck-ehwz`
- Used by `scripts/59_sao_share_feminino_reus_arma.R`, which queries the Socrata API for
  server-side aggregates (year × gender × offense-category counts, defendants identified by
  `primary_charge = true`, `incident_city = 'Chicago'`). No case-level rows are stored in this
  package. The exact SoQL queries are recorded in `results/reforma_sao_gun_gender.csv`.
- Accessed 2026-07-02. The dataset carries no community-area geography usable for the
  treated/control contrast, so the series is citywide.
