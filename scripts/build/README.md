# Build scripts — from raw data to the intermediate files

These three R scripts reconstruct the intermediate files the analysis reads,
starting from the raw Chicago open-data downloads. They are the provenance of
the treatment: the gang-territory assignment, the spatial joins, and the
community-area × year panel are all defined here.

The analysis (`scripts/47..56`) runs offline from the shipped intermediate
files and does **not** require these build scripts. Run them only to rebuild the
intermediates from raw, or to audit how the treatment was constructed.

## Order

```
Rscript scripts/build/03_download_chicago.R       # downloads raw from the portal (needs internet)
Rscript scripts/build/07_consolidate_chicago.R    # filters/dedups to 2008–2024
Rscript scripts/build/11_gang_spatial_join.R      # gang panel + spatial join + aggregation
```

`03` writes into `data/raw/chicago/`; `07` reads from there and writes the
2008–2024 intermediate tables; `11` reads the gang polygons, the boundaries, the
arrests file, and the `07` outputs, and writes the treatment join
(`shootings_with_gang_territory_2008_2024.csv`) and the panel base
(`chicago_vcm_community_area_year.csv`).

## What they produce (verified)

Running `07` then `11` on the frozen raw data reproduces, exactly:

- `chicago_dv_2008_2024.csv` — **855,387** rows
- `chicago_shootings_2008_2024.csv` — **49,291** rows
- `chicago_homicides_2008_2024.csv` — **9,882** rows
- the gang panel, the gang-control table, and `chicago_vcm_community_area_year.csv`

The rebuilt intermediates feed the analysis to the same `32 OK | 0 CHECK` in
`scripts/56_consistency_check.R`. The gang-control area shares differ from a
prior build by sub-percent slivers (R's `sf`/GEOS vs. the earlier `geopandas`
overlay); these move no result and no community area changes its
treatment-tertile assignment.

## The frozen raw data is NOT in this repo

Two reasons. First, the raw files are large (the arrests file alone is ~200 MB,
the domestic-violence exports ~120–145 MB each). Second, the Chicago Data Portal
is a live system: records are revised continuously, so a fresh download will not
return the same counts as the frozen extract used here. Reproducibility means
"from *these* data, *this* code, *these* numbers" — so the exact raw inputs are
frozen separately, with access date and SHA-256 checksums, listed in
`RAW_DATA.md` at the package root. Stage them under `data/raw/chicago/` (the
layout `03` writes and `07`/`11` read) before running `07`/`11`.

## Method (for auditing the treatment)

- Areas: computed in EPSG:26916 (UTM Zone 16N). Coordinates: EPSG:4326 (WGS84).
- Gang control per community area × year: area of intersection between the
  community area and each gang polygon, divided by the community-area area;
  `pct_area_gang_control` is the summed share (capped at 1), `gang_dominant` is
  the gang with the largest share, `n_gangs_present` the distinct count.
- Crime/shooting → gang: point-in-polygon with the `within` predicate; ties
  (a point in two polygons) take the first.
- Missing gang maps for 2011 and 2013 use the nearest available year (2010,
  2012); `gang_data_year` records which map was used.
