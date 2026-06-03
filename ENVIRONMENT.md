# Computational environment

The published results were produced under the environment below. The analyses
are estimator-stable, but a few packages (notably `WeightIt`, `fixest`, `spdep`)
can change behavior across major versions, so the exact versions are recorded
here. Install these versions if you need a bit-for-bit match; the consistency
check (`scripts/56_consistency_check.R`) will tell you if a newer version has
drifted.

## R

- R 4.3.3 (2024-02-29)
- Platform: macOS (aarch64-apple-darwin)
- Locale note: on a non-ASCII project path, run with `env LC_ALL=en_US.UTF-8`.

## Packages (exact versions used)

| Package | Version |
|---|---|
| dplyr | 1.1.4 |
| tidyr | 1.3.1 |
| readr | 2.1.5 |
| stringr | 1.5.1 |
| purrr | 1.0.4 |
| httr | 1.4.7 |
| jsonlite | 2.0.0 |
| fixest | 0.12.1 |
| sandwich | 3.1.1 |
| lmtest | 0.9.40 |
| broom | 1.0.8 |
| sf | 1.0.21 |
| spdep | 1.3.13 |
| spatialreg | 1.3.6 |
| WeightIt | 1.7.0 |
| cobalt | 4.6.2 |
| ggplot2 | 3.5.2 |
| scales | 1.4.0 |

## Pinning to these versions

`scripts/00_setup.R` installs the latest CRAN versions, which is usually fine.
To reproduce the exact environment instead, install the versions above, e.g.:

```r
install.packages("remotes")
remotes::install_version("WeightIt", version = "1.7.0", repos = "https://cloud.r-project.org")
remotes::install_version("fixest",   version = "0.12.1", repos = "https://cloud.r-project.org")
remotes::install_version("spdep",    version = "1.3.13", repos = "https://cloud.r-project.org")
# ... and so on for the rest
```

After installing, run `Rscript scripts/56_consistency_check.R`. It exits 0 and
prints `32 OK | 0 CHECK` when the environment reproduces the published numbers.

For a machine-restorable lockfile rather than prose pinning, the versions above
can be captured in an `renv.lock` (`renv::init()` then `renv::snapshot()`); the
consistency check detects version drift but does not by itself prevent it.

## Determinism

The two wild cluster bootstraps set a fixed seed (`set.seed()` in scripts 48 and
49, 9,999 replications each). The entropy-balancing step (script 50) and the
fixed-effects / Wald tests (script 55) are deterministic and need no seed.
