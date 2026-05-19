# Subglacial RFID particle tracking method: data and R workflow

Author: Matt Jenkin, University of Lausanne, Switzerland

May 2026

This repository is a compact, public method capsule for the RFID particle
tracking workflow described in:

[Tracking coarse sediment in an Alpine subglacial channel using radio-tagged
particles](https://doi.org/10.1017/jog.2023.77)

It is intended as the cleaned reference version of the method code and minimal
example data. It supersedes the earlier Zenodo code snapshot for reuse and
reproducibility, while preserving the scope of a method demonstration rather
than a full raw-data processing archive.

The archived version of this method capsule is available on Zenodo:

[https://doi.org/10.5281/zenodo.20272565](https://doi.org/10.5281/zenodo.20272565)

If you use this code or data, please cite both the method paper and the Zenodo
record.

The workflow demonstrates how to:

1. load cleaned spatial and tabular RFID inputs;
2. calculate roving survey effort across a grid;
3. weight roving detections using detection count, RSSI and survey time;
4. localise particles with weighted KDE;
5. convert particle locations and uncertainty contours to along-channel
   transport distance;
6. estimate stationary antenna detection reaches;
7. combine roving and stationary records into a single transport table.

The article and supporting information are not bundled in this repository. They
are available from the publisher via the paper DOI above.

## How To Run

From the repository root:

```r
source("run.R")
```

This runs the full scripted workflow and writes CSV outputs to `outputs/`.

For a guided explanation, render the R Markdown walkthrough:

```r
rmarkdown::render("method_walkthrough.Rmd", output_file = "method_walkthrough.html")
```

The rendered `method_walkthrough.html` is included so readers can inspect the
method without first running the code.

## Repository Structure

```text
.
├── R/
│   ├── packages.R               # package loading
│   ├── data_io.R                # input loading and input checks
│   ├── geometry.R               # spatial helper functions
│   ├── roving.R                 # roving antenna heuristic and KDE functions
│   ├── stationary.R             # stationary antenna reach functions
│   ├── combine.R                # final roving/stationary table assembly
│   ├── validate.R               # lightweight reproducibility checks
│   └── plots.R                  # plotting helpers
├── scripts/
│   ├── 00_setup.R               # package loading, constants and input loading
│   ├── 01_roving_heuristic.R    # survey effort and roving detection weighting
│   ├── 02_kde_localisation.R    # KDE locations and along-channel distance
│   ├── 03_stationary_antennas.R # stationary antenna reach records
│   └── 04_outputs.R             # final tables, checks and CSV writing
├── data/                        # cleaned minimal input data
├── outputs/                     # generated CSV output tables
├── assets/report.css            # light styling for the rendered walkthrough
├── method_walkthrough.Rmd       # human-readable method walkthrough
├── run.R                        # machine-readable entry point
├── method_walkthrough.html      # rendered walkthrough
├── renv.lock                    # R package version lockfile
├── renv/                        # renv bootstrap/settings files
└── subglacial_RFID_method.Rproj
```

## Software Environment

The workflow is written in R, mainly using `tidyverse` packages, `sf` for
spatial data, and `ks`/`eks` for 2D kernel density estimation. An RStudio project
file and [`renv`](https://rstudio.github.io/renv/) lockfile are provided for
software version management.

If recreating the original package environment, open the project and run:

```r
renv::restore()
```

## Reuse Notes

This repository is intended as a transparent template, not a black-box package.
For another field site, review these assumptions before reuse:

- the coordinate reference system must use metric units;
- the channel centreline must represent the expected transport path;
- grid size should reflect survey density and detection range;
- RSSI behaviour may vary by antenna, water depth, particle position and site;
- KDE bandwidth choice should be re-evaluated for new survey geometries;
- stationary antenna detection ranges should be calibrated or justified.

## License

This method capsule is released under the Creative Commons Attribution 4.0
International License (CC BY 4.0). See `LICENSE.md`.
