# Subglacial RFID Method Snapshot

Author: Matt Jenkin, University of Lausanne, Switzerland

Published date: February 2024

## Repository Role

This public repository is the reproducibility and reference snapshot for the
RFID method paper:

[Tracking coarse sediment in an Alpine subglacial channel using radio-tagged
particles](https://doi.org/10.1017/jog.2023.77)

It is derived from a larger private working/archive repository, `RFID_method`,
which contains fuller development and manuscript context. In that private
archive, this repository is linked as the `dist/` Git submodule:

```text
RFID_method/dist -> https://github.com/m-jenkin/subglacial_RFID.git
```

This repository should be updated deliberately, when the public method snapshot
needs correction, clarification, or a more reproducible presentation.

## What This Reproduces

This is a minimal method capsule. It does not reproduce every result or figure
from the paper. Instead, it shows the computational workflow used to turn active
RFID roving antenna and stationary antenna records into particle transport
distance estimates.

The workflow demonstrates how to:

1. load cleaned spatial and tabular RFID inputs;
2. calculate roving survey effort across a grid;
3. weight roving detections using detection count, RSSI and survey time;
4. localise particles with weighted KDE;
5. convert particle locations and uncertainty contours to along-channel
   transport distance;
6. estimate stationary antenna detection reaches;
7. combine roving and stationary records into a single transport table.

The article and supporting information are included for context:

- `JOG2300077.PDF`
- `JOG2300077_SI.pdf`

## How To Run

From the repository root:

```r
source("run.R")
```

This runs the full scripted workflow and writes CSV outputs to `outputs/`.

For a guided explanation, render the R Markdown walkthrough:

```r
rmarkdown::render("method_walkthrough.Rmd", output_file = "run.html")
```

The rendered `run.html` is included so readers can inspect the method without
first running the code.

## Repository Structure

```text
.
├── R/
│   ├── functions.R              # reusable data, spatial and validation functions
│   └── plots.R                  # plotting helpers
├── scripts/
│   ├── 00_setup.R               # package loading, constants and input loading
│   ├── 01_roving_heuristic.R    # survey effort and roving detection weighting
│   ├── 02_kde_localisation.R    # KDE locations and along-channel distance
│   ├── 03_stationary_antennas.R # stationary antenna reach records
│   ├── 04_combine_records.R     # final combined output tables
│   └── 05_validate_outputs.R    # lightweight reproducibility checks
├── data/                        # cleaned minimal input data
├── outputs/                     # generated CSV output tables
├── assets/report.css            # light styling for the rendered walkthrough
├── method_walkthrough.Rmd       # human-readable method walkthrough
├── run.R                        # machine-readable entry point
├── run.html                     # rendered walkthrough
├── renv.lock                    # R package version lockfile
└── subglacial_RFID.Rproj
```

## Software Environment

The workflow is written in R, mainly using `tidyverse` packages, `sf` for
spatial data, and `ks`/`eks` for kernel density estimation. An RStudio project
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

## Licence

CC BY
