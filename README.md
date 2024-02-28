Author: Matt Jenkin, University of Lausanne, Switzerland ([mjenkin\@unil.ch](mailto:mjenkin@unil.ch){.email})

Date: Feb 2024

Code and data used for the main analysis in the Journal of Glaciology article: [Tracking coarse sediment in an Alpine subglacial channel using radio-tagged particles](https://doi.org/10.1017/jog.2023.77).

Written in R mainly using the `tidyverse` package collection, `sf` for spatial data and `eks` for tidy/geospatial implementation of `ks` kernel smoothing sfunctions. An `.Rproj` and a [`renv`](https://rstudio.github.io/renv/articles/renv.html) lockfile are provided for package version management. This repo represents a major refactoring and cleanup from the original versions (1.0-1.2) published on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.7550558), aiming to aid generalisation to other projects.

------------------------------------------------------------------------

```         
.
└── subglacial_RFID
    ├── README.md
    ├── .Rprofile
    ├── data
    │   ├── antennas.gpkg
    │   ├── channel.gpkg
    │   ├── dGNSS.gpkg
    │   ├── glacier_outline.gpkg
    │   ├── roving_antenna.gpkg
    │   ├── stationary_antennas.csv
    │   └── tags.csv
    ├── renv.lock
    ├── run.R
    └── subglacial_RFID.Rproj
```
