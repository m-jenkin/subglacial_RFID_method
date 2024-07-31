Author: Matt Jenkin, University of Lausanne, Switzerland

Published date: Feb 2024

Code and data used for the main analysis in the Journal of Glaciology article: [Tracking coarse sediment in an Alpine subglacial channel using radio-tagged particles](https://doi.org/10.1017/jog.2023.77). The code demonstrates the steps used to localise particles with roving antenna and stationary antenna data. For an overview, see `run.html` for the code and rendered output. This repo represents a major refactoring and cleanup of the code published on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.7550558), with the aim of improving generalisation to other projects. 

Written in `R` mainly using the core `tidyverse` packages, `sf` for basic spatial data manipulation and `eks` for (tidy) geospatial implementation of the `ks` kernel smoothing functions. 
An `.Rproj` and a [`renv`](https://rstudio.github.io/renv/articles/renv.html) lockfile are provided for software version management. 

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
    ├── run.html
    └── subglacial_RFID.Rproj
```

CC BY
