Author: Matt Jenkin
Date: Oct 2023

Code used for the main analysis in the Journal of Glaciology article: Tracking coarse sediment in an Alpine subglacial channel using radio-tagged particles:
<https://doi.org/10.1017/jog.2023.77>

Author: Matt Jenkin, University of Lausanne, Switzerland (mjenkin@unil.ch)
Date: 2023-04-06

The data and code provided in the file "Particle_tracking.zip" comprise the Supplementary Materials of the method paper titled: "Tracking coarse sediment in an Alpine subglacial channel using radio-tagged particles" by Jenkin et al. (2023) (doi: TBC - currently not published), and were used to perform the main analyses in the paper. The data and code are disseminated in accordance with the open access policy of the Swiss National Science Foundation, which funded the research in grant 188734. The code is written in the open source R programming language for statistical computing using RStudio IDE software. Basic knowledge of the use of R within RStudio is assumed, as is a functional installation of the software. The package 'renv' was used for dependency management.

Following the methods and workflow detailed in the method paper, the code ultimately produces location estimates for individual RFID tagged particles deployed in glacial environments. This is achieved by performing 2D kernel density estimation on georeferenced RFID point data (obtained with a roving antenna). Stationary antenna data is also analysed to provide contextual information on the timing of tagged particle motion. Details of the script and data files are provided below. Spatial data is provided in GeoPackage (GPKG) format, an Open Geospatial Consortium (OGC) standard (https://www.ogc.org/standards/geopackage). OGC simple features standards are used with GPKG files where possible (sf package, https://r-spatial.github.io/sf/). 

INSTRUCTIONS FOR USE:
  -Install R and RStudio IDE if necessary
  -Check working directory is set to the downloaded and unzipped folder.
  -Open and run "./Script/setup.R", installing 'rent' if needed.
  -Open "./Script/analysis.R"
  --Modify lines 3-8 as desired (consult 'combinations' object for TagID and Survey_data possibilities, and method paper for background)
  --Run KDE functions (KDE_single, KDE_multiple) and stationary antenna (stationary_ant) functions as desired
  --Run plotting function as desired
  --View plots as desired with 'plot("plotname")'

FILE STRUCTURE:
  Particle_tracking
    -readme.txt (this file)
    Script
      -analysis.R: script used to run custom localisation and plotting functions
      -KDE_multiple_fun: custom function to localise a single tagged particle in all surveys
      -KDE_single_fun.R: custom function to localise a single tagged particle in a single survey
      -plots.R: custom function to plot the output of KDE_multiple, KDE_single and stationary_ant functions
      -setup.R: script used to load necessary packages, functions and data files for analysis.R
      -stationary_ant_fun.R: custom function to localise tagged particles using stationary antennas
    Data
      -antennas.gpkg (stationary antenna locations and metadata)
        --Reader_ID: factor, the antenna ID code (1-15)
        --Type: factor, "Supraglacial" or "Proglacial""
        --X: numeric, Easting coordinate in Swiss CH1903+ / LV95 projected crs (espg:2056)
        --Y: numeric, orthing coordinate in Swiss CH1903+ / LV95 projected crs (espg:2056)
        --Distance: numeric, the pre-calculated along-channel distance from the injection borehole to each antenna
        --geom: sf point XY geometry column in Swiss CH1903+ / LV95 projected crs (espg:2056)
      -channel.gpkg (subglacial and proglacial centreline)
        --geom: sf linestring XY geometry column in Swiss CH1903+ / LV95 projected crs (espg:2056)
      -dGPS_track.gpkg (dGPS point data from all roving antenna surveys)
        --Timestamp: datetime, the date and time of point measurement in yyyy-mm-dd hh:mm:ss format
        --Date: factor, the survey date in yyyy-mm-dd
        --geom: sf XYZ point geometry column in Swiss CH1903+ / LV95 projected crs (espg:2056)
      -glacier_outline.gpkg (outline of the Glacier d'Otemma snout margin)
        --geom: sf XY polygon geometry column in Swiss CH1903+ / LV95 projected crs (espg:2056)
      -roving_antenna_data.gpkg (point cloud of received radio transmissions from a radio-tagged particle)
        --Timestamp: datetime, the date and time of point measurement in yyyy-mm-dd hh:mm:ss format
        --Date: factor, the survey date in yyyy-mm-dd
        --Tag_ID: factor, the ID code of the tagged particle
        --RSSI: numeric, the received signal strength of the radio transmission
        --Deployment_timestamp: datetime, the time of deployment of the tagged particle in yyyy-mm-dd hh:mm:ss format
        --geom: sf XYZ point geometry column in Swiss CH1903+ / LV95 projected crs (espg:2056)
      -stationary_antenna_data.csv (raw tagged particle detection record from stationary antennas)
        --Timestamp: datetime, the date and time of measurement in yyyy-mm-dd hh:mm:ss format
        --Date: factor, the survey date in yyyy-mm-dd
        --Tag_ID: factor, the ID code of the tagged particle
        --In_Out: factor, indication of the start (In) or end (Out) time of an in-range period
        --Reader_ID: factor, the antenna ID code (1-15)
      -survey_times.csv (survey metadata)
        --Survey_date: date, the date of the survey in yyyy-mm-dd format
        --Survey_time: datetime, the median datetime of the survey in yyyy-mm-dd hh:mm:ss format 
      -tag_list.csv (tagged particle metadata)
        --Tag_ID: factor, the ID code of the tagged particle
        --Deployment_timestamp: datetime, the time of deployment of the tagged particle in yyyy-mm-dd hh:mm:ss format
        --Mass_g: numeric, the particle mass in grams
        --Density_gcm3: numeric, the particle density in grams per cubic centimetre
        --a_axis_mm: numeric, the length of the particles longest axis in millimetres
        --b_axis_mm: numeric, the length of the particles middle axis in millimetres
        --c_axis_mm: numeric, the length of the particles smallest axis in millimetres       
        --Transmit_interval_s: numeric, the particle transmission interval in seconds
        --in_range_threshold_s: numeric, the stationary antenna noise threshold in seconds per timestep
