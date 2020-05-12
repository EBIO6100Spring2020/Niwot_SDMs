# Niwot_SDMs
A separate repo for scripts/data for work associated with SDMs using Niwot LTER data

This the project repo for our SDMs of _Geum rossii_ using NEON LTER vegetative survey data from Niwot Ridge along with lidar indices. All modeling is done in cleaned.modeling.script.R (though it is a bit scattered). Data manipulation and raster extraction is done in the NEON.Data.R file. Smaller raster tiles were filtered and joined using bash scripts and R scripts which are not stored here. If you're interested in testing the entire workflow I can add them to the repo. They are mainly left off because there are a good number of them and they tend to clutter up the repo (and aren't able to be run without sizable datasets stored elsewhere, see below).

All Bayesian multilevel models can be run with the CSV here in the repo. For the maximum entropy models, you will need the raster data which is stored on a separate google drive: https://drive.google.com/open?id=1Lfqnrzx0zALEsCAuwKxoMn39e8UYWIh1. Running maxnet in R also requires specific Java installations, though I can't recall where those are on the Java website.
