source('build.R')
## scrape.R 
# use this file to scrape the EDI portal using batch_pull, ideally note the date and who searched
# say if the data was saved and what you were doin!!

## 1. 2020/02/20 -- WR
# this query is the one that pulls an image
plant_comp <- batch_pull(c("Saddle", "Community", "Survey"), filter = FALSE ,save = FALSE)
summarize_data(plant_comp, plot = TRUE)
# testing out search for plant comp
# Not saving the data
# does it work? There are still a couple of warnings but it is with messy named data.



## 2. 2020/02/20 -- WR
soil_texture <- batch_pull(c("Soil", "Texture"), filter = TRUE ,save = FALSE)
summarize_data(soil_texture, plot = TRUE)
# Searching for soil texture, with filter it returens 2
# need to work on which columns to be factord