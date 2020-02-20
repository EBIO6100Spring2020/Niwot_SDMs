## scrape.R 
# use this file to scrape the EDI portal using batch_pull, ideally note the date and who searched
# say if the data was saved and what you were doin!!

## 1. 2020/02/20 -- WR
plant_comp <- batch_pull(c("Plant", "Community"), filter = FALSE ,save = FALSE)
# testing out search for plant comp
# Not saving the data
# does it work? There are still a couple of warnings but it is with messy named data.
