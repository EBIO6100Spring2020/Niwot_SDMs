
#Gonna try and read in a lot of sensor data

library(raster)
library(dplyr)

#I've plotted a lot of these using Diane Ebert May's data, but keep in mind
#there is no temporal overlap between when the majority of this sensor/Lidar data
#was collected and when Ebert May's sampling took place. Last Ebert May survey was
#in 2011 and i think the earliest NEON data I've seen was with like 2013 or something.

setwd("../NEON.Data/NEON_precipitation/")


soil_salinity_pos <- read.csv("../NEON_conc-h2o-soil-salinity/NEON.D13.NIWO.DP1.00094.001.2020-02.expanded.20200304T004131Z/NEON.D13.NIWO.DP1.00094.001.sensor_positions.20200304T004131Z.csv")
#so yeah five locations for soil moisture from these sensors. probably not fine scale enough
#for our inference.


#so for the veg survey, this is the percent cover, taken at a 1 square meter resolution
#for 8 subplots in each plot. The relationship between the numbers in plot v subplot
#is a bit opaque to me, but im sure its sensible. Probably. Who knows.
veg_survey_NEON <- read.csv("../NEON_presence-cover-plant/NEON.D13.NIWO.DP1.10058.001.2019-08.expanded.20200309T210318Z/NEON.D13.NIWO.DP1.10058.001.div_1m2Data.2019-08.basic.20200309T210318Z.csv")

#ayyy got all that data in, took some doin but managed it. okay. now let's clean it a bit, first by splitting our dates,
#then by pulling out just deschampsia.

veg_files <- list.files(path = "../NEON_presence-cover-plant/10m.veg.PA/", pattern = "*.csv")
veg_files <- paste0("../NEON_presence-cover-plant/10m.veg.PA/",veg_files)
veg_data <- lapply(veg_files, read.csv)
veg_data[[1]]


full_veg_data <- rbind_list(veg_data)
names(full_veg_data)


full_veg_data$year <- gsub("([0-9]{4})-.*","\\1",full_veg_data$endDate)
full_veg_data$month <- gsub(".*-([0-9]{2})-.*","\\1",full_veg_data$endDate)

full_veg_data$genus <-  gsub("([A-Za-z]+).*","\\1",full_veg_data$scientificName)

veg_survey_NEON$year <- gsub("([0-9]{4})-.*","\\1",veg_survey_NEON$endDate)
veg_survey_NEON$month <- gsub(".*-([0-9]{2})-.*","\\1",veg_survey_NEON$endDate)

veg_survey_NEON$genus <-  gsub("([A-Za-z]+).*","\\1",veg_survey_NEON$scientificName)
table(full_veg_data$genus)



just_mertensia <- full_veg_data[full_veg_data$genus=="Mertensia",]

year_plot_mertensia <- just_mertensia[with(just_mertensia,order(year,plotID)),]


names(veg_survey_NEON)
names(full_veg_data)
remove_names <- setdiff(names(veg_survey_NEON),
        names(full_veg_data))
full_veg_data <- full_veg_data%>%select(-"additionalSpecies")

full_veg_data$scale <- "ten"


no_cover_1m2 <- veg_survey_NEON%>%select(-one_of(remove_names))
no_cover_1m2$scale <- "one"
setdiff(names(full_veg_data),
        names(no_cover_1m2))

length(names(no_cover_1m2))
length(names(full_veg_data))
ten_and_1_m_veg <- rbind(full_veg_data,no_cover_1m2)


just_mertensia <- ten_and_1_m_veg[ten_and_1_m_veg$genus=="Mertensia",]

year_plot_mertensia <- just_mertensia[with(just_mertensia,order(year,plotID)),]

frame <- unique(ten_and_1_m_veg[,c("decimalLatitude","decimalLongitude","plotID","subplotID","endDate")])
frame$PA <- 0
frame$exists <- 0

genera <- unique(ten_and_1_m_veg$genus)

ros <- nrow(frame)

genus_PA_list <- list()

for(genus in genera){
  just_current <- ten_and_1_m_veg[ten_and_1_m_veg$genus==genus,]
  temp <- frame
  for(row in 1:ros){
    matches <-  which(just_current$plotID==temp[row,]$plotID&just_current$subplotID==temp[row,]$subplotID&
                        just_current$endDate==temp[row,]$endDate&just_current$genus==genus)
    
    num <- length(matches)
    if(num!=0){
      temp[row,]$PA <- 1
    }
  }
  genus_PA_list[[paste(genus)]] <- temp
}

just_mertensia_PA <- genus_PA_list[["Mertensia"]]

just_mertensia_PA$year <- gsub("([0-9]{4})-.*","\\1",just_mertensia_PA$endDate)


mean(just_mertensia_PA$PA)

which(just_mertensia_PA$PA==1)

library(rstanarm)

year_plot_stan <- stan_glmer(PA ~ 1 + as.numeric(year) + (1|plotID) +(plotID|subplotID),data=just_mertensia_PA, family=binomial)


print(summary(year_plot_stan)[,c("mean","sd","n_eff","Rhat")],digits=3)

launch_shinystan(year_plot_stan)


#alright so now the real problem arises, which is that these are technically just presence only. so how do i convert
#these to either presence absence based on sites sampled, or use them with background points? hmm...

#also now worried that if say a species was present in 1 m2 subplot, they may not have recorded it as present in 10m2? cuz
#they technically don't have to from what I can tell in the protocol...ugh.

#also unfortunately looks like our lat long is just for the plot not for subplots. Might be able to figure the actual location
#out using the info there, based on the corner it shouldve been taken from. so basically, each plot is 400 m2, and subdivided
#into 4 100 m2 subplots, each numbered 40,41,31,32, going left to right from top left. 10 m2 and 1 m2 plots are then
#placed in corners of the 100 m2 subplot and numbered based on the corner they fall in. 1 = bottom left, 2= bottom right,
#3 = top left, 4 = top right. Real question is, where is the GPS coordinate actually taken from? Is it from center of plot
#or corner?

#options:
  # 1 - find plots that were in fact sampled but that do not contain Mertensia. make these absences. Might have a bit of
#        code for this already? dunno. the idea would be though that i have every combination of plot, subplot and year.
        #then we just get in there right? we just like...we just go for it?

plot(full_veg_data$decimalLatitude~full_veg_data$decimalLongitude)

#gonna try and convert our vegetation survey to a spatial df

spat_neon_veg <- SpatialPointsDataFrame(veg_survey_NEON[,6:5],
                                        veg_survey_NEON)

r <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs(spat_neon_veg) <- r

first_dat <- read.csv("NEON.D13.NIWO.DP1.00006.001.2020-02.expanded.20200304T221208Z/NEON.D13.NIWO.DP1.00006.001.900.000.030.PRIPRE_30min.2020-02.expanded.20200304T221208Z.csv")
names(first_dat)
nrow(first_dat)

sensor_positions <- read.csv("NEON.D13.NIWO.DP1.00006.001.2020-02.expanded.20200304T221208Z/NEON.D13.NIWO.DP1.00006.001.sensor_positions.20200304T221208Z.csv")
names(sensor_positions)
head(sensor_positions)


sensor_positions_2 <- read.csv("NEON.D13.NIWO.DP1.00006.001.2020-01.expanded.20200209T005903Z/NEON.D13.NIWO.DP1.00006.001.sensor_positions.20200209T005903Z.csv")
head(sensor_positions_2)



aspect_33 <- raster("../NEON_slope-and-aspect---lidar/only_diane/NEON_D13_NIWO_DP3_449000_4433000_aspect.tif")
plot(aspect_33)

aspect_34 <- raster("../NEON_slope-and-aspect---lidar/only_diane/NEON_D13_NIWO_DP3_449000_4434000_aspect.tif")

plot(aspect_34)

total_aspect <- merge(aspect_33,aspect_34)

plot(total_aspect)

slope_33 <- raster("../NEON_slope-and-aspect---lidar/only_diane/NEON_D13_NIWO_DP3_449000_4433000_slope.tif")
slope_34 <- raster("../NEON_slope-and-aspect---lidar/only_diane/NEON_D13_NIWO_DP3_449000_4434000_slope.tif")
total_slope <- merge(slope_33,slope_34)
plot(total_slope)



canopy_moist_33 <- raster("../NEON_canopy-water-content---mosaic/just_diane/NEON_D13_NIWO_DP3_449000_4433000_WaterIndices/NEON_D13_NIWO_DP3_449000_4433000_MSI.tif")
canopy_moist_34 <- raster("../NEON_canopy-water-content---mosaic/just_diane/NEON_D13_NIWO_DP3_449000_4434000_WaterIndices/NEON_D13_NIWO_DP3_449000_4434000_MSI.tif")
canopy_moist_45 <- raster("../NEON_canopy-water-content---mosaic/just_diane/NEON_D13_NIWO_DP3_450000_4433000_WaterIndices/NEON_D13_NIWO_DP3_450000_4433000_MSI.tif")
total_can <- merge(canopy_moist_33,canopy_moist_34, canopy_moist_45)

plot(total_can)
crop_canopy_MSI <- crop(total_can, extent(diane_reproject)+20)
plot(crop_canopy_MSI)
plot(diane_reproject,add=T)

diane_reproject <- spTransform(oh_diane, crs(slope_33))


#talked a bit about the plot/subplot up above, but i'll mention here too. Seems like
#for the percent cover data which is what we have here, the sampling took place in
#8 1 square meter subplots for each plot. There are only 6 levels to subplotID tho
#so honestly who knows what the structure of this really is??
neon_veg_repro <- spTransform(spat_neon_veg, crs(total_can))
plot(total_can)
plot(just_geodude,add=T, pch=1)
plot(neon_veg_repro, add=T, pch=1)

plot_subplot_combos <- unique(neon_veg_repro@data[,c("plotID", "subplotID")])

plot_subplot_ordered <- plot_subplot_combos[order(plot_subplot_combos$plotID),]

neon_site_locs <- unique(neon_veg_repro@data[,c("decimalLatitude","decimalLongitude")])

unique(c(neon_veg_repro$decimalLatitude,neon_veg_repro$decimalLongitude))

table(neon_veg_repro$endDate)

just_geodude <- neon_veg_repro[neon_veg_repro$genus=="Geum",]

names(neon_veg_repro)

#okay so twelve plots total with 6 subplots in each - except for plot 18 which has 5.

#subplots are incorrectly labeled at the highest level, but the second two numbers are correct.
unique(neon_veg_repro$scientificName)
unique(neon_veg_repro$plotID)
unique(neon_veg_repro$subplotID)


neon_veg_repro$genus <- gsub("([A-Za-z]+).*","\\1",neon_veg_repro$scientificName)

table(neon_veg_repro$genus)

crp_slope <- crop(total_slope,extent(diane_reproject)+20)
plot(crp_slope)
plot(diane_reproject, add=T)

