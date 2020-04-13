
#Gonna try and read in a lot of sensor data

library(raster)
library(dplyr)
library(ggplot2)
library(rstanarm)
#I've plotted a lot of these using Diane Ebert May's data, but keep in mind
#there is no temporal overlap between when the majority of this sensor/Lidar data
#was collected and when Ebert May's sampling took place. Last Ebert May survey was
#in 2011 and i think the earliest NEON data I've seen was with like 2013 or something.

setwd("../NEON.Data/NEON_precipitation/")



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


plot(x=full_veg_data$decimalLongitude,y=full_veg_data$decimalLatitude)
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

#this should give us all the unique combinations of latlong, plot, subplot, and enddate that are present in the data. I
#think. Really hoping this isn't just a roundabout way to expand grid...

frame <- unique(ten_and_1_m_veg[,c("decimalLatitude","decimalLongitude","plotID","subplotID","endDate")])
frame$PA <- 0

#I made this little exists variable to make sure we hadn't generated any combinations of latlong, plot, subplot, and date
#that didn't actually exist in the data. From what i can tell, they're all there. You can check it again in the loop below
#just adding a little conditional to see if the current row combination exists in the ten_and_1_m_veg data, rather
#than using the just_current for genus specific presence/absence.
frame$exists <- 0

genera <- unique(ten_and_1_m_veg$genus)

ros <- nrow(frame)

genus_PA_list <- list()

for(genus in genera){
  just_current <- ten_and_1_m_veg[ten_and_1_m_veg$genus==genus,]
  temp <- frame
  for(row in 1:ros){
    matches <-  which(just_current$plotID==temp[row,]$plotID&just_current$subplotID==temp[row,]$subplotID&
                        just_current$endDate==temp[row,]$endDate)
    
    num <- length(matches)
    if(num!=0){
      temp[row,]$PA <- 1
    }
  }
  genus_PA_list[[paste(genus)]] <- temp
}

just_mertensia_PA <- genus_PA_list[["Mertensia"]]

neon_prairie_fire <- genus_PA_list[["Castilleja"]]

neon_fir <- genus_PA_list[["Abies"]]
just_mertensia_PA$year <- gsub("([0-9]{4})-.*","\\1",just_mertensia_PA$endDate)
neon_prairie_fire$year <- gsub("([0-9]{4})-.*","\\1",neon_prairie_fire$endDate)
neon_fir$year <-  gsub("([0-9]{4})-.*","\\1",neon_fir$endDate)
neon_fir$month_year <- gsub("([0-9]{4}-[0-9]{2}).*","\\1",neon_fir$endDate)


spatial_just_mertensia <- SpatialPointsDataFrame(just_mertensia_PA[,c(2:1)],just_mertensia_PA)


  
r <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs(spatial_just_mertensia) <- r



plot(spatial_just_mertensia[spatial_just_mertensia$PA==1,], pch=16, col="blue")
points(spatial_just_mertensia[spatial_just_mertensia$PA==0,], pch=16, col="yellow")

mert_gg <- ggplot(just_mertensia_PA)+geom_point(aes(x=decimalLongitude,y=decimalLatitude,
                                                    color=PA))+
  facet_wrap(~year)

mert_gg


fire_gg<- ggplot(neon_prairie_fire)+geom_point(aes(x=decimalLongitude,y=decimalLatitude,
                                                    color=PA))+
  facet_wrap(~year)

fire_gg


fir_gg <- ggplot(neon_fir)+geom_point(aes(x=decimalLongitude,y=decimalLatitude,
                                                   color=PA))+
  facet_wrap(~month_year)

fir_gg


spatial_prairie_fire <- SpatialPointsDataFrame(neon_prairie_fire[,2:1],neon_prairie_fire)


crs(total_can)
r2 <- "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(spatial_prairie_fire) <- r

transformed_prairie_fire <- spTransform(spatial_prairie_fire,crs(total_can))

plot(x=spatial_prairie_fire$decimalLongitude,y=spatial_prairie_fire$decimalLatitude)

neon_prairie_fire$canopy_moisture <- extract(total_can, spatial_prairie_fire[,2:1])

plot(total_can)
plot(x=transformed_prairie_fire$decimalLongitude,y=transformed_prairie_fire$decimalLatitude)

mean(just_mertensia_PA$PA)

mean(neon_prairie_fire$PA)

which(just_mertensia_PA$PA==1)

library(rstanarm)

year_plot_stan <- stan_glmer(PA ~ 1 + as.numeric(year) + (1|plotID),data=neon_prairie_fire, family=binomial)


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


#flights for canopy moisture were in September 2017, August 2018, August 2019

setwd("~/Desktop/Project/Niwot_SDM/NEON.Data/2017_NEON_canopy-water-content---mosaic/MSI/")

seventeen_files <- list.files()

seventeen_raster_list <- lapply(seventeen_files, raster)

seventeen_raster_list$fun <- mean
seventeen_raster_list$na.rm <- TRUE

full_MSI_raster_17 <- do.call(mosaic,seventeen_raster_list)
plot(full_MSI_raster_17)

seven_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2017,]
points(x=seven_prairie_fires$decimalLongitude, y=seven_prairie_fires$decimalLatitude, col=seven_prairie_fires$PA)
MSI_2017 <- data.frame(extract(full_MSI_raster_17, seven_prairie_fires))
names(MSI_2017) <- "MSI"
MSI_2017$year <- 2017
MSI_2017[,c("Easting","Northing")] <- c(seven_prairie_fires$decimalLongitude, seven_prairie_fires$decimalLatitude)
MSI_2017$PA <- seven_prairie_fires$PA

msi_17_model <- glm(PA~MSI, data=MSI_2017, family=binomial)
summary(msi_17_model)

setwd("~/Desktop/Project/Niwot_SDM/NEON.Data/2018_NEON_canopy-water-content---mosaic/MSI/")



eighteen_files <- list.files()

eighteen_raster_list <- lapply(eighteen_files, raster)

eighteen_raster_list$fun <- mean
eighteen_raster_list$na.rm <- TRUE

full_MSI_raster_18 <- do.call(mosaic,eighteen_raster_list)
plot(full_MSI_raster_18)
eight_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2018,]
points(x=eight_prairie_fires$decimalLongitude, y=eight_prairie_fires$decimalLatitude, col=eight_prairie_fires$PA)
MSI_2018 <- data.frame(extract(full_MSI_raster_18, eight_prairie_fires))
names(MSI_2018) <- "MSI"
MSI_2018$year <- 2018
MSI_2018[,c("Easting","Northing")] <- c(eight_prairie_fires$decimalLongitude, eight_prairie_fires$decimalLatitude)
MSI_2018$PA <- eight_prairie_fires$PA

pch_lookup <- c("0"=21, "1"=22)
points(x=jitter(MSI_2018$Easting,factor=500),y=jitter(MSI_2018$Northing, factor=5), bg=MSI_2018$MSI*10, pch=pch_lookup[as.character(MSI_2018$PA)])

msi_18_model <- glm(PA~MSI, data=MSI_2018, family=binomial)
summary(msi_18_model)

setwd("~/Desktop/Project/Niwot_SDM/NEON.Data/2019_NEON_canopy-water-content---mosaic/MSI/")

nineteen_files <- list.files()

nineteen_raster_list <- lapply(nineteen_files, raster)

nineteen_raster_list$fun <- mean
nineteen_raster_list$na.rm <- TRUE

full_MSI_raster_19 <- do.call(mosaic,nineteen_raster_list )
plot(full_MSI_raster_19)

nine_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2019,]
points(x=nine_prairie_fires$decimalLongitude, y=nine_prairie_fires$decimalLatitude, col=nine_prairie_fires$PA)

MSI_2019 <- data.frame(extract(full_MSI_raster_19, nine_prairie_fires))
names(MSI_2019) <- "MSI"
MSI_2019$year <- 2019
MSI_2019[,c("Easting","Northing")] <- c(nine_prairie_fires$decimalLongitude, nine_prairie_fires$decimalLatitude)
MSI_2019$PA <- nine_prairie_fires$PA
points(x=MSI_2019$Easting,y=MSI_2019$Northing, col=MSI_2019$PA, cex=MSI_2019$MSI)


msi_19_model <- glm(PA~MSI, data=MSI_2019, family=binomial)
summary(msi_19_model)
#this seems to be our actual moisture values...or something like it? I dunno. Rasters are 
#weird. Anyways.
total_can@data@values

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

plot(y=neon_veg_repro$decimalLatitude,x=neon_veg_repro$decimalLongitude)

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



xs <- seq(449000,453000,by=1000)
ys <- seq(4431000,4434000,by=1000)
xy <- expand.grid(xs,ys)
write.csv(xy,"en.combos.txt", row.names = F, col.names = F)



