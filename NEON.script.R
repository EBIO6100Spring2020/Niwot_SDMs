
#Gonna try and read in a lot of sensor data

library(raster)
library(dplyr)
library(ggplot2)
library(rstanarm)
library(rstan)
#I've plotted a lot of these using Diane Ebert May's data, but keep in mind
#there is no temporal overlap between when the majority of this sensor/Lidar data
#was collected and when Ebert May's sampling took place. Last Ebert May survey was
#in 2011 and i think the earliest NEON data I've seen was with like 2013 or something.

#this should be the working directory for the project itself, but had a lot of dumb problems with that not working, so here
#is the basic one.
setwd("/Users/aaronwestmoreland/Desktop/Project/Niwot_SDM/Niwot_SDMs")
###### Read in Abio Rasters #####


setwd("./NEON.Data/Moisture.Indices/")
index.files <- list.files()

for(file in index.files){
  name <- gsub("[0-9]{4}.([A-Z]{3,4}).tif","\\1",file)
  year <- gsub("([0-9]{4}).[A-Z]{3,4}.tif","\\1",file)
  full_name <- paste(name,year,sep = ".")

  assign(paste(full_name),raster(file))
}

plot(MDI.2017)


setwd("../Aspect/")
aspect <- raster("Aspect.tif")
setwd("../Slope/")
slope <- raster("Slope.tif")

##### read in NEON veg surveys ####


#so for the veg survey, this is the percent cover, taken at a 1 square meter resolution
#for 8 subplots in each plot. The relationship between the numbers in plot v subplot
#is a bit opaque to me, but im sure its sensible. Probably. Who knows.

#ayyy got all that data in, took some doin but managed it. okay. now let's clean it a bit, first by splitting our dates,
#then by pulling out just deschampsia.
setwd("./NEON.Data/")
one_veg_files <- list.files(path = "./1m.veg.PA/", pattern = "*.csv")
one_veg_files <- paste0("./1m.veg.PA/",one_veg_files)
one_veg_data <- lapply(one_veg_files, read.csv)
one_veg_data[[1]]


one_full_veg_data <- rbind_list(one_veg_data)
rm(one_veg_data)



#this is actually 10 and 100 m data!! be aware of that. Data with just a single integer for the subplot is from the 100m2
#survey. I'll talk with michael about how to handle that.
ten_veg_files <- list.files(path = "./10m.veg.PA/", pattern = "*.csv")
ten_veg_files <- paste0("./10m.veg.PA/",ten_veg_files)
ten_veg_data <- lapply(ten_veg_files, read.csv)
ten_veg_data[[1]]


ten_full_veg_data <- rbind_list(ten_veg_data)
rm(ten_veg_data)
names(ten_full_veg_data)


ten_full_veg_data$year <- gsub("([0-9]{4})-.*","\\1",ten_full_veg_data$endDate)
ten_full_veg_data$month <- gsub(".*-([0-9]{2})-.*","\\1",ten_full_veg_data$endDate)

ten_full_veg_data$genus <-  gsub("([A-Za-z]+).*","\\1",ten_full_veg_data$scientificName)

one_full_veg_data$year <- gsub("([0-9]{4})-.*","\\1",one_full_veg_data$endDate)
one_full_veg_data$month <- gsub(".*-([0-9]{2})-.*","\\1",one_full_veg_data$endDate)

one_full_veg_data$genus <-  gsub("([A-Za-z]+).*","\\1",one_full_veg_data$scientificName)
table(one_full_veg_data$genus)


plot(x=full_veg_data$decimalLongitude,y=full_veg_data$decimalLatitude)



names(one_full_veg_data)
names(ten_full_veg_data)
remove_names <- setdiff(names(one_full_veg_data),
        names(ten_full_veg_data))
setdiff(names(ten_full_veg_data), names(one_full_veg_data))
ten_full_veg_data <- ten_full_veg_data%>%select(-"additionalSpecies")

ten_full_veg_data$scale <- "ten"


no_cover_1m2 <- one_full_veg_data%>%select(-one_of(remove_names))
no_cover_1m2$scale <- "one"
setdiff(names(ten_full_veg_data),
        names(no_cover_1m2))

length(names(no_cover_1m2))
length(names(ten_full_veg_data))
ten_one_100_veg_survey<- rbind(ten_full_veg_data,no_cover_1m2)




#this should give us all the unique combinations of latlong, plot, subplot, and enddate that are present in the data. I
#think. Really hoping this isn't just a roundabout way to expand grid...

frame <- unique(ten_one_100_veg_survey[,c("decimalLatitude","decimalLongitude","plotID","subplotID","endDate")])
frame$PA <- 0

#I made this little exists variable to make sure we hadn't generated any combinations of latlong, plot, subplot, and date
#that didn't actually exist in the data. From what i can tell, they're all there. You can check it again in the loop below
#just adding a little conditional to see if the current row combination exists in the ten_one_100_veg_survey data, rather
#than using the just_current for genus specific presence/absence.
frame$exists <- 0

genera <- unique(ten_one_100_veg_survey$genus)

ros <- nrow(frame)

genus_PA_list <- list()

for(genus in genera){
  just_current <- ten_one_100_veg_survey[ten_one_100_veg_survey$genus==genus,]
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


neon_prairie_fire <- genus_PA_list[["Castilleja"]]


neon_prairie_fire$year <- gsub("([0-9]{4})-.*","\\1",neon_prairie_fire$endDate)







fire_gg<- ggplot(neon_prairie_fire)+geom_point(aes(x=decimalLongitude,y=decimalLatitude,
                                                    color=PA))+
  facet_wrap(~year)

fire_gg





neon_prairie_fire$top_sub <- gsub("([0-9]{2}).[0-9].[0-9]{1,2}","\\1",neon_prairie_fire$subplotID)
neon_prairie_fire$mid_sub <- gsub("[0-9]{2}.([0-9]).[0-9]{1,2}","\\1",neon_prairie_fire$subplotID)



spatial_prairie_fire <- SpatialPointsDataFrame(neon_prairie_fire[,2:1],neon_prairie_fire)
r <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs(spatial_prairie_fire) <- r

transformed_prairie_fire <- spTransform(spatial_prairie_fire,crs(full_MSI_raster_17))
ros2 <- nrow(transformed_prairie_fire)


for(j in 1:ros2){
  if(transformed_prairie_fire$top_sub[j]==31){
    
    if(transformed_prairie_fire$mid_sub[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-5
       transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-5
    }else if(transformed_prairie_fire$mid_sub[j]==1){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-10
    }else if (transformed_prairie_fire$mid_sub[j]==2){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude
    }else if (transformed_prairie_fire$mid_sub[j]==3){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-10
     }else if(transformed_prairie_fire$mid_sub[j]==4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
       transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }
  }else if (transformed_prairie_fire$top_sub[j]==32){
    if(transformed_prairie_fire$mid_sub[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-5
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+5
    }else if(transformed_prairie_fire$mid_sub[j]==1){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if (transformed_prairie_fire$mid_sub[j]==2){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+10
    }else if (transformed_prairie_fire$mid_sub[j]==3){
       transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
       transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$mid_sub[j]==4){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+10
    }
  }else if(transformed_prairie_fire$top_sub[j]==40){
    if(transformed_prairie_fire$mid_sub[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]+5
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-5
    }else if(transformed_prairie_fire$mid_sub[j]==1){
     transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-10
    }else if (transformed_prairie_fire$mid_sub[j]==2){
     transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
     transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if (transformed_prairie_fire$mid_sub[j]==3){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]-10
    }else if(transformed_prairie_fire$mid_sub[j]==4){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]
    }
  }else if (transformed_prairie_fire$top_sub[j]==42){
    if(transformed_prairie_fire$mid_sub[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]+5
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+5
    }else if(transformed_prairie_fire$mid_sub[j]==1){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if (transformed_prairie_fire$mid_sub[j]==2){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]+10
    }else if (transformed_prairie_fire$mid_sub[j]==3){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$mid_sub[j]==4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+10
    }
  }
  
}



plot(x=transformed_prairie_fire$Easting,y=transformed_prairie_fire$Northing)


plot(x=neon_prairie_fire$decimalLongitude,y=spatial_prairie_fire$decimalLatitude)




mean(neon_prairie_fire$PA)



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


plot(full_MSI_raster_17)

seven_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2017,]
points(x=seven_prairie_fires$Easting, y=seven_prairie_fires$Northing, col=seven_prairie_fires$PA)

MSI_2017 <- data.frame(extract(full_MSI_raster_17, c(seven_prairie_fires$Northing, seven_prairie_fires$Easting)))
names(MSI_2017) <- "MSI"
MSI_2017$year <- 2017
MSI_2017$Easting <- seven_prairie_fires$Easting
MSI_2017$Northing <- seven_prairie_fires$Northing
MSI_2017$PA <- seven_prairie_fires$PA
MSI_2017$Plot <- as.factor(seven_prairie_fires$plotID)



setwd("~/Desktop/Project/Niwot_SDM/NEON.Data/2018_NEON_canopy-water-content---mosaic/MSI/")


eighteen_files <- list.files()

eighteen_raster_list <- lapply(eighteen_files, raster)

eighteen_raster_list$fun <- mean
eighteen_raster_list$na.rm <- TRUE

full_MSI_raster_18 <- do.call(mosaic,eighteen_raster_list)
plot(full_MSI_raster_18)
eight_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2018,]
points(x=eight_prairie_fires$decimalLongitude, y=eight_prairie_fires$decimalLatitude, col=eight_prairie_fires$PA)
MSI_2018 <- data.frame(extract(full_MSI_raster_18, c(eight_prairie_fires$Northing, eight_prairie_fires$Easting)))
names(MSI_2018) <- "MSI"
MSI_2018$year <- 2018
MSI_2018$Easting <- eight_prairie_fires$Easting
MSI_2018$Northing <- eight_prairie_fires$Northing
MSI_2018$PA <- eight_prairie_fires$PA
MSI_2018$Plot <- eight_prairie_fires$plotID



setwd("~/Desktop/Project/Niwot_SDM/NEON.Data/2019_NEON_canopy-water-content---mosaic/MSI/")

nineteen_files <- list.files()

nineteen_raster_list <- lapply(nineteen_files, raster)

nineteen_raster_list$fun <- mean
nineteen_raster_list$na.rm <- TRUE

full_MSI_raster_19 <- do.call(mosaic,nineteen_raster_list )
plot(full_MSI_raster_19)

nine_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2019,]
points(x=nine_prairie_fires$decimalLongitude, y=nine_prairie_fires$decimalLatitude, col=nine_prairie_fires$PA)

MSI_2019 <- data.frame(extract(full_MSI_raster_19, c(nine_prairie_fires$Northing,nine_prairie_fires$Easting)))
names(MSI_2019) <- "MSI"
MSI_2019$year <- 2019
MSI_2018$Easting <- eight_prairie_fires$Easting
MSI_2018$Northing <- eight_prairie_fires$Northing
MSI_2019$PA <- nine_prairie_fires$PA
MSI_2019$Plot <- nine_prairie_fires$plotID
points(x=MSI_2019$Easting,y=MSI_2019$Northing, col=MSI_2019$PA, cex=MSI_2019$MSI)



?#talked a bit about the plot/subplot up above, but i'll mention here too. Seems like
#for the percent cover data which is what we have here, the sampling took place in
#8 1 square meter subplots for each plot. There are only 6 levels to subplotID tho
#so honestly who knows what the structure of this really is??
  
  



plot(full_slope_raster)
plot(full_aspect_raster)


all_years <- rbind(seven_prairie_fires,eight_prairie_fires,nine_prairie_fires)

all_years$slope <- extract()


raster::extract(full_MSI_raster_19, c(nine_prairie_fires$Northing,nine_prairie_fires$Easting))







plot(x=MSI_2017$Easting, y=MSI_2017$Northing)

nrow(MSI_2017)


msi_17_model <- glm(PA~MSI, data=MSI_2017, family=binomial)
summary(msi_17_model)

stan_17_msi <- stan_glmer(PA~1+MSI+(1|Plot), data=MSI_2017,family="binomial")
launch_shinystan(stan_17_msi)






msi_19_model <- glm(PA~MSI, data=MSI_2019, family=binomial)
summary(msi_19_model)


stan_19_msi <- stan_glmer(PA~1+MSI+(1|Plot), data=MSI_2019, family="binomial")


launch_shinystan(stan_19_msi)


pch_lookup <- c("0"=21, "1"=22)
points(x=jitter(MSI_2018$Easting,factor=500),y=jitter(MSI_2018$Northing, factor=5), bg=MSI_2018$MSI*10, pch=pch_lookup[as.character(MSI_2018$PA)])

msi_18_model <- glm(PA~MSI, data=MSI_2018, family=binomial)
summary(msi_18_model)

stan_msi_18 <- stan_glmer(PA~1+MSI+(1|Plot), data=MSI_2018,family="binomial")
launch_shinystan(stan_msi_18)
summary(stan_msi_18)




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



