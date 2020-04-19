
#Gonna try and read in a lot of sensor data
library(stringr)
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

moisture_list <- list()

for(file in index.files){
  name <- gsub("[0-9]{4}.([A-Z]{3,4}).tif","\\1",file)
  year <- gsub("([0-9]{4}).[A-Z]{3,4}.tif","\\1",file)
  full_name <- paste(name,year,sep = ".")

  
  moisture_list[[paste(full_name)]] <- raster(file)
  #assign(paste(full_name),raster(file))
}


plot(MDI.2017)


setwd("../Aspect/")
aspect <- raster("Aspect.tif")
setwd("../Slope/")
slope <- raster("Slope.tif")

setwd("../Nitrogen_Indices/")

nit.files <-  list.files()


# bad_files <- grep("[A-Z]{3,4}.[0-9]{4}.tif.[A-Z]{3,4}.[0-9]{4}.tif", names(.GlobalEnv),value=T)
# rm(list=bad_files)
nitro_list <- list()
for(file in nit.files){
  name <- gsub("([A-Z]{3,4}).[0-9]{4}.tif","\\1",file)
  year <- gsub("[A-Z]{3,4}.([0-9]{4}).tif","\\1",file)
  full_name <- paste(name,year,sep = ".")
  
  nitro_list[[paste(full_name)]] <- raster(file)
  
}




##### read in NEON veg surveys ####


#so for the veg survey, this is the percent cover, taken at a 1 square meter resolution
#for 8 subplots in each plot. The relationship between the numbers in plot v subplot
#is a bit opaque to me, but im sure its sensible. Probably. Who knows.

#ayyy got all that data in, took some doin but managed it. okay. now let's clean it a bit, first by splitting our dates,
#then by pulling out just deschampsia.
setwd("../")
one_veg_files <- list.files(path = "./1m.veg.PA/", pattern = "*.csv")
one_veg_files <- paste0("./1m.veg.PA/",one_veg_files)
one_veg_data <- lapply(one_veg_files, read.csv)
one_veg_data[[1]]


one_full_veg_data <- rbind_list(one_veg_data)
rm(one_veg_data)

#this is actually 10 and 100 m data!! be aware of that. Data with just a single integer for the subplot is from the 100m2
#survey. I'll talk with michael about how to handle that.


# PROBLEM HERE. MAKE SURE YOU FIX THAT WE CAN HAVE AN ABSENCE RECORDED AT 1 M, THEN A PRESENCE AT 10 M. WHEN WE AGGREGATE
#THIS TO 10 M PLOTS, THAT LOOKS LIKE ONE ABIO VALUE HAVING BOTH AN ABSENCE AND A PRESENCE. PLUS THAT VALUE IS REPLICATED.
#SO NEED TO RESTRUCTURE THE PA TABLE CONSTRUCTION TO JUST GIVE US PRESENCE/ABSENCE AT THE TEN METER SUBLOT. BASICALLY
#IT SHOULD CHECK IF THE .1 PLOT SAW IT, THEN IF THE .10 PLOT SAW IT. IF ITS YES IN 1, DONT CHECK 10. IF ITS NO IN 1, CHECK
#THEN THEN SAY YES OR NO DEPENDING. DON'T WORRY ABOUT 100 METER FOR NOW. 
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



ten_one_100_veg_survey$mid_sub<- gsub("([0-9]{2}.[0-9]).[0-9]{1,2}","\\1",ten_one_100_veg_survey$subplotID)

#this should give us all the unique combinations of latlong, plot, subplot, and enddate that are present in the data. I
#think. Really hoping this isn't just a roundabout way to expand grid...

frame <- unique(ten_one_100_veg_survey[,c("decimalLatitude","decimalLongitude","plotID","mid_sub","endDate")])
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
    matches <-  which(just_current$plotID==temp[row,]$plotID&just_current$mid_sub==temp[row,]$mid_sub&
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





neon_prairie_fire$top_sub <- gsub("([0-9]{2}).[0-9]","\\1",neon_prairie_fire$mid_sub)
neon_prairie_fire$middle <- gsub("[0-9]{2}.([0-9])","\\1",neon_prairie_fire$mid_sub)



spatial_prairie_fire <- SpatialPointsDataFrame(neon_prairie_fire[,2:1],neon_prairie_fire)
r <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs(spatial_prairie_fire) <- r

transformed_prairie_fire <- spTransform(spatial_prairie_fire,crs(nitro_list[[1]]))
ros2 <- nrow(transformed_prairie_fire)


for(j in 1:ros2){
  if(transformed_prairie_fire$top_sub[j]==31){
    
    if(transformed_prairie_fire$middle[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
       transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$middle[j]==1){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-10
    }else if (transformed_prairie_fire$middle[j]==2){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude
    }else if (transformed_prairie_fire$middle[j]==3){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-10
     }else if(transformed_prairie_fire$middle[j]==4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
       transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }
  }else if (transformed_prairie_fire$top_sub[j]==32){
    if(transformed_prairie_fire$middle[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$middle[j]==1){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if (transformed_prairie_fire$middle[j]==2){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]-10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+10
    }else if (transformed_prairie_fire$middle[j]==3){
       transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
       transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$middle[j]==4){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+10
    }
  }else if(transformed_prairie_fire$top_sub[j]==40){
    if(transformed_prairie_fire$middle[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$middle[j]==1){
     transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]-10
    }else if (transformed_prairie_fire$middle[j]==2){
     transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
     transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if (transformed_prairie_fire$middle[j]==3){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]-10
    }else if(transformed_prairie_fire$middle[j]==4){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]
    }
  }else if (transformed_prairie_fire$top_sub[j]==42){
    if(transformed_prairie_fire$middle[j]>4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$middle[j]==1){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]
    }else if (transformed_prairie_fire$middle[j]==2){
      transformed_prairie_fire$Northing[j]<- transformed_prairie_fire$decimalLatitude[j]
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]+10
    }else if (transformed_prairie_fire$middle[j]==3){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j]<- transformed_prairie_fire$decimalLongitude[j]
    }else if(transformed_prairie_fire$middle[j]==4){
      transformed_prairie_fire$Northing[j] <- transformed_prairie_fire$decimalLatitude[j]+10
      transformed_prairie_fire$Easting[j] <- transformed_prairie_fire$decimalLongitude[j]+10
    }
  }
  
}



transf_100 <- transformed_prairie_fire
transf_10 <- transformed_prairie_fire[as.numeric(transformed_prairie_fire$middle)<5,]



plot(x=transformed_prairie_fire$Easting,y=transformed_prairie_fire$Northing)






nit_17 <- nitro_list[grep("2017", names(nitro_list))]
nit_18 <- nitro_list[grep("2018", names(nitro_list))]
nit_19 <- nitro_list[grep("2019", names(nitro_list))]

mo_17 <- moisture_list[grep("2017",names(moisture_list))]
mo_18 <- moisture_list[grep("2018",names(moisture_list))]
mo_19 <- moisture_list[grep("2019",names(moisture_list))]


twenty_seven <- append(nit_17,mo_17)
twenty_eight <- append(nit_18,mo_18)
twenty_nine <- append(nit_19,mo_19)
length(twenty_seven)
# 
# seven_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2017,]
# eight_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2018,]
# nine_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2019,]
# 
# 
# df_all_17 <- data.frame(Easting=seven_prairie_fires$Easting, Northing=seven_prairie_fires$Northing,
#                         PA=seven_prairie_fires$PA, plot=seven_prairie_fires$plotID,
#                         subplot=seven_prairie_fires$subplotID,
#                         year=2017)
# df_all_18 <- data.frame(Easting=eight_prairie_fires$Easting, Northing=eight_prairie_fires$Northing,
#                         PA=eight_prairie_fires$PA, plot=eight_prairie_fires$plotID,
#                         subplot=eight_prairie_fires$subplotID,
#                         year=2018)
# df_all_19 <- data.frame(Easting=nine_prairie_fires$Easting, Northing=nine_prairie_fires$Northing,
#                         PA=nine_prairie_fires$PA, plot=nine_prairie_fires$plotID,
#                         subplot=nine_prairie_fires$subplotID,
#                         year=2019)



trans_10_only_171819 <- transf_10[transf_10$year>2016,]


big_df <- data.frame(Easting=trans_10_only_171819$Easting, Northing=trans_10_only_171819$Northing,
                     PA=trans_10_only_171819$PA, plot=trans_10_only_171819$plotID,
                     subplot=trans_10_only_171819$subplotID, top_sub=trans_10_only_171819$top_sub,mid_sub=trans_10_only_171819$mid_sub,
                     year=trans_10_only_171819$year,ARVI=0, EVI=0, NDLI=0, NDNI=0, NDVI=0, PRI=0,SAVI=0,NDII=0,NDWI=0,NMDI=0,MSI=0,WBI=0)




nam
levels(df_all_year$subplot)






big_row <- nrow(big_df)
big_df$year
for(row in 1:big_row){
  for(i in 9:20){
    year <- big_df$year[row]
    if(year==2017){
      #subtract 8 here because the indices for our yearly raster list is off by 8 relative to column numbers in df
      big_df[row,i] <- rextract(big_df[row,],twenty_seven[[i-8]])
    }else if(year==2018){
      big_df[row,i] <- rextract(big_df[row,],twenty_eight[[i-8]])
    }else if(year==2019){
      big_df[row,i] <- rextract(big_df[row,],twenty_nine[[i-8]])
    }
  }

}

write.csv(x = big_df, file = "Prairie.Fire.PA.w.all.index.10.m.avg.csv")

rextract <- function(dat,rast){
  if(dat$mid_sub==1){
    e <- seq(dat$Easting, dat$Easting+3,by=1)
    n <- seq(dat$Northing, dat$Northing+3,by=1)
    grid <- expand.grid(e,n)
    vals <- raster::extract(rast, grid)
    avg_val <- mean(vals)
    return(avg_val)
  }else if(dat$mid_sub==2){
    e <- seq(dat$Easting-3, dat$Easting,by=1)
    n <- seq(dat$Northing, dat$Northing+3,by=1)

    grid <- expand.grid(e,n)
    vals <- raster::extract(rast,grid)
    avg_val <- mean(vals)
    return(avg_val)
    
  }else if(dat$mid_sub==3){
    e <- seq(dat$Easting, dat$Easting+3,by=1)
    n <- seq(dat$Northing-3, dat$Northing,by=1)
    grid <- expand.grid(e,n)
    vals <- raster::extract(rast,grid)
    avg_val <- mean(vals)
    return(avg_val)
  }else if(dat$mid_sub==4){
    e <- seq(dat$Easting-3, dat$Easting,by=1)
    n <- seq(dat$Northing-3, dat$Northing,by=1)
    
    grid <- expand.grid(e,n)
    vals <- raster::extract(rast,grid)
    avg_val <- mean(vals)
    return(avg_val)
  }
}


rextract17_10 <- function(rast){
  thing <- raster::extract(rast, c(seven_prairie_fires$Easting,seven_prairie_fires$Northing))
  return(thing)
}
rextract18_10 <- function(rast){
  thing <- raster::extract(rast, c(eight_prairie_fires$Easting,eight_prairie_fires$Northing))
  return(thing)
}
rextract19_10 <- function(rast){
  thing <- raster::extract(rast, c(nine_prairie_fires$Easting,nine_prairie_fires$Northing))
  return(thing)
}

indices_17 <- data.frame(lapply(nit_17, rextract17))
indices_18 <-  data.frame(lapply(nit_18, rextract18))
indices_19 <-  data.frame(lapply(nit_19, rextract19))
mindices_17 <- data.frame(lapply(mo_17, rextract17))
mindices_18 <-  data.frame(lapply(mo_18, rextract18))
mindices_19 <-  data.frame(lapply(mo_19, rextract19))


temp_17<- cbind(indices_17,mindices_17)
temp_18<- cbind(indices_18,mindices_18)
temp_19<- cbind(indices_19,mindices_19)

nam<- gsub('([A-Z]{3,4}).[0-9]{4}','\\1',names(df_all_17[7:18]))

names(temp_17) <- nam
names(temp_18) <- nam
names(temp_19) <- nam

df_all_17 <- cbind(df_all_17,temp_17)
df_all_18 <- cbind(df_all_18,temp_18)
df_all_19 <- cbind(df_all_19,temp_19)

names(df_all_17)




df_all_year <- rbind(df_all_17,df_all_18,df_all_19)


names(df_all_17)
library(rstanarm)
names(df_all_year)

df_all_year$plot
df_all_year$subplot

year_plot_ARVI_MSI <- stan_glmer(PA~-1+as.numeric(year)+(1|plot)+(plot|subplot)+ARVI+MSI, data=df_all_year,
                                 family='binomial')


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



#flights for canopy moisture were in September 2017, August 2018, August 2019


pull_rast <- function(list,data, year){
  year_rast <- 
  year_dat <- data[data$year==year,]
  print(year_dat)
}

pull_rast(transformed_prairie_fire,2017)

seven_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2017,]

all_indices_frame_2017<- data.frame(Easting=seven_prairie_fires$Easting,
                                    Northing= seven_prairie_fires$Northing,
                                    PA=as.factor(seven_prairie_fires$PA),
                                    Plot=as.factor(seven_prairie_fires$plotID))
all_indices_frame_2017$Northing <- seven_prairie_fires$Northing
all_indices_frame_2017$PA <- seven_prairie_fires$PA
all_indices_frame_2017$Plot <- as.factor(seven_prairie_fires$plotID)
#moisture stress index
MSI_2017 <- data.frame(raster::extract(MSI.2017, c(seven_prairie_fires$Northing, seven_prairie_fires$Easting)))
names(MSI_2017) <- "MSI"
MSI_2017$year <- 2017
MSI_2017$Easting <- seven_prairie_fires$Easting
MSI_2017$Northing <- seven_prairie_fires$Northing
MSI_2017$PA <- seven_prairie_fires$PA
MSI_2017$Plot <- as.factor(seven_prairie_fires$plotID)
#normalized multi-band drought index

obs <- grep("[A-Z]{3,4}\\.2017", names(.GlobalEnv), value=T)
ob_list <- do.call(list, mget(obs))


for(object in ob_list){
  
  ind.year <- names(object)
  just_ind <- gsub("([A-Z]{3,4}")
  
  assign(paste(just_ind), raster::extract())
  
  }



MSI_2017$NMDI <- raster::extract(MDI.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))
#normalized difference infrared index
MSI_2017$NDII <- raster::extract(DII.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))
#normalized difference water index
MSI_2017$NDWI <- raster::extract(DWI.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))
#water band index
MSI_2017$WBI <- raster::extract(WBI.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))

MSI_2017$ARVI <- raster::extract(ARVI.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))

MSI_2017$EVI <- raster::extract(EVI.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))
MSI_2017$WBI <- raster::extract(WBI.2017,c(seven_prairie_fires$Northing, seven_prairie_fires$Easting))

# SAVI, PRI, NDVI, NDNI, NDLI, ARVI, EVI


mod <- lm(MSI_2017$MSI~MSI_2017$PA)
mod_nm <- lm(MSI_2017$NMDI~MSI_2017$PA)
#NOPE for NMDI
summary(mod_nm)
summary(mod)

mod_ndii <- lm(MSI_2017$NDII~MSI_2017$PA)
summary(mod_ndii)

mod_asp <- lm(MSI_2017$aspect~-1+as.factor(MSI_2017$PA))
summary(mod_asp)

cor(MSI_2017[,c(1,5,7,8,9,10)])



eight_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2018,]
MSI_2018 <- data.frame(raster::extract(MSI.2018, c(eight_prairie_fires$Northing, eight_prairie_fires$Easting)))
names(MSI_2018) <- "MSI"
MSI_2018$year <- 2018
MSI_2018$Easting <- eight_prairie_fires$Easting
MSI_2018$Northing <- eight_prairie_fires$Northing
MSI_2018$PA <- eight_prairie_fires$PA
MSI_2018$Plot <- eight_prairie_fires$plotID
MSI_2018$NMDI <- raster::extract(MDI.2018,c(eight_prairie_fires$Northing, eight_prairie_fires$Easting))
#normalized difference infrared index
MSI_2018$NDII <- raster::extract(DII.2018,c(eight_prairie_fires$Northing, eight_prairie_fires$Easting))
#normalized difference water index
MSI_2018$NDWI <- raster::extract(DWI.2018,c(eight_prairie_fires$Northing, eight_prairie_fires$Easting))
#water band index
MSI_2018$WBI <- raster::extract(WBI.2018,c(eight_prairie_fires$Northing, eight_prairie_fires$Easting))
names(MSI_2018)

nine_prairie_fires <- transformed_prairie_fire[transformed_prairie_fire$year==2019,]

MSI_2019 <- data.frame(raster::extract(MSI.2019, c(nine_prairie_fires$Northing,nine_prairie_fires$Easting)))
names(MSI_2019) <- "MSI"
MSI_2019$year <- 2019
MSI_2018$Easting <- eight_prairie_fires$Easting
MSI_2018$Northing <- eight_prairie_fires$Northing
MSI_2019$PA <- nine_prairie_fires$PA
MSI_2019$Plot <- nine_prairie_fires$plotID
MSI_2019$NMDI <- raster::extract(MDI.2019,c(nine_prairie_fires$Northing, nine_prairie_fires$Easting))
#normalized difference infrared index
MSI_2019$NDII <- raster::extract(DII.2019,c(nine_prairie_fires$Northing, nine_prairie_fires$Easting))
#normalized difference water index
MSI_2019$NDWI <- raster::extract(DWI.2019,c(nine_prairie_fires$Northing, nine_prairie_fires$Easting))
#water band index
MSI_2019$WBI <- raster::extract(WBI.2019,c(nine_prairie_fires$Northing, nine_prairie_fires$Easting))


ncol(MSI_2017)
ncol(MSI_2018)

#talked a bit about the plot/subplot up above, but i'll mention here too. Seems like
#for the percent cover data which is what we have here, the sampling took place in
#8 1 square meter subplots for each plot. There are only 6 levels to subplotID tho
#so honestly who knows what the structure of this really is??
  
  



plot(full_slope_raster)
plot(full_aspect_raster)


all_years <- rbind(MSI_2017,MSI_2018,MSI_2019)



names(all_years)










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






spat_neon_veg <- SpatialPointsDataFrame(veg_survey_NEON[,6:5],
                                        veg_survey_NEON)


crs(spat_neon_veg) <- r


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


if(length(grep("fuck","fuck"))>0){
  print("yiss")
}


