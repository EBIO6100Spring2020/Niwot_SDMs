#Neon Models


setwd("~/Desktop/Project/Niwot_SDM/Niwot_SDMs/")
libs <- c("rstan", "rstanarm", "rethinking", "rsample","purrr","stringr","dplyr","ggplot2",
          "raster", "maxnet", "dismo")

loaded_reqs <- lapply(libs,require, character.only=T)

geum_df <- read.csv("NEON.Data/Elevation.DTM/Geum.PA.all.ind.10.m.avg.elev.csv",header = T)

#warning here, the log odds coefficients get a bit weird when we have a constrained scale like our indices are. Most standard
#transforms are a bit wonky, as they can't necessarily take positive and negative values. The best I've come up with is just
#multiplying by a constant (10) which really just reduces the order of magnitude for our coefficient. Might be a dumb thing to do 
#though. But also, using these indices may be silly since they're not real metrics. Oh well.

#here I'm gonna work just with rstanarm, but then i'll use mcelreath's rethinking approach as well and see how the models differ.



geum_df <- na.omit(geum_df)

geum_df$year_low <- geum_df$year-2016
names(geum_df)


#standardize predictors
geum_df[,c(10:24)] <- scale(geum_df[,c(10:24)])

geum_df$el_exp <- geum_df$elevation*geum_df$elevation

names(geum_df[,c(10:26)])
#Run PCA

geum_df <- na.omit(geum_df)
names(geum_df)
length(names(geum_df))
pca <- prcomp(geum_df[,c(10:23,26)])
summary(pca)

tran <- pca$x

geum_df$PC1 <- tran[,1]
geum_df$PC2 <- tran[,2]
geum_df$PC3 <- tran[,3]

#quick correlation for presence/absence of geum, along with all of our various moisture and nitrogen indices
cor(geum_df[,c(4,10:21)])




#start with some cross-val



#no pooling model

np.mod <- stan_glm(PA~1+as.numeric(year), data=geum_df, family="binomial")

summary(np.mod)


#add in our plots

plot.yr.mod <- stan_glmer(PA~as.numeric(year)+(1|plot), data=geum_df, family="binomial")
summary(plot.yr.mod)

#lots of per plot variance, so lets add two predictors, MSI and NDNI. MSI is a moisture stress index ased around new leaf growth.
#NDNI is a nitrogen index that should capture relative nitrogen content of vegetative cover. So these are proxies for nitrogen
#and moisture stress really, as both are based on reflectance of vegetative cover.




######### This is the worthwhile model ##########

MSI.NDNI.year.plot <- stan_glmer(PA~year_low+(1|plot)+MSI,data=geum_df, family="binomial")
summary(MSI.NDNI.year.plot)

int_mod_year<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept+ b_year*year_low,
      cept~dnorm(0,3),
    b_year~dnorm(0,1.5)
  ), data=geum_df, chains=4
)

mod_plt_year <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains = 4, iter=4000
)


mod_plt_elev_year <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_elev*el_exp,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_elev~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains = 4, iter=4000
)

pc1_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_pc1*PC1,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains = 4, iter=4000
)




pc1_pc2_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains = 4, iter=4000
)

pc1_pc2_pc3 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_pc1*PC1+b_pc2*PC2+b_pc3*PC3,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_pc3~dnorm(0,1.5),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains = 4, iter=4000
)


# mod_plt_year
# summary(mod_plt_year)

comparison_all <- compare(int_mod_year, mod_plt_year,mod_plt_elev_year, pc1_stan,
                          pc1_pc2_stan, pc1_pc2_pc3)
comparison_all


all_year_list <- list(int_mod_year, mod_plt_year,mod_plt_elev_year, pc1_stan,
                      pc1_pc2_stan, pc1_pc2_pc3)

saveRDS(all_year_list, "models_all_year")

plot(precis(mod_plt_elev_year))

#lol amazing, so now no predictors at all. Just plot and year. Love to see it.
#alright, so i fiddled with the prior for the intercept variance, made it a cauchy (0,1) for
#all plot models. That bumped the wbi model up to tie in first, but still not all that 
#convincing of a model to say that wbi has an impact. Like literally same WAIC, very slight
#improvement to the SE, then same everything else and tied in the Aikaike weight @ 0.22. 
plot(comparison)
plot(coeftab(int_mod,mod_year, mod_plt_year, msi_mod,ndni_mod,full_mod,wbi_mod),
     cex.axis=0.01)

cor(geum_df[,c(4,10:21)])

coeftab_plot(int_mod)

plot(precis(ndni_mod))
plot(precis(wbi_mod))


plot(precis(pc1_pc2_stan))

WAIC(mod_plt_year)
WAIC(mod)

loo_plt_yr <- rstanarm::loo(plot.yr.mod)
loo_w_indices <- rstanarm::loo(MSI.NDNI.year.plot)
l_list <- list(loo_plt_yr,loo_w_indices)
m_list <-stanreg_list(plot.yr.mod,MSI.NDNI.year.plot)
loo_model_weights(l_list)

#So how do we want to generate our simulated data? I suppose I could fit a model with year explaining MSI and NDNI, then
#predict forward for 2020, 2021, 2022? Then plug those in. Would need a plotp though. So a function of year and plot, then I can
#give it a year and a plot, one for each subplot i guess? then go for it overall.



summary(just_ndni)






#sep by year models!

geum_17 <- geum_df[geum_df$year==2017,]
geum_18 <- geum_df[geum_df$year==2018,]
geum_19 <- geum_df[geum_df$year==2019,]

pc17 <- prcomp(geum_17[,c(10:23,26)])
pc18 <- prcomp(geum_18[,c(10:23,26)])
pc19 <- prcomp(geum_19[,c(10:23,26)])


summary(pc17)

names(geum_17[,c(10:26)])

geum_17$PC1 <- pc17$x[,1]
geum_17$PC2 <- pc17$x[,2]
geum_17$PC3 <- pc17$x[,3]

geum_18$PC1 <- pc18$x[,1]
geum_18$PC2 <- pc18$x[,2]
geum_18$PC3 <- pc18$x[,3]

geum_19$PC1 <- pc19$x[,1]
geum_19$PC2 <- pc19$x[,2]
geum_19$PC3 <- pc19$x[,3]


#### 2017 models ####

int_mod_17<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
    cept~dnorm(0,3)
  ), data=geum_17, chains=4
)


mod_plt_17<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot],
    cept~dnorm(0,3),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)


mod_plt_elev_year_17 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_el_exp*el_exp,
    cept~dnorm(0,3),
    b_el_exp~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)


pc1_incl_el_exp_17<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)

plot(precis(pc1_incl_el_exp_17))

pc1_pc2_incl_el_exp_17 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=5000,
  control=list(adapt_delta=0.98)
)

plot(precis(pc1_pc2_incl_el_exp_17))

summary(pc1_pc2_incl_el_exp_17)

pc1_pc2_pc3_el_exp_17 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2+b_pc3+PC3,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    b_pc3~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000,
)
summary(pc1_pc2_pc3_el_exp_17)
plot(precis(pc1_pc2_pc3_el_exp_17))

pc_17_compar <- compare(int_mod_17, mod_plt_17, mod_plt_elev_year_17,
                        pc1_incl_el_exp_17, pc1_pc2_incl_el_exp_17, pc1_pc2_pc3_el_exp_17)
pc_17_compar

mod_17_list <- list(int_mod_17, mod_plt_17, mod_plt_elev_year_17,
                    pc1_incl_el_exp_17, pc1_pc2_incl_el_exp_17, pc1_pc2_pc3_el_exp_17)

saveRDS(mod_17_list,"2017_models_pca")

pc_17_compar




##### 2018 models #####





int_mod_18<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
    cept~dnorm(0,3)
  ), data=geum_18, chains=4
)


mod_plt_18<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot],
    cept~dnorm(0,3),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)

mod_plt_elev_year_18 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_el_exp*el_exp,
    cept~dnorm(0,3),
    b_el_exp~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)


pc1_stan_18 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)

pc1_pc2_stan_18 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)



pc1_pc2_pc3_el_exp_18 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2+b_pc3+PC3,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    b_pc3~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000,
)

pc_18_compar <- compare(int_mod_18, mod_plt_18, mod_plt_elev_year_18,
                        pc1_stan_18, pc1_pc2_stan_18, pc1_pc2_pc3_el_exp_18)
pc_18_compar


list_2018 <- list(int_mod_18, mod_plt_18, mod_plt_elev_year_18,
                  pc1_stan_18, pc1_pc2_stan_18, pc1_pc2_pc3_el_exp_18)

saveRDS(list_2018, "2018.models.RDS")

#### 2019 ####


int_mod_19<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
    cept~dnorm(0,3)
  ), data=geum_19, chains=4
)


mod_plt_19<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot],
    cept~dnorm(0,3),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)

mod_plt_elev_year_19 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_el_exp*el_exp,
    cept~dnorm(0,3),
    b_el_exp~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)



pc1_stan_19 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)

pc1_pc2_stan_19 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)



pc1_pc2_pc3_el_exp_19 <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2+b_pc3+PC3,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    b_pc3~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dgamma(1,2),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000,
)

pc_19_compar <- compare(int_mod_19, mod_plt_19, mod_plt_elev_year_19,
                       pc1_stan_19,pc1_pc2_stan_19, pc1_pc2_pc3_el_exp_19)
pc_19_compar


list_2019 <- list(int_mod_19, mod_plt_19, mod_plt_elev_year_19,
                  pc1_stan_19,pc1_pc2_stan_19, pc1_pc2_pc3_el_exp_19)

saveRDS(list_2019, "2019.models.RDS")

plot(precis(mod_plt_elev_year_19))

pc_18_compar
plot(precis(mod_plt_elev_year_18))

pc_17_compar
plot(precis(mod_plt_17))
plot(precis(mod_plt_elev_year_17))

##### This part won't always work #######

#from a paper on maxent: "In a Bayesian framework, 
#Eq. (3) corresponds to a negative log posterior given a Laplace prior."
#No idea how that relates to us tbh.
library(maxnet)
library(rJava)
library(dismo)
system.file("java",package="dismo")
stack17 <- raster::stack(twenty_seven[c(1:12,13,14,15)])



stack18 <- stack(twenty_eight[c(1:12,13,14,15)])
stack19 <- stack(twenty_nine[c(1:12,13,14,15)])

just_pres <- geum_df[geum_df$PA==1,]
pres_17 <- just_pres[just_pres$year==2017,]
pres_EN <- pres_17[,c(2,3)]

ent_geum <- maxent(stack17,p=pres_EN)
response(ent_geum, expand=0)
simple_max_map <- predict(stack17, ent_geum)
back.xy <- randomPoints(stack17,p=pres_EN,n=5000)
#include 2000 background points
ent_geum_back <- maxent(stack17,p=pres_EN,a=back.xy)
back_map <- predict(stack17, ent_geum_back)



pres_18 <- just_pres[just_pres$year==2018,]
EN_18 <- pres_18[,c(2,3)]
back_18 <- randomPoints(stack18,p=EN_18,n=10000)

pres_19 <- just_pres[just_pres$year==2019,]
EN_19 <- pres_19[,c(2,3)]
back_19 <- randomPoints(stack19,p=EN_19,n=10000)

ent_geum_back_18 <- maxent(stack18,p=EN_18,a=back_18)
back_map_18 <- predict(stack18, ent_geum_back_18)

ent_geum_back_19 <- maxent(stack19,p=EN_19,a=back_19)
back_map_19 <- predict(stack19, ent_geum_back_19)


map_list <- list(back_map, back_map_18, back_map_19)


maxent_list <- list(ent_geum_back, ent_geum_back_18, ent_geum_back_19)


resp_18
resp_19


mod_plt_ext <- mod_plt@stanfit

seven_and_seven <- lapply(twenty_seven,test_foc)

scaled_17 <- scale(stack17)

scaled_18 <- scale(stack18)
scaled_19 <- scale(stack19)


writeRaster(scaled_17, "~/Desktop/Project/Niwot_SDM/Niwot_SDMs/scaled.stack_17", overwrite=T)

writeRaster(scaled_18, "~/Desktop/Project/Niwot_SDM/Niwot_SDMs/scaled.stack_18")
writeRaster(scaled_19, "~/Desktop/Project/Niwot_SDM/Niwot_SDMs/scaled.stack_19")

scaled_17 <- raster("scaled.stack_17.gri")

test <- raster("scaled.stack_18.gri")

scaled_17$el_exp <- scaled_17$layer.3*scaled_17$layer.3

scaled_17$el_exp <- focal(scaled_17$el_exp,NAonly=T,w=w)


stack17[is.na(stack17)]




coords <- data.frame(coordinates(scaled_17$layer.3))


full_el_ex <- data.frame(el=scaled_17$layer.3[], x=coords$x, y=coords$y)

plot(scaled_17$layer.3)

griddy <- expand.grid(c(1:4000),c(1:5000))
names(griddy) <- c("Easting","Northing")
head(griddy)

full_el_ex$el_exp <- full_el_ex$el*full_el_ex$el
predictions <- rethinking::sim(mod_plt_elev_year_17, full_el_ex,na.rm=T,n=10)
full_el_ex$predictions_avg <- apply(predictions,2,mean)


library(ggplot2)




coordinates(full_el_ex) <- ~x+y
gridded(full_el_ex) <- TRUE
raster_exp_preds <- raster::stack(full_el_ex)
plot(scaled_17$layer.3)

plot(raster_exp_preds$predictions_avg)

names(predictions_avg) <- "el_exp"

library(reshape2)
pred_mat <- acast(predictions_avg,c(1:4000)~c(1:5000),value.var=predictions_avg)

head(predictions)

w <- matrix(c(0.25,.25,0.25,.25,0,.25,0.25,.25,0.25), 3, 3)

scale(stack17)


test_foc <- function(x){
  raster::focal(x, NAonly=T, w=w)
}


test_data <- data.frame(ARVI=stack18$ARVI.2018[])





sim2 <- function(...){
  rethinking::sim(...,n=5, na.rm=T)
}


raster::writeRaster(stack17,"2017.stack.raster", overwrite=T)
raster::writeRaster(stack18,"2018.stack.raster", overwrite=T)
raster::writeRaster(stack19,"2019.stack.raster", overwrite=T)


raster::stackSave(stack18, "2018.stack")

stack217 <- raster("2017.stack.gri")




plot(stack217)

##### future predictions ####


just_ndni <- lm(NDNI~year_low+plot,data = geum_df)
just_msi <- lm(MSI~year_low+plot,data=geum_df)


plots <- levels(geum_df$plot)

plots <- plots[-1]

years <- c(4,5,6)

future_frame <- expand.grid(years,plot)
names(future_frame) <- c("plot","year_low")
future_frame$plot <- as.factor(future_frame$plot)
future_frame$year_low <- as.numeric(future_frame$year_low)
future_frame$year_low <- future_frame$year_low+3

future_frame$NDNI <- stats::predict(just_ndni,newdata=future_frame[,1:2])
future_frame$MSI <-  stats::predict(just_msi,newdata=future_frame[,1:2])

avg_preds <- function(dat,model){
  preds <- posterior_predict(model,dat,draws=100)
  avg.preds <- apply(preds,MARGIN=2,FUN=mean)
  return(avg.preds)
}

#some very basic predictions here. not wild about this, as we don't have subplot specific 
future_frame$PA <- avg_preds(future_frame,MSI.NDNI.year.plot)





#### cross val work ####

splits <- initial_split(geum_df, prop=0.8)

train <- training(splits)
test <- testing(splits)

#now some monte carlo cross val within our training set

m.c.c.v <- mc_cv(train, prop=0.8, times = 25)

m.c.c.v$splits


#this is a little complex, but basically our mccv object above just contains a bunch of splits for our data. We have to apply
#the assessment and analysis functions to creat our cross val data sets, but we have to apply to each split in mccv separately.
#so this map function takes the splits, separates each data set, fits the model to our analysis set, then predicts based on 
#our assessment set, then bundles them together and returns a list with each model specification and the predictions.
#the posterior predict function takes the posterior estimate for each coefficient from the model, makes a random draw from
#that for the coefficient value, then predicts based on the explanatory data we provide. So to get an idea of how our model
#really predicts, we need to make multiple draws, then average the outcomes. here i have it coded to make 50 draws, then average
#the 0/1 predictions to get a proportion of positives. We can then make extract various confusion metrics from the set of
#true data and predicted values. We can also play with the threshold at which we would say we "predict" a presence. SHould it
#be above 50% of draws predicted presence, above 60%, etc. In fiddling with it earlier I found 0.55 gave the lowest average error,
#but that may be contingent on mc_cv properly splitting along hierarchical structures in the data, i.e. plots.
mod_list <- list()
ls_l <- length(mod_list)
mod_list <- purrr::map(m.c.c.v$splits,
                       function(x){
                         
                         an <- analysis(x)
                         as <- assessment(x)
                         
                         dat <- as.data.frame(an)
                         mod <- stan_glmer(PA~year_low+(1|plot)+MSI+NDNI, data=dat, family="binomial")
                         preds <- posterior_predict(mod,as, draws=100)
                         preds_avg <- apply(preds,FUN=mean,MARGIN=2)
                         cv_true <- data.frame(true_val=as$PA,pred=preds_avg)
                         return(list(mod,cv_true))
                       }
)




mod_list[[1]]


confusion_calc <- function(model_list){
  
  
  false_negative <- length(which(model_list[[2]]$true_val==1&model_list[[2]]$pred<0.6))
  false_positive <- length(which(model_list[[2]]$true_val==0&model_list[[2]]$pred>0.6))
  total_wrong <- false_negative+false_positive
  fn_freq <- false_negative/total_wrong
  fp_freq <- false_positive/total_wrong
  
  all_pos_pred <- length(which(model_list[[2]]$pred>0.6))
  
  false_pos_prop <- false_positive/all_pos_pred
  
  confusion_vec <- c(total_wrong,false_negative, false_positive, fn_freq,fp_freq,false_pos_prop, all_pos_pred)
  return(confusion_vec)
  
}
confusion_frame <- data.frame(total_wrong=rep(NA,ls_l),false_negative=rep(NA,ls_l), false_positive=rep(NA,ls_l), fn_freq=rep(NA,ls_l),
                              fp_freq=rep(NA,ls_l),proportion_pos_false=rep(NA,ls_l), pos_pred=rep(NA,ls_l))


for(mod in 1:ls_l){
  
  confusion_frame[mod,] <- confusion_calc(mod_list[[mod]])
  
}
fr <- mod_list[[1]][[1]]$coefficients
some <- names(fr)


confusion_frame


#now we're mostly predicting negatives where there should be positives it looks like?
barplot(table(confusion_frame$total_wrong))
barplot(table(confusion_frame$false_negative))
barplot(table(confusion_frame$false_positive))


rm(nit_19)





