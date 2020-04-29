#Neon Models



libs <- c("rstan", "rstanarm", "rethinking", "rsample","purrr","stringr","dplyr","ggplot2")

loaded_reqs <- lapply(libs,require, character.only=T)

geum_df <- read.csv("Geum.PA.all.ind.10.m.avg.csv",header = T)

#warning here, the log odds coefficients get a bit weird when we have a constrained scale like our indices are. Most standard
#transforms are a bit wonky, as they can't necessarily take positive and negative values. The best I've come up with is just
#multiplying by a constant (10) which really just reduces the order of magnitude for our coefficient. Might be a dumb thing to do 
#though. But also, using these indices may be silly since they're not real metrics. Oh well.

#here I'm gonna work just with rstanarm, but then i'll use mcelreath's rethinking approach as well and see how the models differ.

geum_df <- na.omit(geum_df)

geum_df$year_low <- geum_df$year-2016
names(geum_df)



#quick correlation for presence/absence of geum, along with all of our various moisture and nitrogen indices
cor(geum_df[,c(4,10:21)])


#start with some cross-val

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

int_mod<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
      cept~dnorm(0,3)
  ), data=geum_df, chains=4
)

mod_year <- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept+b_year*year_low,
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
    a_var~dcauchy(0,1),
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
    a_var~dcauchy(0,1),
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
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains = 4, iter=4000
)
msi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_MSI*MSI,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_MSI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains=4,iter=3000
)

ndni_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_NDNI*NDNI,
    b_year~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_df, chains=4, iter=3000
  )
#ndni_mod

wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_year~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_df, chains=4, iter=3000
)



no_plot_wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+b_year*year_low+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_year~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    cept~dnorm(0,3)
  ), data=geum_df, chains=4, iter=3000
)

plot(precis(no_plot_wbi_ndni))


wbi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_wbi*WBI,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_wbi~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains=4, iter=3000
)
summary(wbi_mod)


full_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_year*year_low+b_MSI*MSI+b_NDNI*NDNI,
    cept~dnorm(0,3),
    b_year~dnorm(0,1.5),
    b_MSI~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_df, chains=4, iter=3000
)
#full_mod


# mod_plt_year
# summary(mod_plt_year)

comparison <- compare(int_mod,mod_year, mod_plt_year, msi_mod,ndni_mod,full_mod,wbi_mod,
                      pc1_stan,pc1_pc2_stan, no_plot_wbi_ndni)
comparison

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


just_ndni <- lm(NDNI~year_low+plot,data = geum_df)
just_msi <- lm(MSI~year_low+plot,data=geum_df)

summary(just_ndni)



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




plot(geum_df$NDNI)
plot(geum_df$MSI)

dups <- c(0.927906848490238,  0.96680011972785, 0.992254570126534,  1.12168728560209,  1.17258661985397)

tab <- table(geum_df$MSI)
which(tab>30)

tab[412]

dup_ind <- which(geum_df$MSI==dups)

nrow(geum_df[dup_ind,])



f1_plot <- geum_df[geum_df$top_sub==41,]

table(f1_plot$ARVI)



pca <- prcomp(geum_df[,c(10:21)])
summary(pca)

rotate_mat <- pca$rotation

pcr_test <- pca$x


geum_df$PC1 <- pcr_test[,1]
geum_df$PC2 <- pcr_test[,2]



#sep by year models!

geum_17 <- geum_df[geum_df$year==2017,]
geum_18 <- geum_df[geum_df$year==2018,]
geum_19 <- geum_df[geum_df$year==2019,]

pc17 <- prcomp(geum_17[,c(10:23)])
pc18 <- prcomp(geum_18[,c(10:23)])
pc19 <- prcomp(geum_19[,c(10:23)])

geum_17$PC1 <- pc17$x[,1]
geum_17$PC2 <- pc17$x[,2]

geum_18$PC1 <- pc18$x[,1]
geum_18$PC2 <- pc18$x[,2]

geum_19$PC1 <- pc19$x[,1]
geum_19$PC2 <- pc19$x[,2]


#### 2017 models ####

int_mod<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
    cept~dnorm(0,3)
  ), data=geum_17, chains=4
)


mod_plt<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot],
    cept~dnorm(0,3),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)

mod_plt_slp_asp<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_slope*slope+b_aspect*aspect,
    cept~dnorm(0,3),
    b_slope~dnorm(0,1.5),
    b_aspect~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)



pc1_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)

pc1_pc2_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains = 4, iter=4000
)
msi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_MSI*MSI,
    cept~dnorm(0,3),
    b_MSI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains=4,iter=3000
)

ndni_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_NDNI*NDNI,
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_17, chains=4, iter=3000
)
#ndni_mod

wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_17, chains=4, iter=3000
)



no_plot_wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    cept~dnorm(0,3)
  ), data=geum_17, chains=4, iter=3000
)

plot(precis(no_plot_wbi_ndni))


wbi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_wbi*WBI,
    cept~dnorm(0,3),
    b_wbi~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains=4, iter=3000
)
summary(wbi_mod)


full_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_MSI*MSI+b_NDNI*NDNI,
    cept~dnorm(0,3),
    b_MSI~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_17, chains=4, iter=3000
)
#full_mod


# mod_plt_year
# summary(mod_plt_year)

comparison17 <- compare(int_mod, mod_plt, mod_plt_slp_asp, msi_mod,ndni_mod,full_mod,wbi_mod,
                      pc1_stan,pc1_pc2_stan, no_plot_wbi_ndni)
comparison17


##### 2018 models #####





int_mod<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
    cept~dnorm(0,3)
  ), data=geum_18, chains=4
)


mod_plt<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot],
    cept~dnorm(0,3),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)

mod_plt_slp_asp<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_slope*slope+b_aspect*aspect,
    cept~dnorm(0,3),
    b_slope~dnorm(0,1.5),
    b_aspect~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)



pc1_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)

pc1_pc2_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains = 4, iter=4000
)
msi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_MSI*MSI,
    cept~dnorm(0,3),
    b_MSI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains=4,iter=3000
)

ndni_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_NDNI*NDNI,
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_18, chains=4, iter=3000
)
#ndni_mod

wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_18, chains=4, iter=3000
)



no_plot_wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    cept~dnorm(0,3)
  ), data=geum_18, chains=4, iter=3000
)

plot(precis(no_plot_wbi_ndni))


wbi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_wbi*WBI,
    cept~dnorm(0,3),
    b_wbi~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains=4, iter=3000
)
summary(wbi_mod)


full_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_MSI*MSI+b_NDNI*NDNI,
    cept~dnorm(0,3),
    b_MSI~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_18, chains=4, iter=3000
)



comparison18 <- compare(int_mod, mod_plt, mod_plt_slp_asp, msi_mod,ndni_mod,full_mod,wbi_mod,
                        pc1_stan,pc1_pc2_stan, no_plot_wbi_ndni)
comparison18


#### 2019 ####


int_mod<- map2stan(
  alist(
    PA ~ dbinom(1,p),
    logit(p) <- cept,
    cept~dnorm(0,3)
  ), data=geum_19, chains=4
)


mod_plt<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot],
    cept~dnorm(0,3),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)

mod_plt_slp_asp<- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_slope*slope+b_aspect*aspect,
    cept~dnorm(0,3),
    b_slope~dnorm(0,1.5),
    b_aspect~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)



pc1_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)

pc1_pc2_stan <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_pc1*PC1+b_pc2*PC2,
    cept~dnorm(0,3),
    b_pc1~dnorm(0,1.5),
    b_pc2~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains = 4, iter=4000
)
msi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_MSI*MSI,
    cept~dnorm(0,3),
    b_MSI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains=4,iter=3000
)

ndni_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_NDNI*NDNI,
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_19, chains=4, iter=3000
)
#ndni_mod

wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var),
    cept~dnorm(0,3)
  ), data=geum_19, chains=4, iter=3000
)



no_plot_wbi_ndni <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+b_NDNI*NDNI+b_wbi*WBI,
    b_wbi~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    cept~dnorm(0,3)
  ), data=geum_19, chains=4, iter=3000
)

plot(precis(no_plot_wbi_ndni))


wbi_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_wbi*WBI,
    cept~dnorm(0,3),
    b_wbi~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains=4, iter=3000
)
summary(wbi_mod)


full_mod <- map2stan(
  alist(
    PA~dbinom(1,p),
    logit(p) <- cept+a[plot]+b_MSI*MSI+b_NDNI*NDNI,
    cept~dnorm(0,3),
    b_MSI~dnorm(0,1.5),
    b_NDNI~dnorm(0,1.5),
    a_mean~dnorm(0,1.5),
    a_var~dcauchy(0,1),
    a[plot]~dnorm(a_mean,a_var)
  ), data=geum_19, chains=4, iter=3000
)



comparison19 <- compare(int_mod, mod_plt, mod_plt_slp_asp, msi_mod,ndni_mod,full_mod,wbi_mod,
                        pc1_stan,pc1_pc2_stan, no_plot_wbi_ndni)

comparison17
comparison18
comparison19




##### This part won't always work #######

#from a paper on maxent: "In a Bayesian framework, 
#Eq. (3) corresponds to a negative log posterior given a Laplace prior."
#No idea how that relates to us tbh.
library(maxnet)
library(rJava)
library(dismo)
system.file("java",package="dismo")
stack17 <- stack(twenty_seven[1:12])
stack18 <- stack(twenty_eight[1:12])
stack19 <- stack(twenty_nine[1:12])

just_pres <- geum_df[geum_df$PA==1,]
pres_17 <- just_pres[just_pres$year==2017,]
pres_EN <- pres_17[,c(2,3)]

ent_geum <- maxent(stack17,p=pres_EN)
response(ent_geum, expand=0)
simple_max_map <- predict(stack17, ent_geum)
back.xy <- randomPoints(stack17,p=pres_EN,n=2000)
#include 2000 background points
ent_geum_back <- maxent(stack17,p=pres_EN,a=back.xy)
back_map <- predict(stack17, ent_geum_back)
response(ent_geum_back, expand=0)
plot(back_map)
plot(simple_max_map)


mod_plt_ext <- mod_plt@stanfit

predictions <- raster::predict(stack18,mod_plt, fun=sim2)


sim2 <- function(...){
  rethinking::sim(...,n=5, na.rm=T)
}



