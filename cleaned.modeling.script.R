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

confusion_calc <- function(model_list){
  
  
  false_negative <- length(which(model_list[[2]]$true_val==1&model_list[[2]]$pred<0.75))
  false_positive <- length(which(model_list[[2]]$true_val==0&model_list[[2]]$pred>0.75))
  total_wrong <- false_negative+false_positive
  fn_freq <- false_negative/total_wrong
  fp_freq <- false_positive/total_wrong
  
  all_pos_pred <- length(which(model_list[[2]]$pred>0.75))
  
  false_pos_prop <- false_positive/all_pos_pred
  
  confusion_vec <- c(total_wrong,false_negative, false_positive, fn_freq,fp_freq,false_pos_prop, all_pos_pred)
  return(confusion_vec)
  
}
confusion_frame <- data.frame(total_wrong=rep(NA,ls_l),false_negative=rep(NA,ls_l), false_positive=rep(NA,ls_l), fn_freq=rep(NA,ls_l),
                              fp_freq=rep(NA,ls_l),proportion_pos_false=rep(NA,ls_l), pos_pred=rep(NA,ls_l))


for(mod in 1:ls_l){
  
  confusion_frame[mod,] <- confusion_calc(mod_list[[mod]])
  
}

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

geum_df$year_low <- geum_df$year-2016

MSI.NDNI.year.plot <- stan_glmer(PA~year_low+(1|plot)+MSI+NDNI,data=geum_df, family="binomial")
summary(MSI.NDNI.year.plot)

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

