setwd("~/Desktop/Project/")
library(dplyr)
library(rstan)
library(rstanarm)
library(lme4)


######## Read in data and process #######

load("niwot composition.Rdata")

#There are three datasets for this project. The first is a presence absence dataset, which we won't be using.
#This is mostly because in 2018 they apparently corrected a bunch of species IDs for the second dataset, but not the 
#first, so I'm not sure what's correct or outdated now.

old_pres <- download_list$`Plant species composition data for Saddle Nodal Plots, 1971-ongoing..1`

#This is the cover data set with new spp names from 2018. We'll fiddle with this a bit, then convert to pres/abs data
#for the analysis. Long story on that.
new_cover <- download_list[[2]]

#This is non-vascular plant cover along with other miscellaneous cover data. Who knows what to do with that?
non_vasc_cover <- download_list[[3]]


head(new_cover)
head(non_vasc_cover)

#here we just sort our data by year, then by plot number. So these will now be ordered so that all of 1971 appears
#together, and then within that, all of plot 1 are together, then plot 2, then plot 3, etc.
year_plot_sort_2018<- new_cover[with(new_cover,order(SAMPLEYEAR,PLOTID)),]

table(year_plot_sort_2018$GENSPP_Updated2018)

#since we are interested in pulling a specific genus, we create a new column that has just the first word
#from our initial 
year_plot_sort_2018$just_genus <- gsub("([A-Za-z]+).*", "\\1", year_plot_sort_2018$GENSPP_Updated2018)


#good, now that we're here, let's talk about cover. For each 1 x 10m plot, there were ten subplot quadrats. Cover was
#measured in EACH quadrat using a 1 m x 10 cm strip. These were assumed to be representative of that quadrat. The cover
#values for each plot is then the average across quadrats within the plot. No error provided for those estimates, so
#no idea how much cover varied within a given plot, just an average over it. Cover is % cover. No word given on 0's here.
#there might be temporal structure? Like...if we have none in 1971, but then we have it in 1991, but then not in 2001,
#we might have no record for 71, a cover for 91, and a cover of 0 for 2001? tough to say tbh.




########Deschampsia Work #######
only_descartes <- year_plot_sort_2018[year_plot_sort_2018$just_genus=="Deschampsia",]


#Switch, to switch, to switch it up to Deschampsia now. Alright. So. Hmm. Should I convert to P/A? Probs. Could always
#do it and just move on wit my life.

years <- unique(year_plot_sort_2018$SAMPLEYEAR)
plots <- unique(year_plot_sort_2018$PLOTID)
pres_abs_table <- expand.grid(years,plots)
names(pres_abs_table) <- c("Year","Plot")
pres_abs_table$pres <- numeric(120)





for(plotyear in 1:120){
  #so what I want is to check if, in my only deschampsia object, there are any rows that contain the year and 
  #plot ID together. If so, add a 1. If not drop it. Okay.
  does_it_exist <- which(only_descartes$PLOTID==pres_abs_table$Plot[plotyear]&only_descartes$SAMPLEYEAR==pres_abs_table$Year[plotyear])
  lth <- length(does_it_exist)
  if(lth>0){
    pres_abs_table$pres[plotyear] <- 1
  }else{
    pres_abs_table$pres[plotyear] <- 0
  }
}
pres_abs_table


no_pool_descartes <- mean(pres_abs_table$pres)

full_pool_descartes <- pres_abs_table %>%
  group_by(Plot)%>%
  summarize(plot_mean=mean(pres),plot_sd=sd(pres),sample_size=n())%>%
  mutate(plot_se=plot_sd/sqrt(sample_size))
pres_abs_table$Plot <- as.factor(pres_abs_table$Plot)

#now fit a lil model with year if ya don't mind

year_model <- glm(pres~-1+Year, data=pres_abs_table, family="binomial")
summary(year_model)
plot(year_model)

rando_plot_model <- glmer(pres~1+(1|Plot),data=pres_abs_table,family = "binomial")

###### Old Carex Work ########


only_carex <- year_plot_sort_2018[year_plot_sort_2018$just_genus=="Carex",]


complete_pool <- mean(only_carex$COVER)
complete_pool <- as.data.frame(complete_pool)

plot_mean <- only_carex %>%
  group_by(PLOTID)%>%
  summarize(plot_mean=mean(COVER),plot_sd=sd(COVER),sample_size=n())%>%
  mutate(plot_se=plot_sd/sqrt(sample_size))
plot_mean

#just a basic plot showing how carex cover varies across plots, with colors for species but not sure I want to do spp level
#stuff yet.
carex_plot <- ggplot(data=only_carex,aes(fill=GENSPP_Updated2018))+geom_bar(aes(x=PLOTID,y=COVER),stat="identity")+
  facet_wrap(~SAMPLEYEAR)
carex_plot

#now we plot with complete pooling vs no pooling. Blue line is complete pooling, so not modeling any plot level means.
# the black dots with error bars are our plot level estimates. Along the x axis we have sample size, which should give
#us an idea of how confident we can really be in any of our data. Lots of 6 and below plots here tbh.

#Yeah so to be clear, I genuinely do not understand how to structure this data for analysis. So if I am saying that
#I want to predict Carex distribution, first I would say what is the average cover. This would be no plot structure
#whatsoever. Then I might say, can I predict cover based on plot? Do we see differential distribution of Carex across plots?
#and the answer is probably yes. But then what is my sample size for that? Because, technically, if I am aggregating
#across the genus, i should sum each individual species to get a genus level cover. how much, in a given plot, does
#carex as a genus cover? Thus I would end up with, really, only 1 data point per plot, which obviously I can't fit
#a plot model to. So. The alternative is to not sum them, and keep them "independent." 

plot_plot <- ggplot(data=plot_mean) +
  geom_hline(mapping=aes(yintercept=complete_pool),data=complete_pool,col="blue") +
  geom_point(mapping=aes(sample_size,y=plot_mean)) +
  geom_linerange(mapping=aes(x=sample_size,ymin=plot_mean-plot_se,ymax=plot_mean+plot_se)) 

plot_plot

only_carex$intercept <- rep(1,length(only_carex$SPP))
only_carex$PLOTID <- as.factor(only_carex$PLOTID)
#now we'll model more explicitly where we have a level for each. So once again, a "no pooling" model where each plot
#get's its own "model," which is to say, it gets its own intercept and variance estimate.

no_pool_model <- lm(COVER ~ -1 + GENSPP_Updated2018, data=only_carex)
summary(no_pool_model)

#now we plot

#res v fitted - got some serious funnel work, and a number of outliers that give us some poor fit. Proly some leverage
#from them as a result. I actually do think a pooled estimate might give us some nice cover on that though...

#qq plot is sigmoidal which is not a thing you wanna see. So not good at all at the high and low ends of our quantiles.

#not miserable on res vs leverage. Some high leverage points, but I think they're the same outliers, so presumably if we
#correct our fit with some plot level pooling we should be good.

plot(no_pool_model)

#here we plot our residuals against an inferred normal distro. Some very unequal tails, some very peaky biz,
#overall not ideal, but hey, that's the way it is. Maybe a partial pool model will cover that so that outliers
#get pulled down a bit when they're low data.

r <- residuals(no_pool_model)
x <- seq(min(r),max(r),length.out=100)
y <- dnorm(x,mean(r),sd(r))
res_df <- data.frame(residuals=r)
norm_df <- data.frame(x=x,y=y)
rm(r,x,y)
ggplot() +
  geom_histogram(mapping=aes(x=residuals,y=stat(density)),data=res_df,bins=60) +
  geom_line(mapping=aes(x=x,y=y),col="red",data=norm_df)

#let's skip the boring stuff and jump to a partially pooled model



part_pool_model <- lmer(COVER ~ -1 +GENSPP_Updated2018+(1|PLOTID),data=only_carex, REML=F)

summary(part_pool_model)

#yet another bad fitted vs residual plot. Goddamn this data is not good. 
plot(no_pool_model)
plot(part_pool_model)

fitted(part_pool_model)[fitted(part_pool_model)>20]
#let's do it bayesian cuz I like bayesian

bae_partial_pool <- stan_lmer(COVER ~ 1 + (1|PLOTID),data = only_carex)

#so here intercept is basically the mean at the plot level. So if we take the plot means and avg them, we get that value.
#each plotid level is then a "random" intercept drawn from a distribution around that global intercept

print(summary(bae_partial_pool)[,c("mean","sd","n_eff","Rhat")],digits=3)

#now we try and pull out our posterior samples for given parameters I guess.

ps_samples <- extract(bae_partial_pool$stanfit)
#we've got six list elements here, alpha (our intercept), b (our plot estimates), aux (I have no idea, presumably
#this is how you play Death Grips on your friend's bayesian model?), theta_L (once again, no idea),
#mean PPD (probably a density or something? makin this up at this point), and lp__ (a very bad word puzzle. the answer
#lppp.)

#take two:
#alpha = intercept
#b = plot estimates
#aux = standard deviation estimate around plot? - this is across plots I am fairly sure
#theta_L = i think this is a transformation of alpha, b, and aux, giving some other estimate of alpha?
#mean_ppd = sample mean of the posterior predictive distribution for the outcome. So I think this is like our
#fited value then? Its the mean predicted outcome, so yeah.
#lp__ = this seems to be the log density up to our constant, the scale factor in posterior estimation that is basically
#all the non-parameter based likelihood. This is basically our p(y|p_post) from the classic bayes I think. Yeah I think
#this is basically the posterior we are exploring. So each entry in this is the posterior probability (?) of that
#given iteration

names(ps_samples)

launch_shinystan(bae_partial_pool)



#Pres/Abs test


######## Old presence absence data work ######

#This is some processing I did with the old presence absence data before realizing that the cover data had updated
#spp designations, with no clear way to see if I could apply those to the p/a data.



str(old_pres)
unique(old_pres$SPP)

old_pres$year <- gsub("(\\d{4})_\\d*","\\1",old_pres$PLOT)
old_pres$plot_id <- gsub("\\d{4}_(\\d*)","\\1",old_pres$PLOT)
old_pres$plot_id <- as.factor(old_pres$plot_id)


