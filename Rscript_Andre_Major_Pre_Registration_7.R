#.........................................................................................................................
#            This script was written for the preregistration for 
#            the project: Approaching or avoiding value? Pavliovian biases in intertemporal choices
#            in February 2017.
#
# Script started: 28/January/2017
# Script finished: 20/April/2017
# 
#.........................................................................................................................

##### necessary packages #####
install.packages("labeling")
install.packages('lme4')
install.packages("gtable")
install.packages("munsell")
install.packages("plyr")
install.packages("lazyeval")
install.packages("assertthat")
install.packages("DBI")
install.packages("magrittr")
install.packages("mnormt")
install.packages('reshape2')
install.packages('arm')
install.packages('DT')
install.packages('blme')
install.packages('shiny')
install.packages('haven')
install.packages('stringdist')
install.packages('coin')
install.packages("sjPlot", dependencies = T)
install.packages('reshape')
install.packages("coefplot")
install.packages("effects")
install.packages('car')
install.packages('psych')
install.packages('lsmeans')
install.packages('afex')
install.packages('pbkrtest')
install.packages('lattice')
install.packages('parallel')
install.packages("plotrix")
install.packages('ggplot2')
install.packages("influence.ME")
install.packages("pastecs")
install.packages("DHARMa")
install.packages("optimx")
install.packages("e1071")
install.packages("gvlma")
install.packages("multcomp")
install.packages("stringr")
install.packages("NMOF")
install.packages("stats4")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("data.table")

###load
library(dplyr)
library(NMOF)
library(stats4)
library(openxlsx)
library(labeling)
library(multcomp)
library(gvlma)
library (lme4)
library(optimx)
library (haven)
library (stringdist)
library (shiny)
library (DT)
library(gtable)
library(plyr)
library(munsell)
library (arm)
library(DBI)
library(assertthat)
library(lazyeval)
library (reshape2)
library(mnormt)
library(magrittr)
library (reshape)
library (blme)
library(DHARMa)
library(pastecs)
library(e1071)
library(influence.ME)
library (ggplot2)
library(plotrix)
library (parallel)
library (lattice)
library (pbkrtest)
library (afex)
library (lsmeans)
library (psych)
library (car)
library(effects)
library(coefplot)
library (coin)
library(sjPlot)
library(stringr)
library(data.table)


#######################################################################################
######################################################################################
########################CREATE FAKE DATA###########################
# 1) Create fake data set
# Extract predictors

"............................................................................................"
#################PLEASE CHANGE THE DIRECTORY###################
transfertrial <- read.csv("U:/Documents/Major/R/transfertrial.csv")
str(transfertrial)
# value_LL
value_LL <- str_sub(transfertrial$valuell, end = -6) # subtract "_Euro"
value_LL <- str_replace_all(value_LL, " ", "") # delete spaces
value_LL <- as.factor(value_LL) # to factor (with levels)
value_LL <- as.numeric(levels(value_LL))[value_LL] # to numeric
# value_SS
value_SS <- str_sub(transfertrial$valuess, end = -6)
value_SS <- str_replace_all(value_SS, " ", "")
value_SS <- as.factor(value_SS)
value_SS <- as.numeric(levels(value_SS))[value_SS]
# amount_SS
amount_SS <- transfertrial$SS
# amount_LL
amount_LL <- transfertrial$LL
# Delay
Delay <- transfertrial$Delayweeks

#because we are considering to use a quadratic term we use the function poly
#change and squared #how big is the LL in comparison to the LL in percetnage change?
relative_change <- poly(((amount_LL/amount_SS)-1)*100, 2)[, 1]
#change squared #how big is the LL in comparison to the LL in percetnage change and to what extent?
relative_change2 <- poly(((amount_LL/amount_SS)-1)*100, 2)[, 2]


# Create function to create dataset
create_fakeData <- function(N, intercept, main_value_SS, main_value_LL, main_amount_SS, main_relative_change, 
                            main_relative_change2, main_Delay, main_amount_LL, SD_intercept, SD_value_SS, SD_value_LL, 
                            SD_amount_SS, SD_amount_LL, SD_Delay, SD_relative_change, SD_relative_change2,
                            SD_error){ # create fake data set
  # Create predictors for dataset (all uPIDercase)
  PID <- rep(1:N, each = 100)
  Trial <- rep(1:100, times = N)
  Value_LL <- rep(value_LL, times = N)
  Value_SS <- rep(value_SS, times = N)
  Amount_SS <- rep(amount_SS, times = N)
  Amount_LL <- rep(amount_LL, times = N)
  Relative_change <- rep(relative_change, times = N)
  Relative_change2 <- rep(relative_change2, times = N)
  Delay <- rep(Delay, times = N)
  # Create z-values
  Value_LL_z <- scale(Value_LL) 
  Value_SS_z <- scale(Value_SS)
  Amount_SS_z <- scale(Amount_SS)
  Amount_LL_z <- scale(Amount_LL)
  Delay_z <- scale(Delay)
  # estimate random effect per person
  g_intercept <- rep(rnorm(N,intercept, SD_intercept), each = 100) # mean intercept per person, 100 observations per person
  g_main_value_SS <- rep(rnorm(N,main_value_SS, SD_value_SS), each = 100) #
  g_main_value_LL <- rep(rnorm(N,main_value_LL, SD_value_LL), each = 100) # 
  g_main_amount_SS <- rep(rnorm(N,main_amount_SS, SD_amount_SS), each = 100) #
  g_main_amount_LL <- rep(rnorm(N,main_amount_LL, SD_amount_LL), each = 100) #
  g_relative_change <- rep(rnorm(N,main_relative_change, SD_relative_change), each = 100) #
  g_relative_change2 <- rep(rnorm(N,main_relative_change2, SD_relative_change2), each = 100) #
  g_main_Delay <- rep(rnorm(N,main_Delay, SD_Delay), each = 100) # 
  # compute finally observed value (including error)
  tau <- g_intercept + g_main_value_SS*g_main_value_LL + g_main_amount_SS + g_relative_change*g_main_Delay + 
    g_relative_change2 + g_main_value_LL:g_main_value_SS:g_relative_change:g_main_Delay + rnorm(100*N,0,SD_error)
  # to exponential (hyperbolic discount)
  Choice <-  round(1/(1+exp(-tau)),0)
  mydata <- data.frame(PID, Value_SS_z, Value_LL_z, Amount_SS_z, Relative_change,
                       Relative_change2 ,Delay_z, tau, Choice, Amount_LL_z, Amount_SS, Amount_LL, Delay, Trial)
  return(mydata)
}

# Try out:
datatryfake <- create_fakeData(N=50, intercept=1, main_value_SS=3, main_value_LL=-3, main_amount_SS=4, 
                               main_relative_change=-4, main_relative_change2=0, main_Delay=1, main_amount_LL=-4,
                               SD_intercept=2, SD_value_SS=2, SD_value_LL=2, SD_amount_SS=2, SD_amount_LL=2,
                               SD_Delay=2, SD_relative_change=2, SD_relative_change2=2, SD_error=2)
View(datatryfake) 
summary(datatryfake)
head(datatryfake)
tail(datatryfake)
str(datatryfake)
########################################################################################################
#with the real data 
#setwd('U:\\Documents\\Major\\testing\\resultsR\\')
#file.list = list.files(pattern = '.csv')
#df.list <- sapply(file.list, read.csv, simplify=FALSE)
#df <- bind_rows(df.list, .id = "id")
#df$Trial <- seq.int(1, 100, each = 100)
#df
#str(df)
#df$id <- as.factor(df$id)

#generating factors variable
datatryfake$Choicef <- factor(datatryfake$Choice, levels=c('0','1'), labels=c('LL','SS'))
datatryfake$PID <- as.factor(datatryfake$PID)
summary(datatryfake)

#########tables to determine intercepts and slopes
with(datatryfake, table(PID,  Relative_change))
with(datatryfake, table(PID,  Amount_SS_z))
with(datatryfake, table(Delay_z, PID))
with(datatryfake, table(Value_LL_z, PID))
with(datatryfake, table(Value_SS_z, PID))
with(datatryfake, table(PID, Relative_change, Relative_change2))
with(datatryfake, table(PID, Value_LL_z, Value_SS_z))
with(datatryfake, table(PID, Delay_z, Choicef))
with(datatryfake, table(PID, Value_SS_z, Choicef))
with(datatryfake, table(PID, Value_LL_z, Choicef))

#some(many)plots of the data to get an idea of whats gonna happen in the model
with(datatryfake, lattice::densityplot(Value_SS_z))
with(datatryfake, lattice::densityplot(Value_LL_z))
plot(datatryfake$Relative_change, xlab = "Relative change SS/LL", xaxt = 'n')
plot(datatryfake$Relative_change2, xlab = "Relative change SS/LL", xaxt = 'n')

lattice::densityplot(~ Amount_SS_z | Value_SS_z, group = Choicef, auto.key = TRUE, data = datatryfake)
lattice::densityplot(~ Relative_change | Value_LL_z, group = Choicef, auto.key = TRUE, data = datatryfake)

lattice::xyplot(Choicef ~ Value_SS_z | Value_LL_z, data = datatryfake, type = c('g', 'p', 'r'))
lattice::xyplot(Choicef ~ Amount_SS_z | PID, data = datatryfake, type = c('g', 'p', 'r'))
lattice::xyplot(Choicef ~ Relative_change | PID, data = datatryfake, type = c('g', 'p', 'r'))
lattice::xyplot(Choicef ~ Value_SS_z | PID, data = datatryfake, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Choicef ~ Value_LL_z | PID, data = datatryfake, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Choicef ~ Relative_change | Value_LL_z, data = datatryfake, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Choicef ~ Amount_SS_z | Value_LL_z, data = datatryfake, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Choicef ~ Amount_SS_z | Value_SS_z, data = datatryfake, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Choicef ~ Amount_SS_z | Value_LL_z, data = datatryfake, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])



#######################################################################################
######################################################################################
########################Model 1 ###########################
#######################################


###################EXCLUSION CRITERIA###########
#simulate data for exlusion criteria
PIDx <- rep(1:50, each = 3)
response = rbinom(150, 1, 0.85)

responsef = NULL
for(i in 1:150){
  if(response[i] == 1){responsef[i] <- "correct"}
  else{responsef[i] <- "incorrect"}
}
responsef = as.factor(responsef)
dfexc = data.frame(PIDx, response, responsef)
dfexc
df_inc = subset(dfexc, dfexc$responsef == "incorrect" )
df_inc
#participant 22, 39 and 42 had more than 1 incorrect response, therefore will be excluded for model1.1 
#but will be included in model 1.2 in order to compare if learning the associated values has an effect
#on how the independent variables explain the dependent variable.

######WARNING Note: everytime the script is ran the number of participant
#id must be changed while using simulated data

df_no_inc = subset(datatryfake, datatryfake$PID != "27" & datatryfake$PID != "24" & datatryfake$PID != "19" ) 

#learned or not based on responses
#comented with is for real data
#setwd('U:\\Documents\\Major\\testing\\askingR\\')
#file.list = list.files(pattern = '.csv')
#df.list <- sapply(file.list, read.csv, simplify=FALSE)
#df2 <- rbindlist(df.list, use.names=T, idcol="id")
#df2
#str(df2)
#df2$id <- as.factor(df2$id)
#summary(df2)
#df_inc = subset(df2, df2$response == "incorrect" )
#tells us which participnats had incorrect reponses and how many 
#df_no_inc = subset(df2, df2$id != "outasking x.csv" & df2$id != "outasking x .csv" )




#####max model#####
#two models, one with all participants:
#MODEL 1.1
maxmodeltry <- glmer(Choicef ~ (Value_SS_z*Value_LL_z) * (Amount_SS_z + Relative_change + Delay_z + 
                       Relative_change2) + 
                       (1 + ((Value_SS_z*Value_LL_z) * (Amount_SS_z + Relative_change + Delay_z + 
                                                          Relative_change2)) | PID), 
                     data = datatryfake, family = binomial(link="probit"), 
                     control = glmerControl(optCtrl = list(maxfun = 1e+9)))

#trying other optimizers in case of convergence problems
maxmodel.Nltrp <- update(maxmodeltry, control=glmerControl(optimizer="nloptwrap", optCtrl = list(maxfun = 10e+9)))
maxmodel.NM <- update(maxmodeltry, control=glmerControl(optimizer="Nelder_Mead"))
maxmodel.nlimb <- update(maxmodeltry, control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
maxmodel.LBFGSB <- update(maxmodeltry,control=glmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))


##Model 1.2 Excludes participants who were incorrect at least 2 times
#note the different dataset.
#for practicality in this script diagnostics and plots will be ran with maxmodeltry
#
maxmodeltry_learnall <- glmer(Choicef ~ (Value_SS_z*Value_LL_z) * (Amount_SS_z + Relative_change + Delay_z + 
                                                            Relative_change2) + 
                       (1 + Value_SS_z*Value_LL_z) * (Amount_SS_z + Relative_change + Delay_z + 
                                                        Relative_change2 | PID), 
                     data = df_no_inc, family = binomial(link="probit"), 
                     control = glmerControl(optCtrl = list(maxfun = 1e+9)))

########assumption check or model fit
#########################Model diagnostics:

#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(maxmodeltry, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(maxmodeltry, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(maxmodeltry, scaled = TRUE)) > 3)/ length(resid(maxmodeltry))*100
sum(abs(resid(maxmodeltry, scaled = TRUE)) > 2.5)/ length(resid(maxmodeltry))*100
sum(abs(resid(maxmodeltry, scaled = TRUE)) > 2)/ length(resid(maxmodeltry))*100
#A boxplot showing for each participant the distr f { bution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(maxmodeltry, PID ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(maxmodeltry, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(maxmodeltry) ~ datatryfake$Choicef, smoother = T, xlim = c(0, 23), ylim = c(0, 15))
summary(maxmodeltry)

##Assumption: Error terms are independent and normally distributed
sjPlot::sjp.glmer(maxmodeltry, type = "ma")


#Dharma package for simulation testing in glmer
simulationOutput <- simulateResiduals(fittedModel = maxmodeltry, n = 1000) #create dataset based on the fitted model
simulationOutput$scaledResiduals #calculated residuals
#GOF is: a uniform (flat) distribution of the overall residuals and
#uniformity in y direction if we plot against any predictor.
#PLOTTING SCALED RESIDUALS
plotSimulatedResiduals(simulationOutput = simulationOutput)
#plot residuals against other predictors
plotResiduals(datatryfake$Value_SS_z, simulationOutput$scaledResiduals)
plotResiduals(datatryfake$Value_LL_z, simulationOutput$scaledResiduals)
plotResiduals(datatryfake$Amount_SS_z, simulationOutput$scaledResiduals)
plotResiduals(datatryfake$Relative_change, simulationOutput$scaledResiduals)
plotResiduals(datatryfake$Delay_z, simulationOutput$scaledResiduals)
plotResiduals(datatryfake$Relative_change2, simulationOutput$scaledResiduals)

#supporting visual inspections
testUniformity(simulationOutput = simulationOutput)
#KS test, overall uniformity of the residuals pvalue>.05 indicates uniformity
#Overdispersion / underdispersion
testOverdispersion(simulationOutput)
#advice overdipsrsion of 1.01 is acceptable even if the test is significant
#A significant value of 5 it's actually a problem it all deopends on the amount of data points 
#zero-inflation
#more xeros than expected
testZeroInflation(simulationOutput)
#Heteroscedasticity or systematic dependency of the dispersion / variance on another variable in the model.
#level of over/underdispersion depends on another parameter. 


###### for marginal effects

glmermfx <- function(x,nsims=1000){
  set.seed(1984)
  pdf <- mean(dlogis(-log((1-fitted(x))/fitted(x))))
  pdfsd <- sd(dlogis(-log((1-fitted(x))/fitted(x))))
  marginal.effects <- pdf*fixef(x)
  sim <- matrix(rep(NA,nsims*length(fixef(x))), nrow=nsims)
  for(i in 1:length(fixef(x))){
    sim[,i] <- rnorm(nsims,fixef(x)[i],diag(vcov(x)^0.5)[i])
  }
  pdfsim <- rnorm(nsims,pdf,pdfsd)
  sim.se <- pdfsim*sim
  res <- cbind(marginal.effects,sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(fixef(x))[1]=="(Intercept)",
         return(res[2:nrow(res),]),return(res))
}
summary(maxmodeltry)
glmermfx(maxmodeltry)


########## p values ##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
#Run your mixed() command (LRT, KR, PB)
maxmodeltry_lrt <- mixed(Choicef ~ (Value_SS_z*Value_LL_z) * (Amount_SS_z + Relative_change + Delay_z + 
                                                                Relative_change2) + 
                           (1 + (Value_SS_z*Value_LL_z) * (Amount_SS_z + Relative_change + Delay_z + 
                                                            Relative_change2) | PID),
                           family = binomial(link="probit"),type = 3,
                           method = "LRT", data = df_no_inc)
summary(maxmodeltry_lrt)
#check p-values
maxmodeltry_lrt$tests

summary(maxmodeltry_lrt)
#### plots
########################
#plotting marginal effects
mfx1 <- glmermfx(maxmodeltry)
mfxdata <- data.frame(cbind(rownames(mfx1),mfx1))
mfxdata[-c(1:6, 13:16, 18:20), ] #Ill have to choose the ones that are NOT significant
ggplot(mfxdata, aes(V1, marginal.effects, ymin = marginal.effects - 2*standard.error, ymax= marginal.effects + 2*standard.error)) +
  theme_bw() + 
  geom_errorbar(aes(x = V1, y = marginal.effects),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = marginal.effects)) +
  geom_hline(yintercept=0) + 
  coord_flip() + ggtitle("Marginal Effects with 95% Confidence Intervals")

#
sjPlot::sjp.int(maxmodeltry, type = "eff")
# plot fixed effects correlation matrix
sjPlot::sjp.glmer(maxmodeltry, type = "fe.cor")
# plot qq-plot of random effects
sjPlot::sjp.glmer(maxmodeltry, type = "re.qq")
#prob curve for fixed effects
sjPlot::sjp.glmer(maxmodeltry, type = "fe.pc")
summary(datatryfake)
# plot probability curves for each covariate
# grouped by random intercepts
sjp.glmer(maxmodeltry, type = "ri.pc", show.se = TRUE)
# plot probability curves for each covariate
# grouped by random intercepts
sjp.glmer(maxmodeltry, type = "ri.pc", facet.grid = FALSE, show.se = TRUE)
sjp.glmer(maxmodeltry, type = "ri.slope",
          vars = "Value_SS_z",
          show.ci = TRUE)
sjp.glmer(maxmodeltry, type = "ri.slope",
          vars = "Value_LL_z",
          show.ci = TRUE)
sjp.glmer(maxmodeltry, type = "ri.slope",
          vars = "Relative_change",
          show.ci = TRUE)
#FIXED EFFECTS
#model ASSUMPTION
sjp.glmer(maxmodeltry, type = "fe")
#predicted values of Choice for pavlovian associations LL grouped by pavlovian associations SS
sjp.glmer(maxmodeltry, type = "pred", vars = c("Value_LL_z", "Value_SS_z"), facet.grid = F)
#predicted values of Choice for pavlovian associations LL grouped by Participant
sjp.glmer(maxmodeltry, type = "pred", vars = c("Value_LL_z", "PID"), facet.grid = F)
#predicted values of Choice for pavlovian associations SS grouped by Participant
sjp.glmer(maxmodeltry, type = "pred", vars = c("Value_SS_z", "PID"), facet.grid = F)

datatryfake$OVprchoice

########ONCE BOTH MODELS (model 1.1 and model 1.2) are checked then they are compared
summary(maxmodeltry_learnall)
summary(maxmodeltry)
#assuming both models pass all the assumptions
#check difference on paremeters, do they vary much?


###########################################################################################
############################################################################################
##################### Manipulation check ##################
###simulate data based on what we expect from the data:

Pre_rates= NULL
Post_rates= NULL
valuesa=c(10,2,0,-2,-10)
Values <- rep(valuesa, times=50)

for (i in 1:250){
  Pre_rates[i] <- sample(1:10, 5, replace=F)
  Post_rates[i] <- sample(1:10, 5, replace=F)
  }
Pre_rates
Post_rates
Values
Figure
for (i in 1:250){
  if(Values[i] == 10 & Post_rates[i] <= Pre_rates[i]){Post_rates[i] <- Post_rates[i] + 
    (Pre_rates[i]-Post_rates[i]) + 2
  }else if(Values[i] == 2 & Post_rates[i] <= Pre_rates[i]){Post_rates[i] <- Post_rates[i] + 
    (Pre_rates[i]-Post_rates[i]) +1
  }else if(Values[i] == 0){Post_rates[i] <- Pre_rates[i]
  }else if(Values[i] == -2 & Post_rates[i] >= Pre_rates[i]){Post_rates[i] <- Post_rates[i] - 
    (Post_rates[i]-Pre_rates[i])-1
  }else if(Values[i] == -10 & Post_rates[i] >= Pre_rates[i]){Post_rates[i] <- Post_rates[i] - 
    (Post_rates[i]-Pre_rates[i])-2
  }
}

for (i in 1:250){
  if(Post_rates[i] > 10){Post_rates[i] = 10
  }else if(Post_rates[i] < 1){Post_rates[i] = 1
  }else{Post_rates[i] = Post_rates[i]}
}
Pre_rates
Post_rates
Values

Change = Post_rates - Pre_rates
Change
datacheckfake <-data.frame(Change, Values, Post_rates, Pre_rates)
summary(datacheckfake)

datacheckfake$PID <- rep(1:50, each = 5)
datacheckfake$PID <- as.factor(datacheckfake$PID)

Figure <- as.vector(sapply(1:50, function(x) sample(1:5, 5)))

for (i in 1:250){
  if(Figure[i] == 1){Figure[i] = "Rhombus"
  }else if(Figure[i] == 2){Figure[i] = "Circle"
  }else if(Figure[i] == 3){Figure[i] = "Pentagon"
  }else if(Figure[i] == 4){Figure[i] = "Square"
  }else if(Figure[i] == 5){Figure[i] = "Triangle"}
}

datacheckfake$Figure <- Figure

#someinfo
str(datacheckfake)
mean(datacheckfake$Change)

sd(datacheckfake$Change)

range(datacheckfake$Change, finite = FALSE)

#skewness
skewness(datacheckfake$Change)

#kurtosis
kurtosis(datacheckfake$Change)

stat.desc(datacheckfake$Change, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)


describe(datacheckfake$Change, skew = TRUE, ranges = TRUE)
#change order
datacheckfake$Values

#plots

densityplot(datacheckfake$Change)

boxplot(datacheckfake$Change)

hist(datacheckfake$Change, plot = TRUE)



datacheckfake$Values2 <- Values2
summary(datacheckfake)
Values
ggplot(datacheckfake, aes(x= Values, y= Post_rates)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(datacheckfake, aes(x= Values, y= Post_rates)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(datacheckfake, aes(x= Values2, y= Post_rates)) +
  geom_point() +
  geom_smooth(method = "loess")


ggplot(datacheckfake, aes(x= Values2, y= Post_rates)) +
  geom_point() +
  geom_smooth(method = "lm")


#factors
datacheckfake$Valuesf = factor(datacheckfake$Values,
                               levels= c("-10","-2","0","2","10"), labels=c("-10","-2","0","2","10") )
options(contrasts=c("contr.sum", "contr.poly"))

ordered(datacheckfake$Valuesf)

######outliers#####


datacheckfake$zChange <- scale(datacheckfake$Change, center = TRUE, scale = TRUE)
length(which(abs(datacheckfake$zChange)>3))


###########################################model2.1############################################
#is there a difference in how attractive were the figures for themeselves?
describeBy(datacheckfake$Pre_rates, group = datacheckfake$Values)

prelike <-lmer(Pre_rates ~ Figure + (1 |PID),
                 data = datacheckfake, 
                 control = lmerControl(optCtrl = list(maxfun = 1e+9)))



#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(prelike, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(prelike, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(prelike, scaled = TRUE)) > 3)/ length(resid(prelike))*100
sum(abs(resid(prelike, scaled = TRUE)) > 2.5)/ length(resid(prelike))*100
sum(abs(resid(regress_1, scaled = TRUE)) > 2)/ length(resid(regress_1))*100
#A boxplot showing for each participant the distr f { bution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(prelike, PID ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(prelike, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(prelike) ~ datacheckfake$Pre_rates, boxplots =FALSE, smoother = FALSE, xlim = c(0, 10), ylim = c(0, 10))
summary(prelike)

########## p values ##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
#Run your mixed() command (LRT, KR, PB)
prelike_lrt <- mixed(Pre_rates ~ Figure + (1 |PID), type = 3, method = "LRT", data = datacheckfake)
summary(prelike_lrt)
#check p-values
prelike_lrt$tests
lsmeans(prelike_lrt, pairwise~Figure)

###################################################model 2.2#######################################################

regress_1 <-lmer(Change ~ Valuesf + (1 |PID),
                 data = datacheckfake, 
                 control = lmerControl(optCtrl = list(maxfun = 1e+9)))
                 
               
options("scipen"=100, "digits"=4)
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(regress_1, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(regress_1, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(regress_1, scaled = TRUE)) > 3)/ length(resid(regress_1))*100
sum(abs(resid(regress_1, scaled = TRUE)) > 2.5)/ length(resid(regress_1))*100
sum(abs(resid(regress_1, scaled = TRUE)) > 2)/ length(resid(regress_1))*100
#A boxplot showing for each participant the distr f { bution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(regress_1, PID ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(regress_1, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(regress_1) ~ datacheckfake$Change, boxplots =FALSE, smoother = FALSE, xlim = c(-10, 10), ylim = c(-5, 5))
summary(regress_1)


########## p values ##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
#Run your mixed() command (LRT, KR, PB)
regress_lrt <- mixed(Change ~ Valuesf + (1 |PID), type = 3, method = "LRT", data = datacheckfake)
summary(regress_lrt)
#check p-values
regress_lrt$tests

summary(regress_1)

lsmeans(regress_lrt, pairwise ~ Valuesf)

ggplot(datacheckfake, aes(x= Values, y= Change)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(datacheckfake, aes(x= Values, y= fitted(regress_1))) +
  geom_point() +
  geom_smooth(method = "lm")



########################################################################################################
#####################################################################################################
#####################################################################################################
####################################EXTINCTIONTION MODEL###############################################
#model3.1
str(datatryfake)
EXTINCTION <- glmer(Choicef ~ Trial*(Value_SS_z*Value_LL_z) +
                    (1 + (Value_SS_z*Value_LL_z) | PID), 
                  data = datatryfake, family = binomial(link="probit"), 
                  control = glmerControl(optCtrl = list(maxfun = 1e+9)))


########assumption check or model fit
#########################Model diagnostics: will be replicated from Model 1.1

########## p values ##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
#Run your mixed() command (LRT, KR, PB)
EXTINCTION_lrt <- mixed(Choicef ~ Trial*(Value_SS_z*Value_LL_z) +
                          (1 + (Value_SS_z*Value_LL_z) | PID), 
                         family = binomial(link="probit"), type = 3,
                        method = "LRT", data = datatryfake)
summary(EXTINCTION_lrt)
#check p-values
EXTINCTION_lrt$tests

summary(EXTINCTION)

