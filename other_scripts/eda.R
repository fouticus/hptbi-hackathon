################################################################################
## setup 

library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(tidyr)
library(GGally)
library(gtsummary)



################################################################################
### load data

df <- read.csv(file.path("csvs", "training.csv"))



################################################################################
### reponses

## mortality ##
table(df$hospdisposition)

## fss ##
dff <- na.omit(df[df$hospdisposition != "Mortality", grep("^fss", names(df))])
cor(dff)

SVD <- svd(dff)
plot(SVD$d)
matplot(SVD$v, type="b")

k <- 3
Xr <- SVD$u  %*% diag(c(SVD$d[1:k], rep(0, length(SVD$d)-k))) %*% t(SVD$v)
cor(Xr)


df %>% 
  pivot_longer(c("fssmental", "fsssensory", "fsscommun", "fssmotor", "fssfeeding", "fssresp")) %>%
  ggplot(aes(value)) + geom_bar() + facet_grid(name~sourcefss)
  
# comments:
# positive correlation b/t all pairs of fss
# first pc is intercept, second and third pc's differentiate fssmotor and fssfeeding from the rest
# Most test scores have origin "Other", and most low scores have source "other"


################################################################################
### predictors

## Age ##
df$agey <- df$age / 365

df %>% ggplot(aes(agey)) + geom_histogram(position="identity")
df %>% ggplot(aes(agey, fill=as.factor(female))) + geom_histogram(position="identity", alpha=0.5)
df %>% ggplot(aes(agey)) + stat_ecdf(geom="step")
df %>% ggplot(aes(agey, color=as.factor(female))) + stat_ecdf(geom="step")

# comments:
# mostly uniform dist over (0, 17.8)
# similar distributions by gender, more boys below 1 year.


## female ##
mean(df$female)

# comments:
# 39% female


## sourceinj ##

table(df$sourceinj)
table(df$sourceinj)/length(df$sourceinj)

# comments:
# ED emergency department: 21%
# Other provider: 30%
# Trauma surgery attending: 47%


## injurytoadmit ##
# duration from injury to admittance

table(df$injurytoadmit)
table(df$hospdisposition, df$injurytoadmit)

# Interesting case:
df %>% filter(injurytoadmit > 10)  
# known or suspected abuse
# Mortality

# comments: 
# mostly 1 and 2 days
# mortality mostly on 0 days, but one is 61 days. good case to keep track of.


## injury mech ##

table(df$injurymech)
table(df$injurymech, df$injurytoadmit)
table(df$injurymech, df$injurytoadmit, df$hospdisposition)
table(df$hospdisposition, df$injurymech)

# comments
# very few self harm


## gcs variables ##
# Glasgow coma scale

vars <- grep("^gcs", names(df), value=T); vars
vars_icu <- grep("icu$", vars, value=T); vars_icu
vars_ed <- grep("ed$", vars, value=T); vars_ed

summary(df[,vars_icu])
summary(df[,vars_ed])

for(var in vars_icu){
  print(var)
  print(table(df[,var]))
}

for(var in vars_ed){
  print(var)
  print(table(df[,var]))
}

hist(df$gcsed)
hist(df$gcsicu)

# comments:
# ICU:
# most were intubated, 
# most were sedated
# most were not chemically paralyzed.
# most eyes were not obstructed
# ED:
# most were intubated
# about have were sedated
# most were not chemically paralyzed
# most eyes were not obstructed


## eddisposition ##

table(df$eddisposition)
table(df$hospdisposition, df$eddisposition)

# comments
# most patients went to ICU after ED
# some went to OR

## admittoct ##

days until ct imaging

table(df$hospdisposition, df$admittoct)

# comments:
# most were CT on same day

## ctskullfrac ##
table(df$ctskullfrac) # about half
table(df$hospdisposition, df$ctskullfrac)
## ctsce ##
table(df$ctce) # about half
table(df$hospdisposition, df$ctce)
## ctsmidlineshift ##
table(df$ctmidlineshift) # most not
table(df$hospdisposition, df$ctmidlineshift)
## ctscompress ##
table(df$ctcompress) # most not
table(df$hospdisposition, df$ctcompress)
## ctsintraparhem ##
table(df$ctintraparhem) # a little less than half
table(df$hospdisposition, df$ctintraparhem)
## ctssubarchhem ##
table(df$ctsubarchhem) # about 1/3
table(df$hospdisposition, df$ctsubarchhem)
## ctsintraventhem ##
table(df$ctintraventhem) # few
table(df$hospdisposition, df$ctintraventhem)
## ctssubhematoma ##
table(df$ctsubhematoma) # about half
table(df$hospdisposition, df$ctsubhematoma) # mortality rare if this is 0
## ctsepihematoma ##
table(df$ctepihematoma) # few
table(df$hospdisposition, df$ctepihematoma)

# correlations
cor(df %>% select(starts_with("ct")) %>% na.omit()) %>% round(2)
# strongest relations: 
#    skullfrac - intraparhem
#    ce - compression
#    epihematoma - midline shift

df %>% mutate(ct = paste(ctskullfrac, ctce, ctmidlineshift, ctcompress, 
                         ctintraparhem, ctsubarchhem, ctintraventhem, ctsubhematoma, ctepihematoma, sep=":"))  %>%
  group_by(ct) %>% summarize(n = n()) %>% arrange(-n)
# Most common groups:
# 1. subhematoma and nothing else
# 2. nothing
# 3-4. skull fracture only, or skull fracture with subhematoma
# 5. subarachnoid hemorrhage and subhematoma

## sourceicu ##
table(df$sourceicu)
# mostly ICU MD, then ICU nursing flowsheet.

## puplrcticu ## 
table(df$puplrcticu)
table(df$hospdisposition, df$puplrcticu)

# "both fixed" has high chance of being mortality

## admittoicudc1 ##
table(df$admittoicudc1)
table(df$admittoicudc2)
table(df$admittoicudc3)
 
# number of icu admissions:
df %>% 
  mutate(n_icu = 1*!is.na(admittoicudc1) + 1*!is.na(admittoicudc2) + 1*!is.na(admittoicudc3)) %>% 
  select(n_icu) %>% table()

df %>% filter(!is.na(admittoicudc3)) %>% select(starts_with("admittoicu"))


## ventyn ##
table(df$ventyn)  # mostly yes.

## admittoint  and admittoext##
table(df$admittoint)
table(df$admittoext)
table(df$hospdisposition, df$admittoext-df$admittoint)

## ICP ##
table(df$hospdisposition, df$icptype1)
table(df$hospdisposition, df$icptype2)
table(df$hospdisposition, df$icptype3)

# ICP not super common overall

## cathtype1 ##
table(df$hospdisposition, df$cathtype1)
table(df$hospdisposition, df$cathtype2)
table(df$hospdisposition, df$cathtype3)

# given central venous for first type, about 50% mortality

## newtrachyn ##
table(df$newtrachyn) # very rare
## newgastyn ##
table(df$newgastyn) # rare
## decomcranyn ##
table(df$decomcranyn) # rare
## lmbrdrainyn ##
table(df$lmbrdrainyn) # very rare
## epihemyn ##
table(df$epihemyn) # rare
## subhemyn ##
table(df$subhemyn) # rare
## entnutyn ##
table(df$entnutyn) # very common

#look at all y/n indicators
df %>% 
  mutate(yn = paste(ventyn, newtrachyn, newgastyn, decomcranyn, lmbrdrainyn, 
                    epihemyn, subhemyn, tpnyn, entnutyn, cardiacarrestyn, sep=":")) %>%
  group_by(yn) %>% summarize(n=n()) %>% arrange(-n)
# most common:
# 1. ventilation and enteral nutrition
# 2. ventilation and cardiac arrest
# 3. ventilation and enteral nutrition and evacuation of epidural hematoma
# 4. ventilation and enteral nutrition and cardiac arrest
# 5. ventilation and decompression and cardiac arrest


## rxhypsal ##
table(df$rxhypsal) # about half
## rxmann ##
table(df$rxmann) # rare
## rxbarb ##
table(df$rxbarb) # rare
## rxinotrvas ##
table(df$rxinotrvas) # about 2/5

## hosplos ##
df %>% ggplot(aes(hosplos, fill=hospdisposition)) + 
  geom_histogram(position="identity", alpha=0.5) + 
  facet_grid(hospdisposition~.)

## cardiac arrest ##
table(df$cardiacarrestyn)  # rare
table(df$cardiacarresticu) # rare
table(df$cardiacarrested)  # very rare
table(df$cardiacarrestor)  # very rare
table(df$cardiacarrestprehosp) # rare
table(df$cardiacarrestother) # rare

df %>% mutate(ca = paste(cardiacarrestyn, cardiacarresticu, cardiacarrested,
                          cardiacarrestor, cardiacarrestprehosp, cardiacarrestother, sep=":")) %>%
  group_by(ca) %>% summarize(n=n()) %>% arrange(-n)
# most common:
# 1. nothing
# 2. yes: prehosp
# 3. yes: icu
# 4. yes: prehosp, icu
# 5. yes: ed


