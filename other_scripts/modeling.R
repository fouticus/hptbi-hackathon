# investigate joint characteristics of variables

################################################################################
## setup 

rm(list=ls()); gc()
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(tidyr)
library(GGally)
library(gtsummary)
library(randomForest)



################################################################################
### load data

df <- read.csv(file.path("csvs", "training.csv"))



################################################################################
### clustering y/n variables

ynvars <- grep("yn$", names(df), value=T)
#d <- dist(df[,ynvars], "manhattan")
pmort <- mean(df$hospdisposition == "Mortality")

X <- df[,c(ynvars, "age", "female")]
X$age <- (X$age-mean(X$age))/sd(X$age)
Y <- as.factor(as.integer(df$hospdisposition == "Mortality"))
rf <- randomForest(X, Y, ntree=1000, classwt=c(1-pmort, pmort))

hist(rf$votes)
df[(rf$predicted == "0") & (rf$y == 1),]

df$y <- rf$y
df$yh <- rf$predicted

df <- df %>% mutate(confusion = case_when(y==yh & yh==1 ~ "TP",
                                          y==yh & yh==0 ~ "TN",
                                          y!=yh & yh==1 ~ "FP",
                                          y!=yh & yh==0 ~ "FN"))
df %>%
  mutate(gcsed = ifelse(is.na(gcsed), -1, gcsed),
         gcsicu = ifelse(is.na(gcsicu), -1, gcsicu)) %>%
  ggplot(aes(gcsed, gcsicu, color=confusion)) + geom_jitter(size=3.0)

ctvars <- grep("^ct", names(df), value=T)
df[is.na(df[,ctvars]),]
df %>% View()

df %>% 
  ggplot(aes(ctce))

# include ct scan results
X2 <- df[,c(ynvars, ctvars, "age", "female")]
X2$age <- (X2$age-mean(X2$age))/sd(X2$age)
# impute missing with column mean
X2 <- apply(X2, 2, function(x){xbar <- mean(x, na.rm=T); x[is.na(x)] <- xbar; return(x)})
rf2 <- randomForest(X2, Y, ntree=1000, classwt=c(1-pmort, pmort))
rf2

rf2$importance
class(rf2$forest)
rf2$forest[[1]]
randomForest::getTree(rf2, 1, labelVar=TRUE)


# include only ct scan results
X3 <- df[,c(ctvars, "age", "female")]
X3$age <- (X3$age-mean(X3$age))/sd(X3$age)
# impute missing with column mean
X3 <- apply(X3, 2, function(x){xbar <- mean(x, na.rm=T); x[is.na(x)] <- xbar; return(x)})
rf3 <- randomForest(X3, Y, ntree=1000, classwt=c(1-pmort, pmort))
rf3
