## Inpatient hospital discharge dataset of Vermont state

#inpatient_2011 <- read.csv("https://raw.githubusercontent.com/kartucson/RFeb272016/master/Inpatient2011.csv")[1:100,c("intage","CHRGS","sex","ccsdxgrp")]
# d <- read.csv("https://raw.githubusercontent.com/kartucson/2015-07-24-R_introduction/gh-pages/training.csv")

in_2011 <- read.csv("Inpatient2011.csv")[,c("intage","CHRGS","sex","ccsdxgrp","pdays")]

system.time(
in_2011_all <- read.csv("Inpatient2011.csv")
)

dim(in_2011)
str(in_2011)
names(in_2011)

in_2011[,"sex"] <- as.factor(in_2011[,"sex"])
in_2011[,"ccsdxgrp"] <- as.factor(in_2011[,"ccsdxgrp"])

na_count <-sapply(in_2011, function(y) sum(length(which(is.na(y)))))

in_2011 <- na.omit(in_2011)
## ETL ##
in_2011$SCUD[is.na(in_2011$SCUD)] = mean(in_2011$SCUD,na.rm=T)

library(Rcmdr)
library(data.table)
library(dplyr)

# Use aggregate
CHRGS_agg <- aggregate(CHRGS~ccsdxgrp,data=in_2011,FUN = function(x) {nrow(x)}) 

# data table
in_2011_DT <- data.table(in_2011)
in_2011_DT_agg <- in_2011_DT[, mean(CHRGS, na.rm = TRUE),by = ccsdxgrp]

# dplyr
in_2011_dplyr_all <- in_2011 %>% select(CHRGS,ccsdxgrp) %>%    group_by(ccsdxgrp) %>% summarize(mean.CHRGS = mean(CHRGS,na.rm=T))
in_2011_dplyr_some_labels <- in_2011 %>% select(CHRGS,ccsdxgrp) %>% filter(ccsdxgrp %in% c(1:5)) %>%   group_by(ccsdxgrp) %>% summarize(mean.CHRGS = mean(CHRGS))
in_2011_dplyr_try_again <- in_2011 %>% select(CHRGS,ccsdxgrp) %>% filter(ccsdxgrp %in% c(1:5)) %>%   group_by(ccsdxgrp) %>% summarize(mean.CHRGS = mean(CHRGS,na.rm=T))

## Viz ##

library(ggplot2)
ggplot(in_2011, aes(x = pdays, y=CHRGS)) + geom_point()
ggplot(in_2011, aes(x = pdays, y=CHRGS)) + geom_point() + xlim(0,100) + ylim(0,50000) 
ggplot(data= in_2011, aes(x = intage, y=CHRGS))  +geom_point() 

ggplot(in_2011, aes(x = pdays, y=CHRGS)) + geom_point() + xlim(0,100) + ylim(0,50000) + aes(colour = in_2011$ccsdxgrp)

jpeg(file="Scatterplot.jpg")
ggplot(in_2011, aes(x = pdays, y=CHRGS)) + geom_point() + xlim(0,100) + ylim(0,50000) + aes(colour = in_2011$ccsdxgrp)
dev.off()

library(scatterplot3d)
#generate the scatterplot
attach(in_2011)
test<-scatterplot3d(CHRGS,pdays,intage,color='red',
                    pch=19,col.grid='lightblue')


plot(mtcars[c(1,3,5,6)])
library(lattice)
splom(mtcars[c(1,3,5,6)], groups=cyl, data=mtcars )

library(Rcmdr)
library(rgl)
library(vrmlgen)
library(rglwidget)
attach(mtcars)
scatter3d(wt, disp, mpg)
persp(x3,z3,zval,theta=320,phi=10,xlab='x1',
      ylab='z1',zlab='y1',ticktype='detailed')->res 
points(trans3d(x1,z1,y1,pm=res),col=2,pch=19)

#Commander()

plot3d(wt, disp, mpg, col="red", size=3)

# browseURL(file.path(getwd(), 'scatter.html'))

### Model development ## Dataset from Trevor & Hastie's statistical learning

## Objective: Understand variable importance in model (Feature selection) from Ordinary least squares perspective as well as non-parametric modeling perspective

adv <- read.csv("Advertising.csv")  ## The dataset used in class

adv2 <- adv[,2:5]

plot(adv$Sales,adv$TV)  ## Better correlation but possibly non-linear 
plot(adv$Sales,adv$Radio) ## Lesser correlation 

ggplot(data=adv,aes(x=Radio,y=Sales)) + geom_point() + geom_smooth(formula=y~x)

cor(adv$Sales,adv$Radio)

model_1 <- lm(Sales~ TV+Radio+Newspaper,data=adv)
summary(model_1)

summary(lm(Sales~TV+Radio, data=adv))          ## Single variable fit
lin1 <- lm(Sales~I(TV^0.5), data=adv)    ## Better fit
summary(lin1)

summary(lm(Sales~I(TV^0.5)+I(TV^2) + TV, data=adv))        ## Which is the best predictor? Can this be done? 
cor(adv$TV^0.5,adv$TV^2)  ## All inputs are pretty collinear
summary(lm(Sales~Radio*TV+  Radio*I(TV^0.5) + Radio*I(TV^2) , data=adv))  ## Interpretation is difficult as both terms of TV cancel each other 
summary(lm(Sales~Radio*TV, data=adv))  ## Simply using TV and Radio first order and interactions
summary(lm(Sales~Radio:TV+ Radio + I(TV^0.5), data=adv)) ## Better legit fit than previous one  
summary(lm(Sales~Radio*TV + I(TV^0.5), data=adv)) ## Best model as of now and we will stop at this in case of OLS  

library(caret)
library(glmnet)
library(randomforest)

X <- adv[,c("TV",        "Radio",     "Newspaper")]
X2	 <- X^2
X0.5	 <- X^0.5
Xall <- cbind(X,X2,X0.5)

Y <- adv$Sales

datam <- data.frame(X,Y)
datam_with_all_possibilities <- data.frame(Xall,Y)

cvfit <- cv.glmnet(as.matrix(X),Y)
coef(cvfit, s = "lambda.1se")
c<-coef(cvfit,s='lambda.min',exact=TRUE)
which(c!=0)

train_control <- trainControl(method="cv", number=10)  # 10 fold CV

model_train <- function(datain,method)
{
  model <- train(Y~., data=datain, trControl=train_control, method=as.character(substitute(method)))
  return (model)
}

model_train_with_interactions <- function(datain,method)
{
  model <- train(Y~.*., data=datain, trControl=train_control, method=as.character(substitute(method)))
  return (model)
}

system.time(   model.lasso <- model_train(datam,lasso) )  ## Lasso model
varImp(model.lasso)  ## Variable importance
model.lasso_Int <- model_train_with_interactions(datam,lasso) ## Lasso model with interactions
varImp(model.lasso_Int) ## Varialbe importance does not reflect the interactions

## Non-parametric approach
model.rf <- model_train(datam,rf)       ## Random forest
importance(model.rf$finalModel)
model.rf_Int <- model_train_with_interactions(datam,rf)  ## Random forest with interactions
importance(model.rf_Int$finalModel)    ## Takes Interactions into consideration
system.time( model.rf_all_possibilities <- model_train_with_interactions(datam_with_all_possibilities,rf) )

importance(model.rf_all_possibilities$finalModel)  ## Looks like Root(TV)*Root(Radio) is best fit
summary(lm(Sales~I(Radio^0.5)*I(TV^0.5), data=adv))  ## This effect is not captured due to overfitting of OLS regression  

#### CONCLUSION ####
# The OLS regression model as well as Random forest models suggest that interaction term TV:Radio is most significant to model Sales 







