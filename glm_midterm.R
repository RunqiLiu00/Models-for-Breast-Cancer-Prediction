#import data, label the class and name the columns
library(ggplot2)
library(easyGgplot2)
cancer <- read.csv("C:/Users/ADMIN/Desktop/breast-cancer-wisconsin.txt", header=FALSE)
View(cancer)
cancer$V11[cancer$V11==2]=0
cancer$V11[cancer$V11==4]=1
cancer$V11=factor(cancer$V11,levels=c(0,1),labels=c("benign","malignant"))
table(cancer$V11)

names(cancer)=c("sampleCodeNumber","clumpThickness","sizeUniformity",
               "shapeUniformity","marginalAdhesion","singleEpithelialCellSize",
               "bareNuclei","blandChromatin","normalNucleoli","mitoses","class")
attach(cancer)

table(cancer$class)
##Data exploration
par(mfrow=c(3,3))
boxplot(clumpThickness~class,col=c(2,3),ylab="clump thickness")
boxplot(sizeUniformity~class,col=c(2,3),ylab="size uniformity")
boxplot(shapeUniformity~class,col=c(2,3),ylab="shape uniformity")
boxplot(marginalAdhesion~class,col=c(2,3),ylab="marginal adhesion")
boxplot(singleEpithelialCellSize~class,col=c(2,3),ylab="single epithelial cell size")
boxplot(bareNuclei~class,col=c(2,3),ylab="bare nuclei")
boxplot(blandChromatin~class,col=c(2,3),ylab="bland chromatin")
boxplot(normalNucleoli~class,col=c(2,3),ylab="normal nucleoli")
boxplot(mitoses~class,col=c(2,3))
table(cancer$class)

library(corrgram)
data_cor=cor(cancer[,-c(1,11)])

corrgram(data_cor,order = FALSE,upper.panel = panel.cor,main="correlation matrix")

#split data
set.seed(1234)
train=sample(nrow(cancer),0.7*nrow(cancer))
cancer.train=cancer[train,]
cancer.validate=cancer[-train,]
table(cancer.train$class)
table(cancer.validate$class)

###logistic model

#full model
fit.full=glm(class~., data=cancer.train[,-c(1)],family=binomial())
summary(fit.full)


#delete non-significant variables and fit the model again
fit.reduced1=glm(class~clumpThickness+marginalAdhesion+bareNuclei+blandChromatin+normalNucleoli+mitoses, data=cancer.train,family=binomial())
summary(fit.reduced1)
#All variables in new model are significant
#Compare them using ANOVA
anova(fit.reduced1,fit.full,test="Chisq")

#stepwise regression
fit.step=step(fit.full)
summary(fit.step)
#stepwise regression gies the same model as fit.reduced2

#Interpreting Model Parameters
coef(fit.step)
exp(coef(fit.step))

#assess the model
prob=predict(fit.step, cancer.validate,type="response")
pred=factor(prob>0.5,levels=c(FALSE,TRUE),labels=c("benign","malignant"))
logit.perf=table(cancer.validate$class,pred,dnn=c("actual","predict"))
logit.perf

###Random forest
library(randomForest)
set.seed(666)
fit.forest=randomForest(class~.,data=cancer.train[,-c(1)],
                        na.action = na.roughfix,importance=TRUE)
fit.forest
importance(fit.forest,type=2)
##assess the model


forest.pred=predict(fit.forest,cancer.validate)
forest.perf=table(cancer.validate$class,forest.pred,dnn=c("actual","predict"))
forest.perf
