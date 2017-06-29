setwd("~/3Fall2016/stat330/Exams")

poll <- read.table("PM.txt",header=TRUE)
head(poll)

# Data Summary #
require(ggplot2)
data.plot <- ggplot(poll,aes(x=Cars,y=Particles))
data.plot + geom_point() + geom_smooth() + theme_grey() +
  ggtitle("Particles by Cars") + xlab("Cars") + ylab("Particles")

with(poll,{cor(Particles,Cars)})

lm.dat <- with(poll,{lm(Particles~Cars)})
summary(lm.dat)

library(MASS)
hist(stdres(lm.dat),main="Histogram of Residuals")

plot(lm.dat$fitted.values,lm.dat$residuals,
     main="Residuals vs. Fitted Values",
     xlab="Fitted Values",ylab="Residuals")
abline(h=0,col='red')

# Log(y) Transformation #
#install.packages("ggplot2")
require(ggplot2)
poll.plot <- ggplot(poll,aes(x=sqrt(Cars),y=log(Particles)))
poll.plot + geom_point() + geom_smooth() + theme_grey() +
  ggtitle(paste("Particles by Cars","\n(After Transformation)")) + xlab("Cars") + ylab("Particles")

with(poll,{cor(Cars,log(Particles))})

lm.poll <- with(poll,{lm(log(Particles)~Cars)})
summary(lm.poll)

library(MASS)
hist(stdres(lm.poll),main=paste("Histogram of Residuals",
                                "\n(After Transformation)"))

plot(lm.poll$fitted.values,lm.poll$residuals,
     main=paste("Residuals vs. Fitted Values",
                "\n(After Transformation)"),
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0,col='red')

# Model Verification #

pred.width <- numeric(0)
coverage <- numeric(0)
bias <- numeric(0)
rpmse <- numeric(0)
for (i in 1:1000){
  n <- 50
  sampl <- sample(1:length(poll$Cars),n)
  
  train <- poll[-sampl,]
  test <- poll[sampl,]
  train.lm <- with(poll,{lm(log(Particles)~Cars)})
  pred.poll <- data.frame(Cars=test$Cars,Particles=test$Particles)
  pred.part <- predict.lm(train.lm,pred.poll)
  pred.poll$predpart <- exp(pred.part)
  bias[i] <- mean(pred.poll$predpart - pred.poll$Particles)
  rpmse[i] <- sqrt(mean((pred.poll$predpart - pred.poll$Particles)^2))
  
  pred.int <- predict.lm(train.lm,test,
                         interval="prediction",level=.95)
  covers <- mean(exp(pred.int[,2]) < test$Particles & 
                   test$Particles < exp(pred.int[,3]))
  coverage[i] <- covers
  int.width <- mean(exp(pred.int[,3]) - exp(pred.int[,2]))
  pred.width[i] <- int.width

}
mean(bias)
mean(rpmse)
mean(coverage)
mean(pred.width)




# Results #

summary(lm.poll) # p-value here
exp(lm.poll$coefficients)
exp(confint(lm.poll)) # estimates here



# A prediction #

dframe <- data.frame(Cars=c(1800))
dframe$pred <- exp(predict.lm(lm.poll,newdata=dframe,
           interval="prediction",level=.95))
dframe


# Conclusions #


