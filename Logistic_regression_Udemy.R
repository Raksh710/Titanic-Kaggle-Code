df.train <- read.csv('titanic_train.csv')
missmap(df.train , main = 'Missing Map' , col=c('yellow','black'),legend = F)

library(ggplot2)

attach(df.train)

print(ggplot(df.train,aes(Survived)) + geom_bar())

print(ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill = factor(Pclass))))

print(ggplot(df.train,aes(Sex)) + geom_bar(aes(fill = factor(Sex))))

print(ggplot(df.train,aes(Age)) + geom_histogram(aes(bins=20,fill='blue',color='blue',alpha=0.5)))

print(ggplot(df.train,aes(SibSp)) + geom_bar())

print(ggplot(df.train,aes(Fare)) + geom_histogram(aes(bins=20,fill='green',color='black',alpha=0.5)))

#FILLING IN THE MISSING DATA BY AVERAGE PASSENGER CLASS VALUE



pl <- ggplot(df.train , aes(x=factor(Pclass),y=Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.5))

pl <- pl + scale_y_continuous(breaks = seq(min(0),max(80),by=2)) + theme_bw()

print(pl)



#IMPUTATION OF AGE BASED ON CLASS



impute_age <- function(age,class){
  
  out <- age
  
  for (i in 1:length(age)) {
    
    if(is.na(age[i])){
      
      if(class[i]==1){
        
        out[i] <- 37
        
        
        
      }else if(class[i]==2){ out[i] <- 29}
      
      else {out[i] <- 24}
      
    }else{
      
      
      
      out[i] <- age[i]
      
    }
    
  }
  
  return(out)
  
}



fixed_ages <- impute_age(Age,Pclass)

df.train$Age <- fixed_ages

missmap(df.train , main='MISSING MAP',col = c('yellow','black'),legend = F)



#CLEANNG OUT THE UNNECCESSARY DATA



library(dplyr)

df.train <- df.train[,-c(1,4,9,10,11)]

df.train$Survived <- factor(df.train$Survived)

df.train$Pclass <- factor(df.train$Pclass)

df.train$Parch <- factor(df.train$Parch)

df.train$SibSp <- factor(df.train$SibSp)

df.train$Embarked <- factor(df.train$Embarked)

df.train$Sex <- factor(df.train$Sex)

#CREATING LOGISTIC MODEL

log.model <- glm(data = df.train , family = binomial(link = 'logit') , Survived~Pclass+Sex+Age)

summary(log.model)



#MAKING TRAINING AND TEST DATA IN THIS TRAINING DATA

library(caTools)

set.seed(101)

split <- sample.split(df.train$Survived , SplitRatio = 0.7)

final.train <- subset(df.train , split==T)

final.test <- subset(df.train , split==F)

final.log.model <- glm(Survived~Age+Sex+Pclass , data = final.train , family = binomial(link = 'logit'))

final.prob <- predict(final.log.model , final.test , type = 'response')

final.pred <- rep(0 , 268)

final.pred[final.prob > 0.5] <- 1

table(final.pred , final.test$Survived)

print(mean(final.pred==final.test$Survived))



# ANOTHER WAY IS USING IFELSE COMMAND

fitted.results <- ifelse(final.prob > 0.5 , 1 , 0)

misclassification.error <- mean(fitted.results != final.test$Survived)


######################################



df.test <- read.csv('titanic_test.csv')


attach(df.test)

impute_age2 <- function(age,class){
  
  out <- age
  
  for (i in 1:length(age)) {
    
    if(is.na(age[i])){
      
      if(class[i]==1){
        
        out[i] <- 37
        
        
        
      }else if(class[i]==2){ out[i] <- 29}
      
      else {out[i] <- 24}
      
    }else{
      
      
      
      out[i] <- age[i]
      
    }
    
  }
  
  return(out)
  
}



fixed_ages2 <- impute_age2(df.test$Age,df.test$Pclass)

df.test$Age <- fixed_ages2



library(dplyr)

df.test <- df.test[,-c(4,9,10,11)]

#df.test$Survived <- factor(df.test$Survived)

df.test$Pclass <- factor(df.test$Pclass)

df.test$Parch <- factor(df.test$Parch)

df.test$SibSp <- factor(df.test$SibSp)



predictaionontesstdata <- predict(log.model , df.test ,  type = 'response')

df.test$Survived <- predictaionontesstdata



df.test$Survived <- ifelse(df.test$Survived >= 0.5,1,0)

head(df.test)


write.csv(df.test , file = 'Final_Results.csv')


