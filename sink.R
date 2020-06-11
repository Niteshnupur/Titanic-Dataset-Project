

### loading the file from directory


titanic = read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\1st class\\titanic23.csv" , stringsAsFactors = FALSE)


titanic


head(titanic)


####showing the names of variables and their types


str(titanic)


#### showing the names variables

names(titanic)


### showing the content of the titanic$survived with their unique values

table(titanic$survived)


##### transferring the catogerical varibales

titanic$sex = factor(titanic$sex)

titanic$embarked = factor(titanic$embarked)

titanic$cabin = factor(titanic$cabin)

titanic$boat = factor(titanic$boat)

titanic$body = factor(titanic$body)




is.factor(titanic$sex)



##### calculating the how many missing vales are present


sum(is.na(titanic$sex))


sum(is.na(titanic$cabin))


sum(is.na(titanic$embarked))


sum(is.na(titanic$boat))


sum(is.na(titanic$body))


sum(is.na(titanic$home.dest))




#### checking the dimension (no of coloumns and no of rows)

dim(titanic)


summary(titanic)



##### deleting the high missing values variables which is not important for predicting the model

names(titanic)

titanic = titanic[-c(3,8,10,12,13,14)]

titanic

names(titanic)

summary(titanic)



str(titanic)


##########################



### checking the how many missing values are present in titanic$pclass

sum(is.na(titanic$pclass))


##### calculating the mean of titanic$pclass

pclass.mean = mean(titanic$pclass , na.rm = T)

pclass.mean



##### replacing the missing values with mean

titanic[is.na(titanic$pclass) , "pclass" ]= pclass.mean



sum(is.na(titanic$pclass))



######################


### checking the how many missing values are present in titanic$age

sum(is.na(titanic$age))


##### calculating the mean of titanic$age

age.mean = mean(titanic$age , na.rm = T)

age.mean



##### replacing the missing values with mean

titanic[is.na(titanic$age) , "age"] = age.mean


sum(is.na(titanic$age))


#####################################


### checking the how many missing values are present in titanic$sibsp

sum(is.na(titanic$sibsp))


##### calculating the mean of titanic$sibsp

sibsp.mean = mean(titanic$sibsp , na.rm = T)

sibsp.mean



##### replacing the missing values with mean

titanic[is.na(titanic$sibsp) , "sibsp"] = sibsp.mean


sum(is.na(titanic$sibsp))



#################################################


### checking the how many missing values are present in titanic$fare

sum(is.na(titanic$fare))


##### calculating the mean of titanic$fare

fare.mean = mean(titanic$fare , na.rm = T)

fare.mean


##### replacing the missing values with mean

titanic[is.na(titanic$fare) , "fare"] = fare.mean


sum(is.na(titanic$fare))



################################


### checking the how many missing values are present in titanic$parch

sum(is.na(titanic$parch))


##### calculating the mean of titanic$parch

parch.mean = mean(titanic$parch , na.rm = T)

parch.mean



##### replacing the missing values with mean

titanic[is.na(titanic$parch) , "parch"] = parch.mean

sum(is.na(titanic$parch))


#######################################################



##### calculating the how many missing values are present in entire data set

lapply(titanic , function(x) sum(is.na(x)))






#### creating the train and test data sample


set.seed(1)


s = sample(1:nrow(titanic), size = 0.7*nrow(titanic))
s

titanic_train = titanic[s,]
titanic_train


titanic_test = titanic[-s,]
titanic_test


head(titanic_train)

head(titanic_test)




##### splitting the titanic_train data set in to two parts
#####     (1) titanic_train1     (2)  titanic_train2


set.seed(2)

s = sample(1:nrow(titanic_train), size = 0.7*nrow(titanic_train))
s

titanic_train1 = titanic_train[s,]
titanic_train1


titanic_train2 = titanic_train[-s,]
titanic_train2


dim(titanic_train)

dim(titanic_train1)

dim(titanic_train2)




library(car)


form <-
  as.formula(paste0('survived ~ ', paste0(setdiff(
    names(titanic_train1), c("survived")
  ), collapse = ' + ')))



form



## For VIF calculation; we create a basic linear model
## lm = linear model
### we creating the linera model (lm)

vif_cal = lm(form , data = titanic_train1 )
vif_cal


### now calculating the vif


vif(vif_cal)




## k is the vif value initialized to a very high value


k = 100000000

## appended_dropped is a sentinal value which will change with k so that we don't run into infinite loop
appended_dropped = c('survived')

## loop will run untill all the values have vif lower than 4

while(k > 4){
  
  vif_cal=lm(form,data=titanic_train1) ## first a linear model for understanding the linear combination
  
  k <- sort(vif(vif_cal),decreasing = T)[1] ## get the value of vif for highest value
  
  if (k <= 4){
    break
  }
  var_dropped <- names(k) ## get the name of the variable which has highest value
  print(k)
  appended_dropped <- c(var_dropped, appended_dropped) ## update the sentinal


  form <-
    as.formula(paste0('survived ~ ', paste0(setdiff(
      names(titanic_train1), c("survived")
    ), collapse = ' + ')))  ## update the formula everytime

      

}



form







## creating the logistic model by using the glm function
### glm (generalised linear model)

fited=glm(form,data=titanic_train1,family = "binomial" )
fited



## run the stepwise function for calculating the AIC

fited=step(fited)


formula(fited)


summary(fited)



library(caTools)


## caTools to get the ROC:

## Run it to determine the ROC Plot

## Install caTools library



caTools::colAUC(predict(fited, titanic_train1, type = 'response'), 
                titanic_train1$survived, plotROC = TRUE )


caTools::colAUC(predict(fited, titanic_train2, type = 'response'), 
                titanic_train2$survived, plotROC = TRUE )



titanic_train1$score = predict(fited, data=titanic_train1)




library(ggplot2)


ggplot(data = titanic_train1, aes(x = score, y = survived, col = factor(survived))) + geom_point() + geom_jitter()



#### performance of score model on validation data

# install.packages('pROC')

library(pROC)



train.score = predict(fited,newdata = titanic_train1,type='response')
train.score



val.score = predict(fited,newdata = titanic_train2,type='response')
val.score


#comparing the auc for train and test

auc(roc(titanic_train1$survived,train.score))

auc(roc(titanic_train2$survived,val.score))



