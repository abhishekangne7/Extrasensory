

---
title: "Final_Thesis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r loading the dataset}
knitr::opts_knit$set(root.dir = 'D://ExtraSensory//Retry1//')
```

## Including Plots

You can also embed plots, for example:

```{r library}

library(tidyverse)
library(lubridate)
library(reshape2)
library(dplyr)
library(corrplot)
library(ggplot2)
library(anytime)
library(scales)
library(pvclust)
library(caret)
library(cluster)
#library(factoextra)
library(mlbench)
library(moments)
library(gridExtra)
library(rpart)
library(partykit)
library(e1071)
library(randomForest)
library(rattle)
library(keras)
```

```{r}
#Saving all the files to a temporary list 
TempList = list.files('D://ExtraSensory//Final_Thesis//', pattern = '.*csv')
#reading the csv files
data <- (read.csv(file=paste("D://ExtraSensory//Final_Thesis//",TempList[1], sep=""), header=TRUE, sep=","))


#Your data tells a story. Tell it with R Markdown. Turn your analyses into high quality documents, reports, presentations and dashboards.
```

```{r}
#str_extract(string, pattern) #Using the str_extract command, I'm trying to look for a particular pattern, that has any value or combinatuion of a-z and/or 0-9
#which is exactly what our files look like

id <- str_extract(TempList[1],"^[A-Z|0-9-]*")
idColumn <-  data.frame(c(rep(id,nrow(data))))
idUser <- idColumn
colnames(idUser)[1] <- "idUser"

for(i in 2:length(TempList)){
  temp <- (read.csv(file=paste("D://ExtraSensory//Final_Thesis//", TempList[i], sep=""), header=TRUE, sep=","))
  id <- str_extract(TempList[i],"^[A-Z|0-9-]*")
  idColumn <- data.frame(c(rep(id,nrow(temp))))
  colnames(idColumn)[1] <- "idUser"
  idUser <- rbind(idUser, idColumn)
  data <- rbind(data, temp)
}
```


```{r}
 #Pre-Analysis

##Before starting the analysis we will try to eliminate or replace the null values. The discrete values will be replaced by mode and continuous values by the average.

### 1.First, we will analyze whether we have a whole column of 'NA' values in which case we will eliminate it.
### 2.The column that exceeded 70% with null values will be eliminated.
### 3.The remaining values will be replaced.


#Funcion for calculation of mode

getmode <- function(v) {
  Vuniq <- unique(v)
  Vuniq[which.max(tabulate(match(v, Vuniq)))]
}

columns_eliminate <- c()
data <- data[,colSums(is.na(data))<nrow(data)]
j <- 0
for(i in 1:ncol(data)){
  if(sum(is.na(data[,i])) > 0){
    if(((sum(is.na(data[,i]))*100)/(sum(!is.na(data[,i]))+sum(is.na(data[,i])))) >= 70 ){
      cat("The columns removed as they exceeding the threshold of NA's",colnames(data[i]),"(>=70% NA's)\n")
      columns_eliminate[j] <- i;
      j<-j+1;
    }
  }
}

print(columns_eliminate)

for(i in 1:ncol(data)){
  if(startsWith(as.character(colnames(data[i])), "label")){
    if((getmode(data[,i])) == 'NaN') data[is.na(data[,i]), i] <- 0
    else data[is.na(data[,i]), i] <- getmode(data[,i])
    
  }
  else{
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
}

data_individual <- cbind(data, idUser)
data <- arrange(data, data$timestamp)


```
```{r}
#Coding the output

## Looking at the labels with the number of minutes/examples spend by all the users 

labels <- data

# From the given set of activities, we will classify an individuals activity based on six prominent actions and the remaining will be classiified as a separate activity
labels <- select(data, (c('label.SITTING', 'label.FIX_walking', 'label.FIX_running', 'label.BICYCLING', 'label.LYING_DOWN', 'label.PHONE_ON_TABLE'))) ##6 activities + 1 Other Activity

z <- 1
code.exit <- c()


for(i in 1:nrow(labels)){
  next_tag <- 0
  for(j in 1:ncol(labels)){
    if (labels[i,j] == 1){
      if(next_tag == 1){
        code.exit[z] <- paste(code.exit[z] , colnames(labels[j]), sep='+');
      }
      else{
        code.exit[z] <-colnames(labels[j]);
      }
      
      next_tag <- 1
      
    }
    
    
  }
  if(next_tag == 0) code.exit[z] <- "Other activity"
  z<-z+1;
}

##

sensors <- select(data, matches("(^raw_acc|^proc_gyro|^raw_magnet|^watch_acceleration|^watch_heading|^location|^location_quick_features|^audio_naive|^audio_properties|^discrete|^lf_measurements)"))
nameSensor <- "empty"
typeSensor <- "Na"
for(i in 1:length(sensors)){
  if(nameSensor == unlist(strsplit(names(sensors[i]), ".", fixed = TRUE))[[1]]){
    next;
  } 
  
  nameSensor <- unlist(strsplit(names(sensors[i]), ".", fixed = TRUE))[[1]]
  cat(sprintf("%-40s %s\n",names(sensors[i]),typeof(sensors[[i]])))
}

sensors <- select(data, matches("(^proc_gyro|^raw_acc)"))
sensors[is.na(sensors)] <- 0
oc_cor <- cor(sensors)
oc_cor[is.na(oc_cor)]  <- 0
corrplot(oc_cor, method="color",type = "full", order = "hclust", tl.col = "black", tl.cex = 0.6 )


##As you can see the sensors (accelerometer and gyroscope), they have both positive and negative correlations between attributes. This makes us think that by applying some feature selection technique we could obtain the most representative variables.

```
```{r}
##Since we have correlations, I will execute the algorithm of simple elimination of correlation variables by threshold, the objective is to remain with fewer variables since with many variables an overfit can be produced. In order to execute it, the algorithm must not receive constant variables (standard deviation 0), nor columns with null values, which we have already discussed.

features <- select(data, -starts_with('label'))
features <- select(features, -starts_with('timestamp'))
colnum_features <- ncol(features)
###We remove the columns where the standard deviation is zero.
features<- Filter(var, features)
cat("We had a total of variables before eliminating: ",colnum_features, ". Now we have a total of: ",ncol(features))
remove = findCorrelation(cor(features), cutoff=0.9)
cat("The variables that could be removed are",colnames(features[remove]),"\n", "We have a total of variables to eliminate from: ",length(remove)," varibles.\n")
features <- select(features, -c(remove))
cat("We had a total of variables before eliminating: ",colnum_features,"Now we have a total of:",ncol(features)," variables.")
###corrplot(cor(features), type = "upper", tl.pos = "td", method = "circle", tl.cex = 0.5, tl.col = 'black')
###Later we will make use of algorithms to stay the most "important" predictors and thus be able to reduce it even more.

```
```{r}
# Distribution of data from sensors

##Now we will see how the data is distributed, to be able to know what type of distribution our data follows. For this I use the functions: skewness and kurtosis. From the library moments. I will show only the first 10 points in the data, the other half I omit to not make the document too long.


for(i in 1:10){
  cat("Skewness y kurtosis:",skewness(features[,i]),"/",kurtosis(features[,i]),colnames(features[,i]),"\n")
  
}

#If the skewness is zero it means that the data follows a normal distribution.
"However, if we have a positive skewness greater than zero, the tail of the distribution points to the right. The kurtosis will tell us how weeping the data is, that is, if it is positive and the higher the value of the kurtosis the more weights are the data. For example, we can see this situation in the variable raw_acc.magnitude_stats.moment3 Plotting we will see it. It is not enough to see these few,
but still we will have to normalize due to the specifications of the machine learning algorithms"

ggplot(features, aes(x=features$raw_acc.magnitude_stats.moment3)) + geom_density()
ggplot(features, aes(x=features$raw_acc.magnitude_stats.moment3)) + geom_histogram(aes(y=..density.., fill=..count..)) + stat_function(fun=dnorm,color="red",args=list(mean=mean(features$raw_acc.magnitude_stats.moment3),sd=sd(features$raw_acc.magnitude_stats.moment3)))
qqnorm(features$raw_acc.magnitude_stats.moment3)
```
```{r}

"Our data does not follow a normal distribution. Therefore, our data needs to be transformed, in this case scaled to be able to normalize them. Why? Because we have to put them on a common scale, since this will improve the understanding of their distribution and be able to compare them more easily avoiding the distortion of scale difference as well as the fact that in this way problems with the algorithms are avoided of adjustment of models that do not possess the property of invariance to scaling, for example, algorithms based on descent gradient (such as neural networks that use backpropagation).

#Subjects

###The ExtraSensory dataset contains data from 60 users (34 subjects were female and 26 were male). The users would use their own phone, and possibly additional convenient devices, like watches (56 out of the 60 agreed to wear the watch). For the purpose of large-scale data collection the creators of extrasensory developed a mobile application for both iphone and android and for smartwatch (Pebble). The app was used to collect both sensor measurement and context labels. Every minute the app records a 20sec window of sensor measurements from the phone and watch.

#We can see the different context labels and the time that we have."

labels <- select(data, starts_with('label'))
timeLabel <- select(labels, -label_source)
timeLabel <- apply(timeLabel,2,function(x){sum(as.numeric(x), na.rm = TRUE)})
timeLabel <- sort(timeLabel, decreasing = TRUE)

for (i in 1:length(timeLabel)){
  cat(sprintf("%-40s %d minutes \n",names(timeLabel[i]),unname(timeLabel[i])))
  
}


```
```{r}
#We can intuit from the images that there is a relationship between 00:00 and 08:00 with the context labels: #sleeping and lying down. But it's hard to appreciate with all the individuals together.

#We can intuit from the images that there is a relationship between 00:00 and 08:00 with the context labels: sleeping and lying down. But it's hard to appreciate with all the individuals together.

# Correlation matrix between the context labels

## With the correlation matrix it is easier to see the correlations between the labels.

oc <- select(data, starts_with('label'))
oc <- select(oc, -starts_with('label_source'))
oc[is.na(oc)] <- 0
oc <- oc[, colSums(oc) > 1]
oc_cor <- cor(oc)
oc_cor[is.na(oc_cor)]  <- 0
corrplot(oc_cor, method="color",type = "full", order = "hclust", tl.col = "black", tl.cex = 0.6 )


##I have also used the hierarchical clustering technique using the agglomerative method (AGNES). Clustering is a technique to group similar data points in a group and separate the different observations into different groups.  In Hierarchical Clustering, clusters are created so that they have a predetermined. 

#We can see groupings that we have detected before visualizing the data.
set.seed(123)
oc <- select(data, starts_with('label'))
oc <- select(oc, -starts_with('label_source'))
oc[is.na(oc)] <- 0
oc <- oc[, colSums(oc) > 1]
#oc <- t(oc)
oc.pre <- preProcess(oc,method="scale")
oc.scaled <- predict(oc.pre, oc)
oc.diana <- agnes(t(oc.scaled), metric="euclidean")
pltree(oc.diana,cex=.6)

#Here I set K equal to 8, to point out the clusters (It's a random value)

set.seed(123)
oc <- select(data, starts_with('label'))
oc <- select(oc, -starts_with('label_source'))
oc[is.na(oc)] <- 0
oc <- oc[, colSums(oc) > 1] #I avoid columns without more than one example
#oc <- t(oc)
oc.pre <- preProcess(oc,method="scale")
oc.scaled <- predict(oc.pre, oc)
oc.agnes <- agnes(t(oc.scaled), metric="euclidean")
pltree(oc.agnes, hang=-1, cex = 0.6)

##Ref: http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
rect.hclust(oc.agnes, k = 8, border = 2:10)


##Now we can visualize some groups that we have found using clustering techniques or fixing ourselves in the correlation matrix, since they make sense. For example, when you are at home lying down you are sleeping.


```

```{r}
record_table <- data_individual %>% group_by(idUser) %>% summarise(LYING_DOWN = sum(label.LYING_DOWN, na.rm = TRUE), SITTING= sum(label.SITTING, na.rm = TRUE), FIX_walking= sum(label.FIX_walking, na.rm = TRUE), FIX_running= sum(label.FIX_running, na.rm = TRUE), BICYCLING= sum(label.BICYCLING, na.rm = TRUE), PHONE_ON_TABLE= sum(label.PHONE_ON_TABLE, na.rm = TRUE))
record_table <- arrange(record_table, record_table$LYING_DOWN, record_table$SITTING, record_table$FIX_walking, record_table$FIX_running, record_table$BICYCLING)

record_table

```
```{r}
#Due to a non-existant set of activities in running and bicycling, we chose only 3 relevant ones
record_table <- data_individual %>% group_by(idUser) %>% 
summarise(SITTING= sum(label.SITTING, na.rm = TRUE), 
FIX_walking= sum(label.FIX_walking, na.rm = TRUE),
LYING_DOWN= sum(label.LYING_DOWN, na.rm = TRUE))

record_table <- arrange(record_table, record_table$SITTING, record_table$FIX_walking, record_table$LYING_DOWN)

record_table
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
```{r}
set.seed(123)
seconds_in_day = 24*60*60
time1 <- as.data.frame(sin(2*pi*data["timestamp"]/seconds_in_day))
time2 <- as.data.frame(cos(2*pi*data["timestamp"]/seconds_in_day))
colnames(time1) <- "timeSin"
colnames(time2) <- "timeCos"
cyclic_time <- cbind(time1, time2)
data<-cbind(cyclic_time, data)
ggplot(data, aes(timeSin))+geom_density()
ggplot(data, aes(timeCos))+geom_density()
ggplot(data, aes(timeSin, timeCos))+geom_point()
hours <- anytime(data$timestamp, tz="PST8PDT")
hours <- format(as.POSIXct(hours, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%Y-%m-%d %H:%M")
hours <- format(as.POSIXct(hours, "%Y-%m-%d %H:%M", tz = ""), format = "%H")
```
```{r}
set.seed(123)
timeSin <- select(data, starts_with('timeSin'))
timeCos <- select(data, starts_with('timeCos'))
labels <- select(labels, -starts_with("label_source"))
```


```{r}
#Recoding the output
#Coding the output

## Looking at the labels with the number of minutes/examples spend by all the users 

labels <- data

labels <- select(data, (c('label.SITTING', 'label.FIX_walking', 'label.LYING_DOWN')))
z <- 1
code.exit <- c()


for(i in 1:nrow(labels)){
  next_tag <- 0
  for(j in 1:ncol(labels)){
    if (labels[i,j] == 1){
      if(next_tag == 1){
        code.exit[z] <- paste(code.exit[z] , colnames(labels[j]), sep='+');
      }
      else{
        code.exit[z] <-colnames(labels[j]);
      }
      
      next_tag <- 1
      
    }
    
    
  }
  if(next_tag == 0) code.exit[z] <- "Other activity"
  z<-z+1;
}
```

```{r}
#We already have a part of the features ready.
#the NA labels as zeros
for(i in 1:ncol(labels)){
labels[is.na(labels[,i]), i] <- 0
}
data_all <- cbind(features, code.exit)
data_all <- cbind(timeSin, data_all)
data_all <- cbind(timeCos, data_all)

table(data_all$code.exit)
EStageData <- data_all
write.csv(EStageData,"D://Thesis//EDA//data_all.csv", row.names = FALSE)
EStageData <- data_all
write.csv(EStageData,"D://Thesis//EDA//data_all.csv", row.names = FALSE)
write.csv(EStageData,"D://Thesis//EDA//data1_all.csv", row.names = FALSE)
preProcMod<-preProcess(data_all[colnames(features)],method=c("center","scale"))
data_all.Transf<-predict(preProcMod,data_all)
Vars_Entry <- colnames(select(data_all.Transf, -starts_with('code.exit')))
```

```{r}
set.seed(123)
data <- data_all
data_all.rf <- randomForest(code.exit ~ ., data=data_all, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)

varImpPlot(data_all.rf)
```

```{r}
preProcMod<-preProcess(data_all[colnames(features)],method=c("center","scale"))
# Comment
data_all.Transf<-predict(preProcMod,data_all)
Vars_Enter <- colnames(select(data_all.Transf, -starts_with('code.exit')))

```

```{r}
library(caret)
set.seed(123)
train.index <- createDataPartition(data_all.Transf$code.exit, p = .7, list = FALSE)
train <- data_all.Transf[ train.index,]
test  <- data_all.Transf[-train.index,]
dfte<-data_all.Transf[1:10000,]
#randomForest <- randomForest( x= data_all.Transf[Vars_Enter], y = as.factor(data_all.Transf$code.exit),n_tree=3)
rf <- randomForest(x=train[1:178],y=as.factor(train[,179]))
cat("Random forest information: ")
print(rf)
saveRDS(rf, "rf.rds")

ypred<-predict(rf,test[1:178])

tstab<-table(test[,179],ypred)

confusionMatrix(tstab)

```
```{r}


 library(xgboost)


  
# Create a training and validation sets
#trainObs <- sample(nrow(train), .8 * nrow(train), replace = FALSE)
#valObs <- sample(nrow(train), .2 * nrow(train), replace = FALSE)


# Create numeric labels with one-hot encoding
train_labs <- as.numeric(train$code.exit) - 1
test_labs <- as.numeric(test$code.exit) - 1

new_train <- model.matrix(~ . + 0, data = train[, 1:178])
new_test <- model.matrix(~ . + 0, data = test[, 1:178])

# Prepare matrices
xgb_train <- xgb.DMatrix(data = new_train, label = train_labs)
xgb_test <- xgb.DMatrix(data = new_test, label = test_labs)

# Set parameters(default)
params <- list(booster = "gbtree", objective = "multi:softprob", num_class = 6, eval_metric = "mlogloss")

# Calculate # of folds for cross-validation
xgbcv <- xgb.cv(params = params, data = xgb_train, nrounds = 100, nfold = 5, showsd = TRUE, stratified = TRUE, print.every.n = 10, early_stop_round = 20, maximize = FALSE, prediction = TRUE)

ypredxg<-predict(xgbcv,test[1:178])

OOF_prediction <- data.frame(xgbcv$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_labs + 1)
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")
head(OOF_prediction)


```
```{r}
# save the model to disk
saveRDS(xgbcv, "D:/Thesis/plots/XGBoost_final.rds")
```

```{r}
model = keras_model_sequential()
model %>%
layer_dense(units = 80, activation = layer_activation_leaky_relu(), input_shape = (178)) %>%
layer_dense(units = 4, activation = 'softmax')
summary(model)
model %>% compile(
loss = 'sparse_categorical_crossentropy',
optimizer = optimizer_adam(),
metrics = c('accuracy'))

```

```{r}

```


```{r comparando_modelos_rf_rpart}

XX  <- confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

```
```{r}
confusionMatrix(tstab)
```

```{R comparando_modelos_global}
#summary(tstab)

plot(tstab)
plot(OOF_prediction)
  ```

