setwd("/Users/vrishtijain/Desktop/CLASS_FALL/DATA_ANALYTICS/PPTS")
library(tidyr)
library(tidyverse)
final_data<-read.csv("sleepdata.csv" , header=TRUE, sep = ";")

temp <-  read.csv("sleepdata-preprocess.csv" , header=TRUE)

hello <-final_data$hours_start

temp$hours_start = hello
temp$Heart.rate = final_data$Heart.rate

final_data = temp
summary(final_data)
str(final_data)



# top get the date and hour  in the correct format for starting time
final_data$hours_start <- format(as.POSIXct(strptime(final_data$Start,"%Y-%m-%d %H:%M:%S",tz="")) 
                           ,format = "%H:%M:%S")

final_data$date_start <- format(as.POSIXct(strptime(final_data$Start,"%Y-%m-%d  %H:%M",tz=""))
                          ,format = "%Y-%m-%d")



# top get the date in the correct format for ending time
final_data$hours_end <- format(as.POSIXct(strptime(final_data$End,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

final_data$date_end <- format(as.POSIXct(strptime(final_data$End,"%Y-%m-%d  %H:%M",tz=""))
                              ,format = "%Y-%m-%d")


average = (0.555-0.368+0.194)/3
for(i in 1:length(final_data$Wake.up)){
  if(final_data$Wake.up[i] == ":)"){
    final_data$wake_senti[i] = 0.557
    count1= count1+1
    
  }
  else if(final_data$Wake.up[i] == ":("){
    final_data$wake_senti[i] =  -0.368
    count2 = count2+1
  }
  else  if(final_data$Wake.up[i] == ":|"){
    final_data$wake_senti[i] =   0.194
    count3 = count3+1
  }
  else {
    final_data$wake_senti[i] =  average 
    count4 = count4+1
  }}


# remove the percentage from the sleep quality
x <- c(preee$Sleep.quality)
preee$Sleep.quality =  as.numeric(sub("%","",x))



# to convert time in bed to numeric value

# converting the time in bed to numeric value
for (i in 1: length(data$Time.in.bed)){
  x = data$Time.in.bed[i]
  data$new_time[i]=as.numeric(sub(":",".",x))
}



# one hot encoding for sleep notes
for(i in 1:length(final_data$Sleep.Notes)){
  
  if(final_data$Sleep.Notes[i] == "Stressful day" ){
    final_data$stressful_day[i] = 1
    print("hi")
  }
  else if(final_data$Sleep.Notes[i] == "Drank coffee" ){
    final_data$drank_coffee[i] = 1
  }
  else if(final_data$Sleep.Notes[i] == "Drank tea" ){
    final_data$drank_tea[i] = 1
  }
  else if(final_data$Sleep.Notes[i] == "Worked out" ){
    final_data$worked_out[i] = 1
  }}


for(i in 1:length(final_data$X.3)){
  
  if(final_data$X.3[i] == "Stressful day" ){
    final_data$stressful_day[i] = 1
  }
  else if(final_data$X.3[i] == "Drank coffee" ){
    final_data$drank_coffee[i] = 1
  }
  else if(final_data$X.3[i] == "Drank tea" ){
    final_data$drank_tea[i] = 1
  }
  else if(final_data$X.3[i] == "Worked out" ){
    final_data$worked_out[i] = 1
  }
}

for(i in 1:length(final_data$X.4)){
  
  if(final_data$X.4[i] == "Stressful day" ){
    final_data$stressful_day[i] = 1
  }
  else if(final_data$X.4[i] == "Drank coffee" ){
    final_data$drank_coffee[i] = 1
  }
  else if(final_data$X.4[i] == "Drank tea" ){
    final_data$drank_tea[i] = 1
  }
  else if(final_data$X.4[i] == "Worked out" ){
    final_data$worked_out[i] = 1
  }
}

for(i in 1:length(final_data$X.5)){
  
  if(final_data$X.5[i] == "Stressful day" ){
    final_data$stressful_day[i] = 1
  }
  else if(final_data$X.5[i] == "Drank coffee" ){
    final_data$drank_coffee[i] = 1
  }
  else if(final_data$X.5[i] == "Drank tea" ){
    final_data$drank_tea[i] = 1
  }
  else if(final_data$X.5[i] == "Worked out" ){
    final_data$worked_out[i] = 1
  }
}

 

 time=as.numeric(unlist(strsplit("55:99:00", ":")))
# convert the start and end hour to numeric 
 for ( i in 1:length(final_data$hours_end)){
  
   
   time = as.character( final_data$hours_end[i])
   time=as.numeric(unlist(strsplit(time, ":")))
   ans = time[1]*60+time[2]+time[3]/60
  final_data$numeric_hours_end[i]  =  ans
   
   time =  as.character( final_data$hours_start[i])
   time=as.numeric(unlist(strsplit(time, ":")))
   ans = time[1]*60+time[2]+time[3]/60
   final_data$numeric_hours_start[i]  =   ans

 }
 
 
 # for distribution graph and normal curve over it
 m<-mean(final_data$Heart.rate )
 std<-sqrt(var(final_data$Heart.rate  ))
 hist(final_data$Heart.rate , density=30, breaks=20, prob=TRUE, 
      xlab="Heart.rate  ", 
      main="normal curve over Heart.rate  ")
 curve(dnorm(x, mean=m, sd=std), 
       col="darkblue", lwd=2, add=TRUE, yaxt="n")
 
 # for gg plots
 attach(final_data)
 qq<- as.data.frame(  table(wake_senti))
 qq
 df <- data.frame(wake_senti=c(qq["wake_senti"]),
                  frequency=c(table(wake_senti)))
 df
 ggplot(data=df, aes(x=wake_senti, y=frequency)) +
   geom_bar(stat="identity",color="black", fill="lightblue")+
   geom_text(aes(label=frequency), vjust=-.3, color="black", size=4.5)+
   theme_minimal()
 
 
 boxplot(final_data$Sleep.quality)
 
mean_heart =mean(final_data$Heart.rate , na.rm = TRUE)
mean_heart

for ( i in 1:length(final_data$Heart.rate)){
  if ( is.na(final_data$Heart.rate[i])){
    print(i)
    final_data$Heart.rate[i] = mean_heart
  }
}




final_data = temp



# the linear models
 lm1 <- lm(final_data$Sleep.quality ~ final_data$numeric_hours_end + final_data$numeric_hours_start 
           + final_data$new_time   + final_data$stressful_day  +final_data$worked_out + final_data$drank_coffee
       final_data$new_time    +final_data$drank_tea    +final_data$Heart.rate     + final_data$wake_senti)
 
 summary(lm1)
 

 
 # removving the extra variable so that it would only have the required attributes
 library(dplyr)
 str(final_data)
 sleep_data = final_data
 
 
 subset_sleep = c("Sleep.quality", "wake_senti", "numeric_hours_end", "numeric_hours_start",
                  "stressful_day","worked_out", "drank_coffee", "drank_tea", "new_time", "Heart.rate")
 subset_sleep = final_data[, subset_sleep]
 subset_sleep

 
 #correlation plots
 correlations<-cor(subset_sleep)
 round(correlations, 2)
 library(corrplot)
 corrplot(correlations, type = "lower", order = "hclust", 
          tl.col = "black", tl.srt = 45)
 
 
 col<- colorRampPalette(c("blue", "white", "red"))(30)
 heatmap(x = correlations, col = col, symm = TRUE)

 corrplot(correlations, method = 'number')
 
 
 # creating class for sleep quality in order to apply Classification models
 extra_vairbale <- as.integer(as.character(subset_sleep$Sleep.quality)) 
 for (i in 1:length(extra_vairbale )) 
 { if(extra_vairbale [i] >= 0 & extra_vairbale [i] <=45)
 { subset_sleep$sq_class[i] = "low_quality" } 
   else if(extra_vairbale [i] >45 & extra_vairbale [i] <=65)
   { subset_sleep$sq_class[i] = "medium_quality" } else 
       { subset_sleep$sq_class[i] = "high_quality"}} 
 

 ## We are using validatioin set approach for resampling. Select 80% observation for training and 20% for testing.
 ## Removing sleep qualtiy nymber 
 subset_sleep = subset_sleep[, -1]

 m  = dim(subset_sleep)[1]

 val_sleep <- sample(1:m, size = round(m/4), replace = FALSE, 
                  prob = rep(1/m, m)) 
 train_learn <- subset_sleep[-val_sleep,]
 test_valid <- subset_sleep[val_sleep,]
 
 
 aa_train_target <- sub_aa[-val,20]
 aa_test_target <- sub_aa[val, 20]
 
 attach(train_learn)
 
 library("visNetwork")
 library(rpart)
 library(rpart.plot)
 # generate the decision tree model
tree1<- rpart(sq_class~., train_learn)
tree1
 rpart.plot(tree1)
 visTree(tree1)
 t_pred = predict(tree1,test_valid,type="class")
 acc_table<-table(t_pred , test_valid$sq_class)
 acc_table
 accuracy_tree<- sum(diag(acc_table))/sum(acc_table)
 accuracy_tree
 summary(tree1)
 
 # trying svm 

 svm.model_sleep <- svm(sq_class ~ ., data = train_learn, type='C-classification', gamma = 0.01)
 
 pred_svm<- predict(svm.model_sleep,test_valid, type ="class")
 tab_svm<-table(pred_svm , test_valid$sq_class)
 tab_svm
 accuracy_svm<- sum(diag(tab_svm))/sum(tab_svm)
 accuracy_svm
 
 summary(svm.model_sleep)
 






