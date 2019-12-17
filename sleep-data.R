setwd("/Users/vrishtijain/Desktop/CLASS_FALL/DATA_ANALYTICS/PPTS")
library(tidyr)
library(tidyverse)


preee<-read.csv("sleepdata.csv" , header=TRUE, sep = ";")


str(preee)
summary(preee)


head(preee)
attach(data)
#https://stackoverflow.com/questions/19292438/split-date-time


# top get the date and hour  in the correct format for starting time
data$hours_start <- format(as.POSIXct(strptime(preee$Start,"%Y-%m-%d %H:%M:%S",tz="")) 
                  ,format = "%H:%M:%S")

data$date_start <- format(as.POSIXct(strptime(data$Start,"%Y-%m-%d  %H:%M",tz=""))
                  ,format = "%Y-%m-%d")



# top get the date in the correct format for ending time
data$hours_end <- format(as.POSIXct(strptime(data$End,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

data$date_end <- format(as.POSIXct(strptime(data$End,"%Y-%m-%d  %H:%M",tz="")) ,format = "%Y-%m-%d")

write.csv(data, "sleepdata-preprocess.csv")
#fidning unique mood emoji
unique_emoji = unique(data$Wake.up)

print(unique_emoji)
#  :) == sentiment score 	0.557
# 	: | sentimental scoire 0.194
#   :( senti mental score  	-0.368
#https://pdfs.semanticscholar.org/eef0/4bd58707b827af8fb4e6f6792bf730bf96db.pdf
#http://kt.ijs.si/data/Emoji_sentiment_ranking/

#http://kt.ijs.si/data/Emoji_sentiment_ranking/
count1 =0
count2 =0
count3= 0
count4= 0
average = (0.555-0.368+0.194)/3
for(i in 1:length(data$Wake.up)){
  if(data$Wake.up[i] == ":)"){
    data$wake_senti[i] = 0.557
    count1= count1+1
    
  }
  else if(data$Wake.up[i] == ":("){
    data$wake_senti[i] =  -0.368
    count2 = count2+1
  }
  else  if(data$Wake.up[i] == ":|"){
    data$wake_senti[i] =   0.194
    count3 = count3+1
  }
    else {
      data$wake_senti[i] =  average 
      count4 = count4+1
    }}

print(count1)
print(count2)
print(count3)
print(count4)
write.csv(data, "sleepdata-preprocess.csv")
barplot(table(data$Wake.up))
barplot(table(data$wake_senti))

# df %>% separate(data.frame(data$Sleep.Notes), sep = ":")
# separate_rows(data$Sleep.Notes,sep=":", convert = False)
data$stressful_day = 0
data$drank_tea =0
data$drank_coffee =0
data$worked_out =0


df <- data.frame(x = data$Sleep.Notes)
df %>% separate(x, c("a", "b", "c"))
for(i in 1:length(data$Sleep.Notes))
  {
  if(grep( "Stressful day" , data$Sleep.Notes[i] ) == 1){
    data$stressful_day[i] = 1
    count1= count1+1
  }
  else if (grep("Drank tea" ,data$Sleep.Notes[i]) == 1){
    data$drank_tea[i] = 1
    count2 = count2+1
  }
  else  if(grep("Drank coffee" , data$Sleep.Notes[i]) == 1){
    data$drank_coffee[i] =  1
    count3 = count3+1
  }
  else if (grep(  "Worked out" , data$Sleep.Notes[i]) == 1) {
    data$worked_out[i] =  1 
    count4 = count4+1
  }
  else {
    
  }
}
print(count1)
print(count2)
print(count3)
print(count4)

print(data$Sleep.Notes[7])

print(grep("Drank tea" , data$Sleep.Notes[7]))
if (grep("Drank tea" , data$Sleep.Notes[7]) == 1 ){
print("aiuhdsudha")
  print(data$Sleep.Notes[5])
}
# separate_(data, col = colnames(i)[7],
#             into = c("note1", "note2", "note3", "note4"),
#             sep = ":")


# remove the percentage from the sleep quality
x <- c(preee$Sleep.quality)
preee$Sleep.quality =  as.numeric(sub("%","",x))
write.csv(data, "sleepdata-preprocess.csv")


cov(preee$Sleep.quality , preee$Heart.rate)



cov(data$Sleep.quality , data$wake_senti)
data$new_time =0 
# to convert time in bed to numeric value

# converting the time in bed to numeric value
for (i in 1: length(data$Time.in.bed)){
  x = data$Time.in.bed[i]
  data$new_time[i]=as.numeric(sub(":",".",x))
}
write.csv(pre, "sleepdata-preprocess-final.csv")
#write.csv(data, "sleepdata-preprocess.csv")

preee <- na.omit(preee) 
cov(data$Sleep.quality , data$new_time )
data = data[, -1]

mm <-lm(data$Sleep.quality~data$new_time + data$wake_senti)
summary (mm)
summary(mm)$coef 
plot(data$Sleep.quality~data$new_time )
abline(mm)




mm1 <-lm(data$Sleep.quality~data$new_time )
summary (mm1)
summary(mm1)$coef 

plot(data$Sleep.quality~data$new_time )
abline(mm1)



boxplot(data$Sleep.quality)

filter_data= subset(final_data, final_data$Sleep.quality > 25) 
sleep<-table(filter_data$Sleep.quality)
barplot(sleep, xlab= "Sleep quality")

lm1 <- lm(final_data$Sleep.quality ~ final_data$numeric_hours_end + final_data$numeric_hours_start 
             + final_data$new_time   + final_data$stressful_day  +final_data$worked_out + final_data$drank_coffee
             +final_data$drank_tea         + final_data$wake_senti)
           
summary(lm1)



