#To clear the R environment of any predefined objects
rm(list=ls())
#To set working directory
setwd("F:/DS/edWisor/Project 2")
getwd()

#To load required libraries
library(ggplot2)    # used for ploting
library(dplyr)      # used for data manipulation and joining
library(scales)     # used for "pretty_brakes() function"
library(DMwR)       # used for KNN Imputation
library(outliers)   # used for outlier detection & modification
library(corrgram)   # used for plotting correlation amongst variables
library(corrplot)   # used for plotting correlation amongst variables
library(caret)      # used for various model training
library(lubridate)  # used for handling date format data
library(FNN)        # used for KNN modeling
library(randomForest) # used for Random Forest implementation
library(rpart)      # used for Decision Tree algorithm implementation

#To load the data
data = read.csv("day.csv",header = T, na.strings = c(""," ","NA",NA))

####################Data Exploration######################
str(data)           #"data.frame"
dim(data)             # 731 x 16

###Univariate Analysis###
#col = names(data)
#To find the unique values in each column
#for (i in col) {
#  print(i)
#  print(length(unique(data[,i])))
#}
#Data has 7 categorical variables, 8 numeric variables & one date type variable.
#Target variable is integer type in nature.

###Data Consolidation###
#Convert into Proper data types
#-->ignoring "instant" as it is just like serial number.
data = data[,-1]
#dim(data)       #731 x 15 

#_____Data type conversion_____#
catnames = c("season","yr","mnth","holiday","weekday","workingday","weathersit")   #categorical variables
for (i in catnames) {
  data[,i] = as.factor(data[,i])
}

numnames = c("temp","atemp","hum","windspeed","casual","registered","cnt")         #numerical variables
for (i in numnames) {
  data[,i] = as.numeric(data[,i])
}

data$dteday = as.Date(data$dteday)     #It changed date "02-04-11" to "2011-04-02".


str(data)

###_________________________Graphical analysis_______________________________###
#Histogram for Target variable (continuous variable)
ggplot(data, aes_string(x = data$cnt)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Count of total rental bikes") + ylab("Frequency") + ggtitle("Target Variable Histogram") +
  theme(text=element_text(size=10))

#Histogram for Independent Continuous Variables
ggplot(data, aes_string(x = data$temp)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Normalized Temperature (*C)") + ylab("Frequency") + ggtitle("IndependentVariable:Norm Temperature") +
  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$atemp)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Normalized Feeling Temperature (*C)") + ylab("Frequency") + ggtitle("IndependentVariable:Norm Feeling Temperature") +
  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$hum)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Normalized Humidity") + ylab("Frequency") + ggtitle("IndependentVariable:Humidity") +
  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$windspeed)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Normalized Windspeed") + ylab("Frequency") + ggtitle("IndependentVariable:Windspeed") +
  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$casual)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Count of Casual Users") + ylab("Frequency") + ggtitle("IndependentVariable:Casual Users") +
  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$registered)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Count of Registered Users") + ylab("Frequency") + ggtitle("IndependentVariable:Registered Users") +
  theme(text=element_text(size=10))

#Bar graph for Independent Categorical Variables
ggplot(data, aes_string(x = data$season)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Season of the year") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Seasons ") +  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$yr)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("year") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Year 2011-2012 ") +  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$mnth)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Month of the year") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Months 1-12 ") +  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$holiday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Holiday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Holiday ") +  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$weekday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Day of the week") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Week Day ") +  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$workingday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Working day") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Working day ") +  theme(text=element_text(size=10))

ggplot(data, aes_string(x = data$weathersit)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Weather") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Weather of the day ") +  theme(text=element_text(size=10))

###Bivariate Analysis
##Target Variable Vs Independent Continuous Variables: Scatter Plots
#1.cnt Vs temp
ggplot(data) + 
  geom_point(aes(data$temp, data$cnt),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Count of bike rentals") + xlab("Temperature (*C)") + ggtitle("Scatter plot: Rental Count Vs Temperature") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#2.cnt Vs atemp
ggplot(data) + 
  geom_point(aes(data$atemp, data$cnt),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Count of bike rentals") + xlab("Feeling Temperature (*C)") + ggtitle("Scatter plot: Rental Count Vs Temperature") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#3.cnt Vs hum
ggplot(data) + 
  geom_point(aes(data$hum, data$cnt),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Count of bike rentals") + xlab("Humidity") + ggtitle("Scatter plot: Rental Count Vs Humidity") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#4.cnt Vs windspeed
ggplot(data) + 
  geom_point(aes(data$windspeed, data$cnt),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Count of bike rentals") + xlab("Speed of Wind") + ggtitle("Scatter plot: Rental Count Vs Wind speed") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#5.cnt Vs Casual
ggplot(data) + 
  geom_point(aes(data$casual, data$cnt),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Count of bike rentals") + xlab("Casual Users count") + ggtitle("Scatter plot: Rental Count Vs Casual users") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#6.cnt Vs Registered
ggplot(data) + 
  geom_point(aes(data$registered, data$cnt),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Count of bike rentals") + xlab("Registered Users count") + ggtitle("Scatter plot: Rental Count Vs Registered users") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

##Target Variable Vs Independent categorical Variables: Bar plots
#7.cnt Vs Season
ggplot(data, aes_string(x = data$season, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("Season") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Seasons ") +  theme(text=element_text(size=10))

#8.cnt Vs Year
ggplot(data, aes_string(x = data$yr, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("year") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Year 2011-2012 ") +  theme(text=element_text(size=10))

#9.cnt Vs Month
ggplot(data, aes_string(x = data$mnth, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("Month") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Month 1-12 ") +  theme(text=element_text(size=10))

#10.cnt Vs Holiday
ggplot(data, aes_string(x = data$holiday, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("Holiday") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Holiday ") +  theme(text=element_text(size=10))

#11.cnt Vs Weekday
ggplot(data, aes_string(x = data$weekday, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("Day of the week") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Week day ") +  theme(text=element_text(size=10))

#12.cnt Vs Working day
ggplot(data, aes_string(x = data$workingday, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("Working day") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Working day ") +  theme(text=element_text(size=10))

#13.cnt Vs Weather
ggplot(data, aes_string(x = data$weathersit, y = data$cnt)) +
  geom_bar(stat= "identity",fill =  "violet") + theme_bw() +
  xlab("Weather of the day") + ylab('Count of bike rentals') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Count Vs Weather ") +  theme(text=element_text(size=10))

##Independent Variable Vs Independent Variable (Interdependencies):
#14.Temp Vs Season
ggplot(data) + 
  geom_point(aes(x = data$season,y = data$temp),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Temperature") + xlab("Season") + ggtitle("Scatter plot: Temperature Vs Season") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10)) 

#15.Hum Vs Season
ggplot(data) + 
  geom_point(aes(x = data$season,y = data$hum),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Humidity") + xlab("Season") + ggtitle("Scatter plot: Humidity Vs Season") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10)) 

#16.Windspeed Vs Season
ggplot(data) + 
  geom_point(aes(x = data$season,y = data$windspeed),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Windspeed") + xlab("Season") + ggtitle("Scatter plot: Windspeed Vs Season") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10)) 

#17.Month Vs Season
ggplot(data) + 
  geom_point(aes(x = data$season,y = data$mnth),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Month") + xlab("Season") + ggtitle("Scatter plot: Month Vs Season") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_discrete(breaks=pretty_breaks(n=10)) 

#17.Weekday Vs Workingday
ggplot(data) + 
  geom_point(aes(x = data$workingday,y = data$weekday),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Day of the week") + xlab("working day") + ggtitle("Scatter plot: Weekday Vs Workingday") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_discrete(breaks=pretty_breaks(n=10)) 

#18.Season Vs Weather
ggplot(data) + 
geom_point(aes(x = data$weathersit,y = data$season),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("season") + xlab("weather") + ggtitle("Scatter plot: Season Vs weather") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_discrete(breaks=pretty_breaks(n=10)) 

#19.Holiday Vs workingday
ggplot(data) + 
  geom_point(aes(x = data$workingday,y = data$holiday),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Holiday") + xlab("Working Day") + ggtitle("Scatter plot: Holiday Vs working day") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_discrete(breaks=pretty_breaks(n=10)) 

#20.Temperature Vs Weather
ggplot(data) + 
  geom_point(aes(x = data$weathersit,y = data$temp),colour = "orange", alpha= 0.4) +
  theme_bw()+ ylab("Temperature") + xlab("weather") + ggtitle("Scatter plot: Temperature Vs weather") + 
  theme(text=element_text(size=10)) + 
  scale_x_discrete(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10)) 

#And so on. The graphs are plotted and recorded in the project report.


###To extract days from "dteday" and make a new variable
data$day = day(data$dteday)
#As we already have information about the year and month, we have the whole date information & can remove the "dteday" date type variable as it may not be suitable for modeling.
data[,1] = data[,16]
data[,16] = NULL            #dim = 731 x 15

col = names(data)

##################______________Missing Value Analysis______________##################
sum(is.na(data))

#There are no missing values for this data set.

####################____________Outlier Analysis____________########################
####Box Plot distribution & outlier check####
str(data)
for(i in 1:length(numnames)){
  assign(paste0("gn",i), ggplot(aes_string(y = (numnames[i]), x = data$cnt), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "light blue",outlier.shape=18,outlier.size=3, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numnames[i],x="Bike Rental Count")+
           ggtitle(paste("Box plot for",numnames[i])))
}

#Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)
gridExtra::grid.arrange(gn5,gn6,gn7,ncol=3)

#To check number of outliers in data (ignoring categorical variables, checked earlier)
out = 0.0
for(i in numnames){
   val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
   out = out + length(val)
   print(i)
   print(length(val))
}
out #= 59. Total Outliers in the data set is 59.
#(59/731)*100 = 8.07% of data.

##To test for the best method to find missing values for this dataset
#data[12,12]   #data[12,12] = 0.304627 (actual)
#data[12,12]= NA
#By median method:
#data$windspeed[is.na(data$windspeed)]=median(data$windspeed, na.rm = T)
#data[12,12]  #data[12,12] = 0.180971 (median)

#reupload data
#data[12,12]   #data[12,12] = 0.304627 (actual)
#data[12,12]= NA
#by mean method:
#data$windspeed[is.na(data$windspeed)]=mean(data$windspeed, na.rm = T)
#data[12,12]  #data[12,12] = 0.1903299 (mean)

#reupload data
#data[12,12]   #data[12,12] = 0.304627 (actual)
#data[12,12]= NA
#By KNN imputation method:
#(KNN takes only numeric inputs)
#for (i in col) {
#  data[,i] = as.numeric(data[,i])
#}
#data= knnImputation(data, k=3)      #For k=5,7,9, the difference was even more than k=3.
#data[12,12]  #data[12,12] = 0.2324425 (KNN)
#We freeze NA imputation by MEDIAN method as it is closest to actual value.

#reupload data
#Converting outliers to NAs
#Select variables with outliers
Out_Var = c('hum','windspeed','casual')  #Variables with outliers

for(i in Out_Var){
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  data[,i][data[,i] %in% val] = NA
}
sum(is.na(data)) #To verify

data= knnImputation(data, k=3)

sum(is.na(data)) #To verify

#Confirm again if any outlier exists
out = 0.0
for(i in numnames){
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  out= out + length(val)
  print(i)
  print(length(val))
}
out #= 3. Windspeed has 2 outliers & Casual has 1 outlier.

#-->Redo 2 times the imputing using NAs by KNN imputation until 0 outliers.

write.csv(data, 'data_without Outliers.csv', row.names = F)

#To load the data
#data = read.csv("data_without Outliers.csv",header = T)

############################Feature Selection#############################
#Correlation Plot
corrgram(data, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot", font.labels = 1)
#cor(x), x must be numeric
#Convert all columns to numeric type
#for (i in col) {
#  data[,i] = as.numeric(data[,i])
#}    #NOTE: This changes all zero factor levels to numeric 1. so, "0" --> 1.
#mat = cor(data)                                                                 
#corrplot(as.matrix(mat),method= 'pie',type = "lower", tl.col = "black", tl.cex = 0.7)

#If |r|>0.8, those two variables are redundant variables.
#Output: "mnth"-"season", "temp"-"atemp" & "cnt"-"registered" are highly positively correlated.

#redo data conversion to proper types
catnames = c("season","yr","mnth","holiday","weekday","workingday","weathersit")   #categorical variables
for (i in catnames) {
  data[,i] = as.factor(data[,i])
}

numnames = c("dteday","temp","atemp","hum","windspeed","casual","registered","cnt")  #numerical variables
for (i in numnames) {
  data[,i] = as.numeric(data[,i])
}

#######Chi-square Test of Independence (within Categorical Variables)            
for(i in catnames){
  for(j in catnames){
    if(i!=j){
     print(names(data[i]))
     print(paste0(" Vs ", names(data[j])))
     print(chisq.test(table(data[,j],data[,i])))
    }
  }
}
#If p-value<0.05 (Reject Null Hypothesis) => variable A depends on variable B.
#If p-value>0.05 (Do Not Reject Null Hypothesis) => Variable A & variable B are independent of each other.
#Output: "workingday"-"holiday","weekday"-"workingday","weekday"-"holiday" & "mnth"-"season depend on each other significantly.

#######Using Random Forest Algorithm: 
data.rf=randomForest(data$cnt~.,data = data, ntree=1000, keep.forest= F, importance= T)
importance(data.rf,type = 1)
#"holiday" has the least importance.
varImpPlot(data.rf,type = 1)

#######ANOVA test (comparision of Target Vs categorical variables)
anovacat = aov(cnt ~ season + yr + mnth + holiday + workingday + weekday + weathersit , data = data)
summary(anovacat)
#If p-value<0.05 (Reject Null Hypothesis) => Population means are significantly different.
#If p-value>0.05 (Do Not Reject Null Hypothesis) => Population means are not significantly different or are same.

###################___________Feature Engineering____________######################
#From Chi-square test, we notice that "working day", "holiday" & "weekday" depend on each other and intuitively there is a logical connection within them.
#We make a new variable using this connection between the three varibles
#Denote: 1-->weekend, 2--> working day, 3--> holiday

data$day = NA
for (i in 1:nrow(data)){
  if ((data[i,7]=="0") && (data[i,5]=="0")){data[i,16] = 1}                             #weekend
  else if ((data[i,7]=="1") && (data[i,5]=="0")){data[i,16] = 2}                        #working day
  else if ((data[i,7]=="0") && (data[i,5]=="1")){data[i,16] = 3}                        #holiday
  else data[i,16] =NA 
}
sum(is.na(data$day)) #= 0, so no anomaly data case where it is working day & holiday both.

###################Dimensional Reduction######################
#Won't remove "dteday" variable as the user count is tracked on each day.
#As we added "day" new variable using "workingday" & "holiday", we can remove them both as "day" holds the information of both.
data$holiday = data$day
data$day = NULL
colnames(data)[5] = "day"
data$day = as.factor(data$day)        # New variable "day": Factor w/ 3 levels "1","2","3"
#"Season" has multicollinearity problem as well and it is related to "mnth", so we can remove it.
data= subset(data, select= -c(season,workingday,temp,casual,registered))
factor_data = subset(data, select= c(yr,mnth,day,weekday,weathersit))  #5 factor variables
num_data = subset(data, select= c(dteday,atemp,hum,windspeed,cnt))  #5 numerical variables, contains target variable
dim(data)                   # 731 obs. x 10 variables
str(data)
################################Feature Scaling##################################
#All continuous variables are already normalised in this data set.


rm(list= ls()[!(ls() %in% c('data','factor_data','num_data'))])

##############################Sampling#################################
set.seed(777)

sample.index = sample(nrow(data), 0.8*nrow(data), replace = F)   #80% data -->Train set, 20%--> Test set
train = data[sample.index,]
test = data[-sample.index,]
dim(train)    # 584 x 11
dim(test)     # 147 x 11

##################################Model Development###################################
#As the target variable is of numeric type, this is a regression problem.
######1.Decision Tree######
#Decision trees can handle both categorical and numerical variables at the same time as features.
dt=rpart(cnt~.,data = train,method= "anova")
summary(dt)

#Predict for new test cases
predict.dt=predict(dt,test[,-10])

#Error metric:
postResample(predict.dt,test[,10])
#Output:
#RMSE          Rsquared     MAE 
#1036.8218286    0.7105788  768.8217306 

#calculate MAPE
mape = function(y,yi)
  {mean(abs((y-yi)/y))*100
  }
mape.dt = mape(test[,10],predict.dt)     #30.79%

library(mltools)
rmsle(predict.dt,test[,10])    #0.3665

#######2.Random Forest Algorithm#######
rf = randomForest(cnt~., train, importance = TRUE, ntree = 500)
summary(rf)
#Predict for test case:
predict.rf <- data.frame(predict(rf, subset(test, select = -c(cnt))))
#Error metric:
postResample(predict.rf,test[,10])
#Output:
#RMSE          Rsquared        MAE 
#778.4675527   0.8507608 576.6110231 

mape.rf = mape(test[,10],predict.rf$predict.rf..subset.test..select....c.cnt...)   # 24.9%

########3.Multiple Linear Regression########

#creating dummy variables for categorical data
library(dummies)
factor_new = dummy.data.frame(factor_data, sep = ".")   #731 x 27

#sampling#
df = cbind(factor_new, num_data)
#for (i in 1:ncol(df)) {
#  df[,i] = as.numeric(df[,i])
#}
str(df)           # 731 X 32

set.seed(123)
train_index = sample(1:nrow(df), 0.8*nrow(df))
train.df = df[train_index,]         #584 x 32
test.df = df[-train_index,]         #147 x 32

#Check Multicollinearity 
library(usdm)
vif(df[,-32])
vifcor(df[,-32], th = 0.8)
#Output:
#3 variables from the 31 input variables have collinearity problem: yr.1, weathersit.2, day.2
#removing multicollinear variables and redo check:
df = subset(df, select= -c(yr.1, weathersit.2, day.2))
train.df = subset(train.df, select= -c(yr.1, weathersit.2, day.2))  #584 x 29
test.df = subset(test.df, select= -c(yr.1, weathersit.2, day.2))    #147 x 29
dim(df)  #731 x 29
#Recheck VIFCORR: No variable from the 29 input variables has collinearity problem.

#run regression model
lr = lm(cnt~., data = train.df)
#summary of the model
summary(lr)

#Predict for test case:
predict.lr= predict(lr, test.df[,-29])
#Error metric:
postResample(predict.lr,test.df[,29])
#Output:
#RMSE          Rsquared         MAE 
#800.2783046   0.8303233 581.4298996 

mape.lr = mape(test.df[,29],predict.lr) #17.5%

##############4.KNN Implementation##############
#To check for best k value:
model <- train(cnt~., data = train, method = "knn",
               trControl = trainControl("cv", number = 10),
               tuneLength = 15)
model$bestTune
#k = 3 , 9
plot(model)

#K=3:
predict.knn = knn.reg(train = train.df[,-29],test = test.df[,-29],train.df$cnt, k= 3)
print(predict.knn)

#Error metric:
postResample(predict.knn$pred,test.df[,29])
#Output:
#RMSE            Rsquared         MAE 
#1392.7631351    0.4544424 1110.0045351

mape.knn = mape(test.df[,29],predict.knn$pred)  #38.98%

#K=5:
#predict.knn = knn.reg(train = train.df[,-29],test = test.df[,-29],train.df$cnt, k= 5)
#print(predict.knn)
#Error metric:
#mape(test.df[,29],predict.knn$pred)
#Output:
#mape
#45.26592 %
#postResample(predict.knn$pred,test.df[,29])
#RMSE            Rsquared         MAE 
#1450.9419952    0.4484269 1169.3782313

#K=7:
#predict.knn = knn.reg(train = train.df[,-29],test = test.df[,-29],train.df$cnt, k= 7)
#print(predict.knn)
#Error metric:
#mape(test.df[,29],predict.knn$pred)
#Output:
#mape
#47.63637 %
#postResample(predict.knn$pred,test.df[,29])
#RMSE            Rsquared         MAE 
#1456.0507716    0.4983456 1171.8736638 
######And so on, done upto k = 11.

#A new dataframe to store results
algorithm <- c('Decision Tree','Random Forest','Linear Regression','KNN')
MAPE_val <- c(mape.dt,mape.rf,mape.lr,mape.knn)
results <- data.frame(algorithm, MAPE_val)
print(results)
barplot(results$MAPE_val, width = 1, names.arg = results$algorithm,
        ylab="MAPE value", xlab = "Algorithm",col='pink')

##Thus, we find the "Multiple Linear Regression Algorithm" gives us the best result with the least MAPE for this dataset.
