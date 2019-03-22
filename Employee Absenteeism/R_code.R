#To clear the R environment of any predefined objects
rm(list=ls())
#To set working directory
setwd("F:/DS/edWisor/Project 1")
getwd()

#To load required libraries
library(xlsx)
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrgram)   
library(corrplot)   # used for making correlation plot
library(DMwR)
library(randomForest)
library(class)
library(FNN)
library(scales)


#To load the data
data = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1, header = T)
data[data == " " | data == "" | data == "NA"] = NA

############Data Exploration#############
str(data)
dim(data)
#Data has 9 categorical variables & 12 numeric variables
#Target variable is continuous in nature

###Univariate Analysis
#Since all data are in numeric type, we don't need to convert it for data consolidation
#Histogram for Target variable (continuous variable)
ggplot(data, aes_string(x = data$Absenteeism.time.in.hours)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Absenteeism time (hours)") + ylab("Frequency") + ggtitle("Target Variable Histogram") +
  theme(text=element_text(size=10))

#Histogram for Independent Continuous Variables
ggplot(data, aes_string(x = data$Transportation.expense)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Transport cost") + ylab("Frequency") + ggtitle("IndependentVariable:Transportation Cost") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Distance.from.Residence.to.Work)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Distance from Residence to work (km)") + ylab("Frequency") + ggtitle("Independent Variable:Distance") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Service.time)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Service time (hours)") + ylab("Frequency") + ggtitle("Independent Variable: Service time") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Age)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Age (years)") + ylab("Frequency") + ggtitle("Independent Variable: Employee Age") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Work.load.Average.day.)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Average Work load per day") + ylab("Frequency") + ggtitle("Independent Variable: Work load") +
  theme(text=element_text(size=7))
ggplot(data, aes_string(x = data$Hit.target)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Hit target") + ylab("Frequency") + ggtitle("Independent Variable: Hit target") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Son)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Number of children") + ylab("Frequency") + ggtitle("Independent Variable:Employee's children") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Weight)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Weight (kg)") + ylab("Frequency") + ggtitle("Independent Variable:Employee's weight") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Height)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Height (cm)") + ylab("Frequency") + ggtitle("Independent Variable:Employee's height") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Body.mass.index)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Body Mass Index") + ylab("Frequency") + ggtitle("Independent Variable:Employee's BMI") +
  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Pet)) + 
  geom_histogram(fill="blue", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Number of pets") + ylab("Frequency") + ggtitle("Independent Variable:Employee's pets") +
  theme(text=element_text(size=10))

#Bar graph for Independent Categorical Variables
ggplot(data, aes_string(x = data$ID)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("ID") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Employee ID ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Reason.for.absence)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Reason of Absence") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Reason of Absence ") +  theme(text=element_text(size=9))
ggplot(data, aes_string(x = data$Month.of.absence)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Month of Absence") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Month ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Day.of.the.week)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Day of the week") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Day ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Seasons)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Seasons") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Seasons ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Disciplinary.failure)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Disciplinary Failure") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Disciplinary Failure ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Social.drinker)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Social Drinker") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: drinking habits ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Social.smoker)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Social smoker") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Smoking habits ") +  theme(text=element_text(size=10))
ggplot(data, aes_string(x = data$Education)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Education") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Independent Variable: Education ") +  theme(text=element_text(size=10))

###Bivariate Analysis
##Target Variable Vs Independent Categorical Variables
length(unique(data$ID))
#1.Absenteeism time Vs ID
ggplot(data) + 
  geom_point(aes(data$ID, data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("ID") + ggtitle("Scatter plot: Absenteeism Vs ID") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#2.Absenteeism time Vs Reason of Absence
ggplot(data) + 
  geom_point(aes(data$Reason.for.absence, data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Reason") + ggtitle("Scatter plot: Absenteeism Vs Reason") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) 

#3.Absenteeism time Vs Month of Absence
ggplot(data) + 
  geom_point(aes(data$Month.of.absence, data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Month of absence") + ggtitle("Scatter plot: Absenteeism Vs Month") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))  

#4.Absenteeism time Vs Month of Absence
ggplot(data) + 
  geom_point(aes(data$Seasons , data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Seasons") + ggtitle("Scatter plot: Absenteeism Vs Seasons") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

#5.Absenteeism time Vs Transportation Expense
ggplot(data) + 
  geom_point(aes(data$Transportation.expense , data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Transportation Expense") + ggtitle("Scatter plot: Absenteeism Vs Transportation") + 
  theme(text=element_text(size=9)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

#6.Absenteeism time Vs Distance from residence to work
ggplot(data) + 
  geom_point(aes(data$Distance.from.Residence.to.Work , data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Distance from residence to work (km)") + ggtitle("Scatter plot: Absenteeism Vs distance") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

#7.Absenteeism time Vs Service time
ggplot(data) + 
  geom_point(aes(data$Service.time , data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Service time(hrs)") + ggtitle("Scatter plot: Absenteeism Vs Service time") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

#8.Absenteeism time Vs Age
ggplot(data) + 
  geom_point(aes(data$Age , data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("age (years)") + ggtitle("Scatter plot: Absenteeism Vs Age") + 
  theme(text=element_text(size=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

#9.Absenteeism time Vs workload
ggplot(data) + 
  geom_point(aes(data$Work.load.Average.day. , data$Absenteeism.time.in.hours),colour = "violet", alpha = 0.4) +
  theme_bw()+ ylab("Absenteeism time (hours)") + xlab("Work load average per day") + ggtitle("Scatter plot: Absenteeism Vs workload") + 
  theme(text=element_text(size=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

#And so on. The graphs are plotted and recorded in the project report.

##########Fixing Data Anomalies##########
#Treating the 0's in 'Reason of absences' & 'Month of absence' as missing values
zero_index = which(data$Reason.for.absence == 0)
for(i in zero_index){
  data$Reason.for.absence[i]= NA
}
zero_index = which(data$Month.of.absence == 0)
for(i in zero_index){
  data$Month.of.absence[i]= NA
}

##################Missing Value Analysis##################
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$NumberOfMissingValues = missing_val$Missing_percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1,3)]
write.csv(missing_val, "Missing_percentage.csv", row.names = T)

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw() + theme(text=element_text(size=8))

##To test for the best method to find missing values for this dataset
#data[6,6]=179 (actual)
data[6,6]=NA
#By median method:
data$Transportation.expense[is.na(data$Transportation.expense)]=median(data$Transportation.expense, na.rm = T)
data[6,6]
#data[6,6]= 225 (median)
#reupload data again
rm(list = ls())
data = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1, header = T)
data[data == " " | data == "" | data == "NA"] = NA
zero_index = which(data$Reason.for.absence == 0)
for(i in zero_index){
  data$Reason.for.absence[i]= NA
}
zero_index = which(data$Month.of.absence == 0)
for(i in zero_index){
  data$Month.of.absence[i]= NA
}
#By mean method:
data[6,6]=NA 
data$Transportation.expense[is.na(data$Transportation.expense)]=mean(data$Transportation.expense, na.rm = T)
data[6,6]
#data[6,6]= 221.0929 (mean)
#reupload data again
rm(list = ls())
data = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1, header = T)
data[data == " " | data == "" | data == "NA"] = NA
zero_index = which(data$Reason.for.absence == 0)
for(i in zero_index){
  data$Reason.for.absence[i]= NA
}
zero_index = which(data$Month.of.absence == 0)
for(i in zero_index){
  data$Month.of.absence[i]= NA
}
#By KNN Imputation:
data[6,6]=NA
data=knnImputation(data, k=5)
data[6,6]
#data[6,6]= 179 (KNN), which is the closest to 179 and hence, we freeze this method.

#reload the data & replace the missing values
rm(list = ls())
data = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1, header = T)
data[data == " " | data == "" | data == "NA"] = NA
zero_index = which(data$Reason.for.absence == 0)
for(i in zero_index){
  data$Reason.for.absence[i]= NA
}
zero_index = which(data$Month.of.absence == 0)
for(i in zero_index){
  data$Month.of.absence[i]= NA
}
data = knnImputation(data, k=5) #KNN
sum(is.na(data)) #To verify

write.csv(data, 'data_Missing.csv', row.names = F)

####################Outlier Analysis########################
####Box Plot distribution & outlier check####
#str(data)
#since all the variables are numeric data type, we don't need to change data type here

cnames = colnames(data)

for(i in 1:length(cnames)){
           assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = data$Absenteeism.time.in.hours), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "light blue",outlier.shape=18,outlier.size=3, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism time(hours)")+
           ggtitle(paste("Box plot for",cnames[i])))
  }

#Plotting plots together
gridExtra::grid.arrange(gn2,gn3,gn4,gn5,ncol=4)
gridExtra::grid.arrange(gn6,gn7,gn8,gn9,ncol=4)
gridExtra::grid.arrange(gn10,gn11,gn18,gn19, gn20,ncol=5)

#Replace all outliers with NA and impute using KNN:

#for(i in cnames){
#   val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
#   print(length(val))
#   data[,i][data[,i] %in% val] = NA
#}
#sum(is.na(data)) #To verify
#No. of NAs(outliers in whole dataset)= 501, which is high.


OutCol = colnames(data)
#We don't want some categorical columns to manipulated for outliers, like ID (Employee's ID is unique and can't be an outlier).
#also, Target variable shouldn't be manipulated much.
OutCol = OutCol[c(-1,-2,-3,-4,-5,-12,-13,-15,-16,-21)]   #OutCol contains only continuous variables now
for(i in OutCol){
   val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
   print(length(val))
   data[,i][data[,i] %in% val] = NA
}
sum(is.na(data)) #To verify
#No. of NAs(outliers in numeric variables)= 231 (without categorical variables)
#highest no. of outliers found in "height" column, i.e. 119 NA in that column now
#119/740*100 = 16% > 10%, the part of the data to be manipulated.
#We fill in the NAs using KNN imputation.
data = knnImputation(data, k=5)
sum(is.na(data)) #to verify

write.csv(data, 'data_without Outliers.csv', row.names = F)

############################Feature Selection############################
#Correlation Plot
corrgram(data, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot", font.labels = 1)
mat = cor(data)
corrplot(as.matrix(mat),method= 'pie',type = "lower", tl.col = "black", tl.cex = 0.7)
#If |r|>0.8, those two variables are redundant variables.
#Output: "Weight"&"Body.Mass.Index" are highly positively correlated.

#Chi-square Test of Independence
n = c(1,2,3,4,5,12,13,15,16)              #indices of the categorical variables
for(i in n){
  print(names(data[i]))
  print(chisq.test(table(data$Absenteeism.time.in.hours,data[,i])))
 }
#If p-value<0.05 (Reject Null Hypothesis) => Target variable depends on the independent variable.
#If p-value>0.05 (Do Not Reject Null Hypothesis) =>Target variable & independent variable are independent of each other.

#Using Random Forest Algorithm:
data.rf=randomForest(data$Absenteeism.time.in.hours~.,data = data, ntree=1000, keep.forest= F, importance= T)
importance(data.rf,type = 1)

###################Dimensional Reduction######################
#From Correlation Plot --> We can drop "Weight" or "Body.Mass.Index" column.
#From Chi square test --> We drop "Education" & "Social.smoker"
data= subset(data, select= -c(Body.mass.index,Education,Social.smoker))

if("Body.mass.index" %in% OutCol) OutCol = OutCol[ - which(OutCol == "Body.mass.index")]
if("Education" %in% OutCol) OutCol = OutCol[ - which(OutCol == "Education")]
if("Social.smoker" %in% OutCol) OutCol = OutCol[ - which(OutCol == "Social.smoker")]

################################Feature Scaling##################################
#Only for continuous variables
#Normality check
qqnorm(data$Absenteeism.time.in.hours)
qqnorm(data$Transportation.expense)
qqnorm(data$Distance.from.Residence.to.Work)
qqnorm(data$Service.time)
qqnorm(data$Height)
qqnorm(data$Weight)
#None of the continuous variables have a normal distribution.
hist(data$Age)
hist(data$Work.load.Average.day.)
#We go for "Normalization", instead of standardization
data1 = data
for(i in OutCol){
  print(i)
  data[,i] = (data[,i] - min(data[,i]))/(max(data[,i]) - min(data[,i]))
}

##############################Sampling#################################
set.seed(1234)
sample.index = sample(nrow(data), 0.8*nrow(data), replace = F)   #80% data -->Train set, 20%--> Test set

##################################Model Development###################################
rm(list= ls()[!(ls() %in% c('data','sample.index','data1'))])
train = data[sample.index,]
test = data[-sample.index,]
dim(train)
dim(test)

######1.Decision Tree######
library(rpart)
dt=rpart(Absenteeism.time.in.hours~.,data = train,method= "anova")
summary(dt)

#Predict for new test cases
predict.dt=predict(dt,test[,-18])

#Error metric:
regr.eval(test[,18], predict.dt, stats = c("mae","mse","rmse","mape"))
#Output:
#mae        mse       rmse       mape
#5.753875 180.407076  13.431570   Inf
postResample(predict.dt,test[,18])
#RMSE   Rsquared        MAE 
#13.4315701  0.1213995  5.7538750

#MAPE is Inf as some actual values of target variable is 0 hours.
#MAPE is not usually used as a measure in case of time-series Analysis.
rms.dt = RMSE(predict.dt, test[,18], na.rm = F)


#######2.Random Forest Algorithm#######
rf = randomForest(Absenteeism.time.in.hours~., train, importance = TRUE, ntree = 500)
summary(rf)
#Predict for test case:
predict.rf <- data.frame(predict(rf, subset(test, select = -c(Absenteeism.time.in.hours))))
#Error metric:
regr.eval(test[,18], predict.rf, stats = c("mae","mse","rmse","mape"))
#Output:
#mae        mse         rmse       mape 
#5.295667 153.418298  12.386214     Inf
postResample(predict.rf,test[,18])
#RMSE   Rsquared        MAE 
#12.3862140  0.2561696  5.2956666

#MAPE is Inf as some actual values of target variable is 0 hours.
#Errors did decrease.
rms.rf = RMSE(predict.rf, test[,18], na.rm = F)


########3.Multiple Linear Regression########
#Check Multicollinearity 
library(usdm)
vif.data= vif(data[,-18])
vifcor(data[,-18], th = 0.8)
#Output:
#No variable from the 17 input variables has collinearity problem. 

#To run regression model
lr = lm(Absenteeism.time.in.hours~., data = train)
#summary of the model
summary(lr)

#Predict for test case:
predict.lr=predict(lr, test[,-18])
#Error metric:
regr.eval(test[,18], predict.lr, stats = c("mae","mse","rmse","mape"))
#Output:
#mae        mse       rmse       mape
#6.527533 188.009158  13.711643   Inf
postResample(predict.lr,test[,18])
#RMSE    Rsquared         MAE 
#13.71164317  0.08126101  6.52753308 

#Random Forest Algorithm got better result than Linear Regression.
rms.lr = RMSE(predict.lr, test[,18], na.rm = F)


##############4.KNN Implementation##############
#Predict for test data:
#K=5:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 5)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae        mse        rmse       mape
#6.712344 199.341057  14.118819    Inf

#K=7:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 7)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse        rmse       mape 
#6.418315 196.195775  14.006990   Inf 

#K=9:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 9)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse        rmse       mape 
#6.167826 187.593597  13.696481   Inf

#K=11:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 11)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse        rmse       mape 
#6.136897 192.691006  13.881319   Inf

#K=13:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 13)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse       rmse        mape  
#6.039239 192.793157  13.884998   Inf

#K=15:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 15)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse        rmse        mape 
#6.081862 195.226633  13.972352   Inf

#K=17:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 17)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse        rmse       mape
#5.944805 194.947266  13.962352   Inf 

#K=19:
predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 19)
print(predict.knn)
#Error metric:
regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#Output:
#mae       mse        rmse       mape
#5.853058 193.726139  13.918554   Inf
postResample(predict.knn$pred,test[,18])
#RMSE          Rsquared         MAE 
#13.91855378  0.05935665  5.85305761

#K=21:
#predict.knn = knn.reg(train = train[,-18],test = test[,-18],train$Absenteeism.time.in.hours, k= 21)
#regr.eval(test[,18], predict.knn$pred, stats = c("mae","mse","rmse","mape"))
#mae       mse        rmse       mape 
#5.855254 194.307576  13.939425   Inf

#To check for best k value:
model <- train(Absenteeism.time.in.hours~., data = train, method = "knn",
               trControl = trainControl("cv", number = 10),
               tuneLength = 20)
model$bestTune
plot(model)
rms.knn = RMSE(predict.knn$pred, test[,18], na.rm = F)

#A new dataframe to store results
algorithm <- c('Decision Tree','Random Forest','Linear Regression','KNN')
RMSE_val <- c(rms.dt,rms.rf,rms.lr,rms.knn)
results <- data.frame(algorithm, RMSE_val)
print(results)
barplot(results$RMSE_val, width = 1, names.arg = results$algorithm,
         ylab="RMSE value", xlab = "Algorithm",col='pink')
