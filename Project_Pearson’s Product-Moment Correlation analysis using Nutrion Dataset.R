# EDA (EXPLORATORY DATA ANALYSIS) EDA is nothing but Data exploration technique to understand the various aspects of data . 
# EDA Basically consists of 3 steps :-
#       1>- understand the data 
#        2>- clean the data 
#       3>- Analysis the relationship between the variables
# Step 1. importing the data sets
df <- read.csv('C:\\Users\\91620\\Documents\\dav\\starbucks-menu-nutrition-drinks.csv')
df
typeof(df)
head(df,10)
NCOL(df)
NROW(df)
tail(df,10)
summary(df)
length(unique(df$X))
length(unique(df$Calories))
length(unique(df$Carb...g.))
length(unique(df$Fiber..g.))
length(unique(df$Protein))
length(unique(df$Sodium))
is.null(df)# NO null values present in Data but missing values are present in Data .
# this section of data doesn't have any numerical attributes .
# so, I am reading the another part of data which is the part of the same data set.
data <-  read.csv('C:\\Users\\91620\\Documents\\dav\\starbucks_drinkMenu_expanded.csv')
nrow(data)  
head(data,10)
tail(data,10)

#data4 <-  read.csv2('C:\\Users\\91620\\Documents\\dav\\starbucks-menu-nutrition-food.csv')
#mydata = read.csv("C:\\Users\\91620\\Documents\\dav\\starbucks-menu-nutrition-food.csv", quote = "\"", skipNul = TRUE)
mydata  
head(mydata,10)
ncol(mydata)
nrow(mydata)
summary(data)
nunique(data) # finding unique values
unique(mydata)# finding unique values
# CLEANING THE DATA
is.null(data)# this function will return False if any null or missing values prsent in data otherwise it will return True .
is.null(mydata) # In both Data there is no null value present .so,we don't have to remove any rows or columns or put any value in place of null .
# checking for evaluation that any column is redundant or not
#mean(data$Calories)
head(mydata,20)# here ,we have to find the correlation between bevarages and carb .and there is no missing values (a lot of).so,there is no need of removing any attributes .
# Mean, median and mode are the measure of central tendency of data (either grouped or ungrouped).
#MEASURE OF CENTRAL TENDENCY
# generating function for mean .
add<-0# Initializing a variable
count<-0
N<-242 # LENGTH OF EVERY COLUMN
#X<-c(data$Calories)
meanfunction<-function(X)
{  
  
  for (i in 1:N)
  {
    add=(add+X[i])
    count=count+1
  }
  print(count)
  print(add)
  Mean=(add/242)# gives the  mean of any attributes(numerical)
  print(Mean)
  
  }
X<-c(data$Calories)
# assigning  Variables  to the whole column 
# calling function with column names for both data set
meanfunction(X)
summary(data)
X2<-data$Trans.Fat..g.
meanfunction(X2)
X3<-data$Saturated.Fat..g.
meanfunction(X3)
X4<-data$Sodium..mg.
meanfunction(X4)
X5<-data$Total.Carbohydrates..g.
meanfunction(X5)
X6<-data$Cholesterol..mg.
meanfunction(X6)
X7<-data$Dietary.Fibre..g.
meanfunction(X7)
X8<-data$Sugars..g.
meanfunction(X8)
X9<-data$Protein..g.
meanfunction(X9)
X10<-mydata$Calories
X_11<-data$Caffeine..mg.
meanfunction(X_11)
mean(mydata$Carb...g.)

#v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
#uniqv <- unique(v)
#uniqv
#match(v,uniqv)
#z<-tabulate(match(v,uniqv))
#z
#which.max(z)
#uniqv[1]
# creating the function for finding the mode
findmode <- function(X){
  uniqv <- unique(X) 
  uniqv[which.max(tabulate(match(X, uniqv)))]
}
result <- findmode(data$Beverage_category)
print(result) # printing the mode for Beverage_category attributes
result_2 <-findmode(data$Beverage)
print(result_2)
#summary(data)
result_3 <- findmode(data$Calories)
print(result_3)
data[4]
result_4<-findmode(data$Beverage_prep)
print(result_4)
result_5 <- findmode(data$Total.Fat..g.)
print(result_5)
result_6 <- findmode(data$Trans.Fat..g.)
print(result_6)
result_7<-findmode(data$Saturated.Fat..g.)
print(result_7)
result_8 <-findmode(data$Sodium..mg.)
print(result_8)
result_9 <- findmode(data$Total.Carbohydrates..g.)
print(result_9)
result_10 <-findmode(data$Cholesterol..mg.)
print(result_10)
result_11<-findmode(data$Dietary.Fibre..g.)
print(result_11)
result_12<-findmode(data$Sugars..g.)
print(result_12)
result_13<-findmode(data$Protein..g.)
print(result_13)
result_14<-findmode(data$Vitamin.A....DV.)
print(result_14)
result_15<-findmode(data$Vitamin.C....DV.)
print(result_15)
result_16<-findmode(data$Calcium....DV.)
print(result_16)
result_17<-findmode(data$Iron....DV.)
print(result_17)
result_18<-findmode(data$Caffeine..mg.)
print(result_18)
colnames(data)
#  MODE IS THAT VALUE OF A ATTRIBUTES WHICH HAVE MAXIMUM FREQUENCY
# MEDIAN OF THE DATA
# MEDIAN is the middle value of the data set. It splits the data into two halves. 
# If the number of elements in the data set is odd then the center element is median and if it is even then the median would be the average of two central elements.
# for finding the median i have to check the size of the elements of dataset i.e dataset is odd or even in size .
nrow(data)
is.null(data)
# Here  In this dataset the number of elements present in every attributes column is 242 i.e even .
#So,our Median is  median would be the average of two central elements.
# defining a median function
X1<-order(data$Calories,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X2<-c(data$Calories)
x_3=X1[N/2]
x_4=X1[(N/2)+1]
#med= median
print(x_3)
print(x_4)
med=(X2[x_3]+X2[x_4])/2
paste("Median of calories attribute is ", med)
X3<-order(data$Trans.Fat..g.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X4<-c(data$Trans.Fat..g.)
x_5=X3[N/2]
x_6=X3[(N/2)+1]
#med= median
print(x_5)
print(x_6)
med_1=(X4[x_5]+X4[x_6])/2
paste("Median of Trans.Fat..g.attribute is", med_1)#summary(data)  
X5<-order(data$Saturated.Fat..g.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X6<-c(data$Saturated.Fat..g.)
x_7=X5[N/2]
x_8=X5[(N/2)+1]
#med= median
print(x_7)
print(x_8)
med_2=(X6[x_7]+X6[x_8])/2
paste("Median of Saturated.fat..g. attribute is ", med_2)
#summary((data))
X7<-order(data$Sodium..mg.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X8<-c(data$Sodium..mg.)
x_9=X7[N/2]
x_10=X7[(N/2)+1]
#med= median
print(x_9)
print(x_10)
med_3=(X8[x_9]+X8[x_10])/2
paste("Median of Sodium..mg. attribute is ", med_3)
#summary(data)
X9<-order(data$Total.Carbohydrates..g.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X10<-c(data$Total.Carbohydrates..g.)
x_11=X9[N/2]
x_12=X9[(N/2)+1]
#med= median
print(x_11)
print(x_12)
med_4=(X10[x_11]+X10[x_12])/2
paste("Median of Total.carbohydrates..g. attribute is ", med_4)
#summary(data)
X11<-order(data$Cholesterol..mg.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X12<-c(data$Cholesterol..mg.)
x_13=X11[N/2]
x_14=X11[(N/2)+1]
#med= median
print(x_13)
print(x_14)
med_5=(X12[x_13]+X12[x_14])/2
paste("Median of Cholesterol..mg. attribute is ", med_5)
#summary(data) 
X13<-order(data$Dietary.Fibre..g.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X14<-c(data$Dietary.Fibre..g.)
x_15=X13[N/2]
x_16=X13[(N/2)+1]
#med= median
print(x_15)
print(x_16)
med_6=(X14[x_15]+X14[x_16])/2
paste("Median of Dietary.Fibre..g. attribute is ", med_6)
#summary(data) 
X15<-order(data$Sugars..g.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X16<-c(data$Sugars..g.)
x_17=X15[N/2]
x_18=X15[(N/2)+1]
#med= median
print(x_17)
print(x_18)
med_7=(X16[x_17]+X16[x_18])/2
paste("Median of Sugars..g. attribute is ", med_7)
#sumary(data)
X17<-order(data$Protein..g.,decreasing = FALSE)# Here , for the calories data column the values are repeated more than once .so,data column is sorted .
# It will give the index of  the sorted values .
X18<-c(data$Protein..g.)
x_19=X17[N/2]
x_20=X17[(N/2)+1]
#med= median
print(x_19)
print(x_20)
med_8=(X18[x_19]+X18[x_20])/2
paste("Median of Protein.g. attribute is ", med_8)
# summary(data)
# For rest of the attribute We can't Find median or mean for that because those are belongs to the non numeric categories .
# so,According to the NIOR topology we Can't Find mean and Median for them But Mode Can be found for those .  
#head(data,10)
# print(summary(data))
#  measures of dispersion.
# quantile deviation
#25 percent of data will lie below Q1, 50 percent of data below Q2 and 75 percent below Q3.
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3=d_1[a_1[m_2]]+.25*((d_1[a_1[f_1]])-(d_1[a_1[t_1]]))# 75% of data will lie below Q3 .
print(Q3)
#Here, Q2 is called the Median.  
# Quartiles are obtained in almost the same way as median.

Z=(N+1)/4
print(Z)
floor(Z)
M=floor(Z)
print(M)
#Q1=(N+1)/4 th term 
#Q2=Median
#Q3=3*(N+1)/4th term .
d_1<-c(data$Calories)
a_1<-order(data$Calories,decreasing = FALSE)
print(a_1[M])
Q1=print(d_1[61])# Q1
z_1=((N+1)*2)/4
print(z_1)
M_1=floor(z_1)
print(M_1)
t=M_1-1
f=M_1+1
Q2=d_1[a_1[M_1]]+.5*((d_1[a_1[f]])-(d_1[a_1[t]]))# median
print(Q2)
z_2=((N+1)*3)/4
print(z_2)

#Quartile deviation is a statistic that measures the deviation in the middle of the data.
#Quartile deviation (QD)is also referred to as the semi interquartile range 
#And QD is half of the difference between the third quartile and the first quartile value.
# The formula for quartile deviation of the data is Q.D = (Q3 - Q1)/2.
QD=(Q3-Q1)/2
print(QD)
#Interquartile range (IQR) is a measure of dispersion that encompasses the middle half of the data by taking the difference between the data values positioned at the 25th and 75th percentiles. 
#The IQR accentuates the central range of the data rather than the maximum and minimum values.
IQR=Q3-Q1
print(IQR)# INTER QUARTILE RANGE
#summary(data)
#VARIANCE AND STANDARD DEVIATION
#A variance is the average of the squared differences from the mean.
#Standard deviation is a statistical measurement that looks at how far a group of numbers is from the mean. 
#Standard deviation measures how far apart numbers are in a data set.
V=var(d_1)
print(V)
sd=sqrt(V)# standard deviation of calories 
print(sd)
summary(data)
d_2<-c(data$Trans.Fat..g.)
a_2<-order(data$Trans.Fat..g.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_2[M])
Q1_T=(d_2[a_2[M]])+.75*((d_2[a_2[f_1]])-(d_2[a_2[t_1]]))#Q1 OF Trans.Fat..g.
print(Q1_T)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_T=d_2[a_2[m_2]]+.25*((d_2[a_2[f_1]])-(d_2[a_2[t_1]]))# 75% of data will lie below Q3 .
print(Q3_T)
QD_T=(Q3_T-Q1_T)/2# QUANTILE DEVIATION OF Trans.Fat..g.
QD_T
IQR_T=(Q3_T-Q1_T)
IQR_T
V_1=var(d_2)
print(V_1)# VARIANCE AND SD OF  Trans.Fat..g 
SD_1=sqrt(V_1)
print(SD_1)

summary(data)
d_3<-c(data$Saturated.Fat..g.)
a_3<-order(data$Saturated.Fat..g.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_3[M])
Q1_S=(d_3[a_3[M]])+.75*((d_3[a_3[f_1]])-(d_3[a_3[t_1]]))#Q1 OF Saturated.Fat..g.
print(Q1_S)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_S=d_3[a_3[m_2]]+.25*((d_3[a_3[f_1]])-(d_3[a_3[t_1]]))# 75% of data will lie below Q3 .
print(Q3_S)
QD_S=(Q3_S-Q1_S)/2# QUANTILE DEVIATION OF Saturated.Fat..g.
QD_S
IQR_S=(Q3_S-Q1_S)
IQR_S
V_2=var(d_3)
print(V_2)
SD_2=sqrt(V_2)
print(SD_2)#VARIANCE AND SD OF  Trans.Fat..g 

d_4<-c(data$Sodium..mg.)
a_4<-order(data$Sodium..mg.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_4[M])
Q1_So=(d_4[a_4[M]])+.75*((d_4[a_4[f_1]])-(d_4[a_4[t_1]]))#Q1 OF Sodium..mg.
print(Q1_So)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_So=d_4[a_4[m_2]]+.25*((d_4[a_4[f_1]])-(d_4[a_4[t_1]]))# 75% of data will lie below Q3 .
print(Q3_So)
QD_So=(Q3_So-Q1_So)/2# QUANTILE DEVIATION OF Sodium..mg.
QD_So
IQR_So=(Q3_So-Q1_So)
IQR_So
V_3=var(d_4)
print(V_3)
SD_3=sqrt(V_3)#VARIANCE AND SD OF sodium..mg.
print(SD_3)
#summary(data)
d_5<-c(data$Total.Carbohydrates..g.)
a_5<-order(data$Total.Carbohydrates..g.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_5[M])
Q1_To=(d_5[a_5[M]])+.75*((d_5[a_5[f_1]])-(d_5[a_5[t_1]]))#Q1 OF 
print(Q1_To)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_To=d_5[a_5[m_2]]+.25*((d_5[a_5[f_1]])-(d_5[a_5[t_1]]))# 75% of data will lie below Q3 .
print(Q3_To)
QD_To=(Q3_To-Q1_To)/2# QUANTILE DEVIATION OF Total.Carbohydrates..g.
QD_To
IQR_To=(Q3_To-Q1_To)
IQR_To
V_4=var(d_5)
print(V_4)
SD_4=sqrt(V_4)# VARIANCE AND SD OF Total.Carbohydrates..g.
print((SD_4))

#summary(data)
d_6<-c(data$Cholesterol..mg.)
a_6<-order(data$Cholesterol..mg.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_6[M])
Q1_C=(d_6[a_6[M]])+.75*(((d_6[a_6[f_1]])-(d_6[a_6[t_1]])))#Q1 OF Cholesterol..mg.
print(Q1_C)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_C=d_6[a_6[m_2]]+.25*(((d_6[a_6[f_1]])-(d_6[a_6[t_1]])))# 75% of data will lie below Q3 .
print(Q3_C)
QD_C=(Q3_C-Q1_C)/2# QUANTILE DEVIATION Of Cholesterol..mg.
QD_C
IQR_C=(Q3_C-Q1_C)
IQR_C 
V_5=var(D_6)
print(V_5)
SD_5=sqrt(V_5) # VARIANCE AND SD OF Cholesterol..mg
print(SD_5)
#summary(data)
d_7<-c(data$Dietary.Fibre..g.)
a_7<-order(data$Dietary.Fibre..g.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_7[M])
Q1_D=(d_7[a_7[M]])+.75*(((d_7[a_7[f_1]])-(d_7[a_7[t_1]])))#Q1 OF Dietary.Fibre..g.
print(Q1_D)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_D=d_7[a_7[m_2]]+.25*(((d_7[a_7[f_1]])-(d_7[a_7[t_1]])))# 75% of data will lie below Q3 .
print(Q3_D)
QD_D=(Q3_D-Q1_D)/2# QUANTILE DEVIATION OF Dietary.Fibre..g.
QD_D
IQR_D=(Q3_D-Q1_D)
IQR_D 
#summary(data)
d_8<-c(data$Sugars..g.)
a_8<-order(data$Sugars..g.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_8[M])
Q1_Su=(d_8[a_8[M]])+.75*(((d_8[a_8[f_1]])-(d_8[a_8[t_1]])))#Q1 OF Sugars..g.
print(Q1_Su)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_Su=d_8[a_8[m_2]]+.25*(((d_8[a_8[f_1]])-(d_8[a_8[t_1]])))# 75% of data will lie below Q3 .
print(Q3_Su)
QD_Su=(Q3_Su-Q1_Su)/2# QUANTILE DEVIATION OF Sugars..g.
QD_Su
IQR_Su=(Q3_Su-Q1_Su)
IQR_Su
V_7=var(d_8)
print(V_7)
SD_7=sqrt(V_7)# VARIANCE AND SD OF Sugars..g.
SD_7
d_9<-c(data$Protein..g.)
a_9<-order(data$Protein..g.,decreasing = FALSE)
print(Z)
t_1=M-1
f_1=M+1
print(a_9[M])
Q1_P=(d_9[a_9[M]])+.75*(((d_9[a_9[f_1]])-(d_9[a_9[t_1]])))#Q1 OF Protein..g.     
print(Q1_P)
m_2=floor(z_2)
print(m_2)
t_1=m_2-1
f_1=m_2+1
Q3_P=d_9[a_9[m_2]]+.25*(((d_9[a_9[f_1]])-(d_9[a_9[t_1]])))# 75% of data will lie below Q3 .
print(Q3_P)
QD_P=(Q3_P-Q1_P)/2# QUANTILE DEVIATION of Protein..g.     
QD_P
IQR_Su=(Q3_P-Q1_P)
IQR_Su
V_8=var(d_9)
print(V_8)
SD_8=sqrt(V_8)# VARIANCE AND SD OF Protein..g.
SD_8
summary(data)

# CHECKING THE OUTLIERS IN DATASET
#An Outlier is an observation that lies an abnormal distance from other values in a random sample from a population
pdf("plot_box_data.pdf")
#typeof(data)
boxplot(data$Calories,names("Calories"))
boxplot(data$Trans.Fat..g.,names = c("Trans.FAT..g."))
boxplot(data$Saturated.Fat..g.,names = c("Saturated.Fat..g."))
boxplot(data$Sodium..mg.,names = c("Sodium..mg."))
boxplot(data$Total.Carbohydrates..g.,names = c("Total.Carbohydrates..g."))
boxplot(data$Cholesterol..mg.,names = c("Cholesterol..mg."))
boxplot(data$Dietary.Fibre..g.,names = c("Dietary.Fibre..g."))
boxplot(data$Sugars..g.,names=c("Sugars..g."))
boxplot(data$Protein..g.,names = c("Protein..g."))
while (!is.null(dev.list()))  dev.off()
# DROPING THE NON NUMERICAL ATTRIBUTES FROM DATASET FOR FINDING THE CORRELATION MATRIX 
data_1 <- data[-c(1,2,3,5,14,15,16,17,18)]
head(data_1)

# RELATIONSHIP ANALYSIS
# correlation matrix, which is used to investigate the dependence between multiple variables at the same time.
#The result is a table containing the correlation coefficients between each variable and the others.
# COMPUTING CORR. MATRIX
res <- cor(data_1)
round(res, 2)
res
# Draw a correlogram
install.packages("corrplot")
pdf("corr_plt.pdf")
library(corrplot)
corrplot(res, type = none, order = "hclust", 
         tl.col = "black", tl.srt = 45)

while (!is.null(dev.list()))  dev.off()
# SCATTER PLOTS
summary(data_1)
pdf("scatter.pdf")
# Change point shape (pch = 19) and remove frame.
plot(data_1$Calories, data_1$Protein..g., main = "SCATTER PLOT",
     xlab = "CALORIES", ylab = "PROTIEN",
     pch = 19, frame = FALSE)
# Add regression line

plot(data_1$Calories, data_1$Total.Carbohydrates..g., main = "Main title",
     xlab = "CALORIES", ylab = "PROTIEN",
     pch = 19, frame = FALSE)
abline(lm(data_1$Calories ~ data_1$Total.Carbohydrates..g., data =data_1), col = "blue")
plot(data_1$Trans.Fat..g., data_1$Total.Carbohydrates..g., main = "Main title",
     xlab = "CALORIES", ylab = "PROTIEN",
     pch = 19, frame = FALSE)
abline(lm(data_1$Trans.Fat..g. ~ data_1$Total.Carbohydrates..g., data =data_1), col = "blue")
plot(data_1$Dietary.Fibre..g., data_1$Total.Carbohydrates..g., main = "Main title",
     xlab = "CALORIES", ylab = "PROTIEN",
     pch = 19, frame = FALSE)
abline(lm(data_1$Dietary.Fibre..g. ~ data_1$Total.Carbohydrates..g., data =data_1), col = "blue")
while (!is.null(dev.list()))  dev.off()
# converting the first data set in numeric for finding the correlation
head(df,5)
df1<???lapply(df,as.numeric)
str(df1)
df1$Carb...g.[is.na(df1$Carb...g.)] <- median(df1$Carb...g., na.rm = T)
print(df1$Carb...g.)# Replaced the null values of the Carb...g. attributes with median values Because Outliers are present In this data set . 
data_3<- data[c(1:177),c(5:14)]
nrow(data_3)
tail(data_3)
cor(data_3$Total.Carbohydrates..g., df1$Carb...g., method = c("pearson", "kendall", "spearman"))

#cor.test(x, y, method=c("pearson", "kendall", "spearman"))