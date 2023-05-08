# 第1章 R语言基础知识

# 以下绘图代码将在后续章节中陆续讲解

data=as.matrix(mtcars) #heatmap()接受的数值对象为矩阵，因此我们需要事先将mtcars数据框转成矩阵
heatmap(data) #不需要传入任何参数，就可以进行热图的绘制。

heatmap(data, scale="column")#传入scale参数。
heatmap(data, scale="column", col = terrain.colors(256),Colv = NA, Rowv = NA)


library(forestplot)
os_forest <- read.csv('os_forest.csv',header = FALSE)
# 读入数据的时候大家一定要把header设置成FALSE，保证第一行不被当作列名称。
tiff('Figure 1.tiff',height = 1600,width = 2400,res= 300)
forestplot(labeltext = as.matrix(os_forest[,1:3]),
           #设置用于文本展示的列，此处我们用数据的前三列作为文本，在图中展示
           mean = os_forest$V4, #设置均值
           lower = os_forest$V5, #设置均值的上限
           upper = os_forest$V6, #设置均值的下限
           is.summary = c(T,T,T,F,F,T,F,F,T,F,F),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.4, #设置点估计的方形大小
           lineheight = unit(10,'mm'),#设置图形中的行距
           colgap = unit(3,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 1.5,#设置区间估计线的粗细
           col=fpColors(box='#458B00',  summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           xlab="The estimates",#设置x轴标签
           graph.pos = 3)#设置森林图的位置，此处设置为3，则出现在第三列


library(survival)
library(survminer)
fit <- survfit(Surv(time, status) ~ sex, data = lung)

ggsurvplot(fit,
           pval = TRUE, #在图上添加log rank检验的p值
           conf.int = TRUE,#添加置信区间
           risk.table = TRUE, #在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           linetype = "strata", # 改变不同组别的生存曲线的线型
           surv.median.line = "hv", # 标注出中位生存时间
           ggtheme = theme_bw(), # 改变图形风格
           palette = c("#E7B800", "#2E9FDF"))#图形颜色风格


ggsurvplot(
  fit,                    
  pval = TRUE,             
  conf.int = TRUE,    
  conf.int.style = "ribbon",  # 设置置信区间的风格
  xlab = "Time in days",   # 设置x轴标签
  break.time.by = 200,     # 将x轴按照200为间隔进行切分
  ggtheme = theme_light(), # 设置图形风格
  risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
  risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
  risk.table.y.text = FALSE,# 以条柱展示风险表的标签，而非文字
  ncensor.plot = TRUE,      # 展示随访过程中不同时间点死亡和删失的情况
  surv.median.line = "hv",  # 添加中位生存时间
  legend.labs = 
    c("Male", "Female"),    # 改变图例标签
  palette = 
    c("#E7B800", "#2E9FDF") # 设置颜色
)


ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", 
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")
dev.off()

# 以上绘图代码将在后续章节中陆续讲解


# Listing 1.1 - A Sample R session

age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)
sd(weight)
cor(age,weight)
plot(age,weight)
# q()


# Listing 1.2 - An example of commands used to manage the R Workspace. 

setwd("d:/dxyclass/ch01") # change the path to one of your directories
options()
options(digits=3)
x <- runif(20)
summary(x)
hist(x)
savehistory()
save.image()
# q()


# Listing 1.3 - Working with a new package

help.start()
install.packages("vcd")
help(package="vcd")
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)
# q()


#  Creating vectors
a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)


# Using vector subscripts
a <- c(1, 2, 5, 3, 6, -2, 4)
a[3]
a[c(1, 3, 5)]
a[2:6]                     


a <- c("k", "j", "h", "a", "c", "m")
a[3]
a[c(1, 3, 5)]
a[2:6]


# Listing 2.1 - Creating Matrices
y <- matrix(1:20, nrow=5, ncol=4)
y
cells    <- c(1,26,24,68)
rnames   <- c("R1", "R2")
cnames   <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames)) 
mymatrix

mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=FALSE,
                   dimnames=list(rnames, cnames))
mymatrix


# Listing 2.2 - Using matrix subscripts
x <- matrix(1:10, nrow=2)
x
x[2,]
x[,2]
x[1,4]
x[1, c(4,5)]


# Listing 2.3 - Creating an array
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2,3,4), dimnames=list(dim1, dim2, dim3))
z


# Listing 2.4 - Creating a dataframe
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata


# Listing 2.5 - Specifying elements of a dataframe
patientdata[1:2]
patientdata[c("diabetes","status")]
patientdata$age
table(patientdata$diabetes,patientdata$status)


# Listing 2.6 - Using factors
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)                               
summary(patientdata)


# Listing 2.7 - Creating a list
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h, j, k)
mylist


# Entering data interactively from the keyboard
mydata <- data.frame(age=numeric(0),
                     gender=character(0), weight=numeric(0))
mydata <- edit(mydata)


# Entering data inline
mydatatxt <- "
age gender weight
25 m 166
30 f 115
18 f 120
"
mydata <- read.table(header=TRUE, text=mydatatxt)


# Importing data from a delimited text file

# First, save the following 4 lines in a file named 
# "studentgrades.csv" in the current working directory
# StudentID,First,Last,Math,Science,Social Studies
# 011,Bob,Smith,90,80,67
# 012,Jane,Weary,75,,80
# 010,Dan,"Thornton, III",65,75,70
# 040,Mary,"O'Leary",90,95,92

# Next, read the data into R using the read.table() function
grades <- read.table("studentgrades.csv", header=TRUE,
                     row.names="StudentID", sep=",")
grades # print data frame
str(grades) # view data frame structure

# Alternatively, import the data while specifying column classes 
grades <- read.table("studentgrades.csv", header=TRUE,
                     row.names="StudentID", sep=",",
                     colClasses=c("character", "character", "character",
                                  "numeric", "numeric", "numeric"))
grades # print data frame
str(grades) # view data frame structure


# leadership dataset
manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99)
q1 <- c(5,3,3,3,2)
q2 <- c(4,5,5,3,2)
q3 <- c(5,2,5,4,1)
q4 <- c(5,5,5,NA,2)
q5 <- c(5,5,2,NA,1)
leadership <- data.frame(manager,date,gender,age,q1,q2,q3,q4,q5, 
                         stringsAsFactors=FALSE)


# Listing 4.2 - Creating new variables
mydata<-data.frame(x1 = c(2, 2, 6, 4),
                   x2 = c(3, 4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2
attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)
mydata <- transform(mydata,
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2)


# Recoding variables
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                    leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"

leadership <- within(leadership,{
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young" })


# Renaming variables with the plyr package
names(leadership)
names(leadership)[2] <- "testDate"
leadership

library(plyr)
leadership <- rename(leadership,
                     c(manager="managerID", date="testDate"))


# Applying the is.na() function
is.na(leadership[, 6:10])


# Recode 99 to missing for the variable age
leadership[age == 99, "age"] <- NA
leadership


# Excluding missing values from analyses
x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4]
z <- sum(x)

x <- c(1, 2, NA, 3)
y <- sum(x, na.rm=TRUE)


# Listing 4.4 - Using na.omit() to delete incomplete observations
leadership
newdata <- na.omit(leadership)
newdata


# Converting character values to dates
mydates <- as.Date(c("2007-06-22", "2004-02-13"))

strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")


# Working with formats
today <- Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%A")


# Calculations with with dates
startdate <- as.Date("2004-02-13")
enddate   <- as.Date("2009-06-22")
enddate - startdate


# Date functions and formatted printing
today <- Sys.Date()
dob <- as.Date("1956-10-12")
difftime(today, dob, units="weeks")


# Listing 4.5 - Converting from one data type to another
a <- c(1,2,3)
a
is.numeric(a)
is.vector(a)
a <- as.character(a)
a
is.numeric(a)
is.vector(a)
is.character(a)


# Sorting a dataset
newdata <- leadership[order(leadership$age),]

attach(leadership)
newdata <- leadership[order(gender, age),]
detach(leadership)

attach(leadership)
newdata <-leadership[order(gender, -age),]
detach(leadership)


# Selecting variables
newdata <- leadership[, c(6:10)]

myvars <- c("q1", "q2", "q3", "q4", "q5")
newdata <-leadership[myvars]

myvars <- paste("q", 1:5, sep="")
newdata <- leadership[myvars]


# Dropping variables
myvars <- names(leadership) %in% c("q3", "q4") 
leadership[!myvars]


# Listing 4.6 - Selecting observations
newdata <- leadership[1:3,]
newdata <- leadership[leadership$gender=="M" &
                        leadership$age > 30,]
attach(leadership)
newdata <- leadership[gender=='M' & age > 30,]
detach(leadership)


# Selecting observations based on dates
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$date >= startdate &
                              leadership$date <= enddate),]



# Using the subset() function
newdata <- subset(leadership, age >= 35 | age < 24,
                  select=c(q1, q2, q3, q4))
newdata <- subset(leadership, gender=="M" & age > 25,
                  select=gender:q4)


# Listing 4.7 - Using SQL statements to manipulate data frames
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg",
               row.names=TRUE)
newdf
sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear
      from mtcars where cyl in (4, 6) group by gear")

# 第一章代码结束
