Sys.setlocale(locale = "chinese")
###数据读写###
#1.以列的形式逐一键入
ID <- c(1,2,3)
age <- c(22,13,24)
status <- c('p','r','p')
dataf <- data.frame(ID,age,status)

#2.edit()方法赋值或fix()方法直接修改输入，先确定好变量属性
dataf2 <- data.frame(patientID=character(0),age=numeric(),lalala=character())
dataf2 <- edit(dataf2)
fix(dataf2)

#3.读入本地文件
#read.table读取文本文档,有缺失值时会报错；read.csv读取csv文档，有缺失值时会自动填补NA
dataf3 <- read.csv('C:/Dataset/WHO-COVID-19-global-table-data.csv')
View(dataf3)

#4.创建工作空间
dir.create('c:/R_workspace')
setwd('c:/R_workspace')
getwd()
list.files()

#5.读入xlsx文件
install.packages('xlsx')
library(xlsx)

#6.点击式：File -> Import Dataset，代码如下
#此方式不能导入csv文件，且储存路径不能出现中文
library(readxl)
WHO_COVID_19_global_table_data <- read_excel("C:/Dataset/WHO-COVID-19-global-table-data.xlsx")
View(WHO_COVID_19_global_table_data)



###数据排序与长宽型数据转换###
x <- sample(1:100,10)
sort(x,decreasing = T) #对x进行排序，decreasing默认为F，为升序排列，T时降序排列
rank(x) #按原顺序返回每个数排序后的序号
order(x) #按排序后顺序返回每个数在原顺序中的序号
x[order(-x)] #对x进行排序，在x前加上-号或者decreasing设置为T可实现降序
head(iris[order(-iris$Sepal.Length,iris$Sepal.Width),]) #对iris数据集进行排序

#长宽型数据转换
freshmen <- c(178,180,182,180)
sophomores <- c(188,172,175,172)
juniors <- c(167,172,177,174)
height <- stack(list(fresh=freshmen,sopho=sophomores,jun=juniors)) #生成长型数据
tapply(height$values,height$ind,mean)

View(Indometh)
wide <- reshape(Indometh, v.names = 'conc', idvar = 'Subject', 
                timevar = 'time', direction = 'wide') #reshape函数将长型转换为宽型
long <- reshape(wide,idvar = 'Subject', varying = list(2:12), 
                v.names = 'concentrtion',direction = 'long') #reshape函数将宽型转换为长型

install.packages('reshape2')
install.packages('Rcpp')
library(reshape2)
library(Rcpp)
new_iris <- melt(data = iris,id.vars = 'Species')

View(new_iris)
View(iris)


dcast()

###未完待续



###变量的因子化(变量的转换)###
age <- sample(20:80,20)

#1.公式法的两种写法
age1 <- 1+(age>30)+(age>=40)+(age>=50)
age_fac <- factor(age1,labels = c('young','middle','middle_old','old'))

age2 <- 1*(age<30)+2*(age>=30&age<40)+3*(age>=40&age<50)+4*(age>=50)
age_fac <- factor(age2,labels = c('young','middle','middle_old','old'))

#2.cut()法
age3 <- cut(age, breaks = 4, labels = c('young','middle','middle_old','old'),
            include.lowest = T, right = T)
age4 <- cut(age, breaks = seq(20,80,length=4), labels = c('young','middle','old'))
#此处切分形成4个切分点，即3个区段，区别于上一种切分方式

#3.ifelse()法，常用于嵌套
age5 <- ifelse(age>50,'old','young')
class(age5) #输出"character"
ifelse(age>60,'old',ifelse(age<30,'young','middle')) #嵌套时注意逻辑是否重叠

#4.car包
library(car)
recode(var = age, recodes = 'low:29 = 1; 30:39 = 2; 40:49 = 3; 50:hi = 4')



###apply函数家族###
#1.apply(),对数据框或者矩阵的行(参数为1)或列(参数为2)进行某个函数的操作
mat <- matrix(1:24, nrow = 4, ncol = 6)
apply(mat,1,sum)
apply(iris[,1:4], 2, sd)

#2.lapply(),起到遍历每个量的作用,最后返回一个list
lapply(1:5,log)
lapply(iris[,1:3],function(x)lm(x~iris$Petal.Width,data = iris[,1:3]))
#函数返回值为list时(如示例的线性回归),仅可使用lapply()函数，其他函数会报错

#3.sapply(),返回数值型向量
sapply(1:5,function(x)x+3)

#4.★tapply(),仅适用于数据框,其他报错,输出一个数组
class(tapply(iris$Sepal.Length, INDEX = iris$Species, mean))
#根据一个分类变量对另一个变量进行切分,并进行汇总

#5.mapply()
my_fun <- function(x,y){
  if(x>4)return(y)
  else return(x)
}
my_fun(1:5,2:6)
#此时会出现警告,因为if语句不能进行向量化操作
mapply(FUN = my_fun,1:5,2:6) #使if具有向量化操作的能力



###数据汇总函数###
survival <- data.frame(id=1:10,
                       cancer=sample(c('lung','liver','colon'),10,replace = T),
                       treatment=sample(c('Surg','Chemo'),10,replace = T),
                       sur_day=sample(100:1000,10))

ave(survival$sur_day, survival$cancer, FUN = sd)
#输出和元素个数相同的数,但数值的种类数与分类变量的种类相同
#可以通过FUN参数改变函数

by(data = survival$sur_day, INDICES = list(survival$cancer,survival$treatment), FUN = mean)
#INDICES函数既可以接收单个factor也可以接收list，如果没有值的话返回NA


data(mtcars)
aggregate(mtcars, by = list(VS=mtcars$vs==1, high=mtcars$mpg>22),mean)
aggregate(.~Species,data=iris,mean)

by(mtcars, mtcars$cyl, function(x)lm(mpg~disp+hp, data=x))

my_array <- array(1:24,dim = c(3,4,2))
sweep(x = my_array, MARGIN = 1,STATS = 1, FUN = '+') #对array进行操作



###plyr包###
install.packages('plyr')
install.packages('dplyr')
library(plyr)
my_matrix <- matrix(c(1:24),nrow = 3,ncol = 8)
#aaply()








































































