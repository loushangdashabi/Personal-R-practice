###R包的加载和卸载###
installed.packages()#查看已经安装的R包
.libPaths()#显示R包所在的位置

install.packages('ggplot2')

install.packages('devtools')
library(devtools)#包的名字不用引号
install_github('lijian13/rinds')#加载GitHub上的包，需要devtools包

#R包的卸载
#remove.packages('包名')，或者直接在packages里面点×



###向量###
x <- 1 #Alt + -快捷输入 <- 
z <- 1:5 #形成1-5的正整数
a <- c(1,2,4,3,5)

identical(a,z) #判断a,z是否相同，R语言向量的方向体现在index的一致性

vector1 <- 1:10
vector1[1:4] #访问vector中index为1-4的元素
vector1[-c(2,5,8)] #去掉index为2,5,8的元素；c为combine，为结合的意思
vector2 <- c(1,4,'ABC','nihao')

x <- 1:5
y <- 6:10
result <- x+y #相同index的元素相加
x <- 1
result2 <- x+y #x中仅有一个元素，所有y中元素都加x
x <- 1:2
result3 <- x+y #报错，长目标元素个数不是短目标元素个数的倍数
y <- 6:11
result3 <- x+y #按照短目标元素index以此循环加至长目标元素



###数值型向量###
vector1 <- c(1:3,6:10)
x <- 1
class(x)
is.integer(x) #判断是否是整型
y <- 1:3
is.integer(y)

seq(from = 1, to = 5, by = 1) #从1到5，生成步长为1
seq(10,1,-1) #简写
seq(1,5,length.out = 8) #生成8位的向量，也可以直接len=8
seq(1,5,along.with = 1:3) #以along.with后面的序列长度为模板生成特定规律的向量
seq(1,7,along.with = 1:3)

rep(c(1,3),times=5) #有规律有方法地重复元素，输出1 3 1 3 1 3 1 3 1 3
rep(c(1,3),each=5) #输出1 1 1 1 1 3 3 3 3 3
rep(c(1,3),len=9)

x <- rep(c(1:2),times=4)
length(x)



###逻辑型向量###
logit <- rep(c(T,F),len=5) #T为1，F为0
sum(logit)

#逻辑表达式
#逻辑判断符&表示和、|表示或，其余与python中相同,仅输出TRUE和FALSE
x <- seq(1,100,len=20)
index <- x>80 #index为逻辑型
x[index] #输出index中为TURE的

which(x>80) #which函数为数值型
x[which(x>80)]

x[which(x>80 & x<90)]
x[x>80 & x<90]



###字符串###
string <- c('abc','def',1,2) #统一被c函数整合为
is.character(string) #字符串格式被称为是character

letters #内置的所有小写字母的字符串向量
LETTERS #内置的所有大写字母的字符串向量
#如果提取的index超过向量的最大index会返回NA(NOT available)



###因子型向量（分类变量，分为有序和无序）###
my_fac <- factor(x =rep(c(1,2), time=5), levels = c(1,2), labels = c('Male','Female'))
class(my_fac) #输出"factor"

my_fac2 <- factor(LETTERS[1:5],labels = letters[1:5])
my_fac3 <- factor(letters[1:5],labels = letters[1:5])

my_fac4 <- gl(n = 2, k = 5, labels = c('control','treatment'))
#n和k都接收正整数，n表示level，k表示重复的次数

my_fac5 <- gl(n = 2, k = 1, length = 8, labels = c('control','treatment'))

temp_string <- c('A','B','AB','O')
my_fac6 <- as.factor(temp_string) #将字符串或者数值型转化为因子
as.character(my_fac6) #将因子或者数值型转化为字符串

nlevels(my_fac6) #返回因子型变量的水平个数
levels(my_fac6) #返回因子型变量的具体水平

#生成哑变量，即设置reference，R中默认level中的第一位为reference
my_fac7 <- relevel(my_fac6, ref = 'B')

x <- c('Placebo','10mg','20mg','50mg')
my_order_fac <- factor(x,ordered = T)
#这样生成的有序分类变量会出现Levels: 10mg < 20mg < 50mg < Placebo的错误
#解决方法1：用0mg代替Placebo

#解决方法2：
install.packages("DescTools")
library(DescTools)
my_order_fac2 <- reorder.factor(my_order_fac,new.order = x)
#即根据x的顺序对因子进行排序，但是我觉得很拉跨这个方法



###列表和矩阵###
my_list <- list(1,2,3,'R','nihao',T,F)
my_list2 <- list(1:10,letters[1:5])
my_list2[[2]][1]
my_list3 <- list(1:10,letters[1:5],list(11:14,LETTERS[1:5],list(15:18)))
my_list3[[3]][[2]][1]

my_matrix <- matrix(data = 1:6, nrow = 2, byrow = T)
my_matrix2 <- matrix(data = 1:10, nrow = 5)
#data输入的数字，nrow为生成的矩阵行数,ncol表示列数
#byrow默认值为F，表示纵向排列数据，T时横向排列数据
my_matrix3 <- matrix(data = 2, nrow = 3, ncol = 4)
my_matrix4 <- matrix(data = letters[1:3], nrow = 2, ncol = 4)
#如此生成my_matrix4会产生警告，但是不影响生成，因为nrow × ncol不是data的整数倍
#修改行和列的名称
my_matrix5 <- matrix(data = 1:12, nrow = 3, ncol = 4, 
                     dimnames = list(c('A','B','C'),c('V1','V2','V3','V4')))
#dimnames接收的是一个list，list[[1]]为行名，list[[2]]为列名，dim为dimension维度的意思

#转置
t(my_matrix5)

my_matrix6 <- matrix(c(1:5,letters[1:5]),nrow = 2)



###数组###
my_array <- array(data = 1:16, dim = c(2,4,2)) #dim的含义为生成2行4列2层的数组
dim(my_array) #返回数组的维度信息
dim(my_array) <- c(4,2,2)

my_array2 <- array(data = 1:16, dim = c(4,2,2), 
                   dimnames = list(c(LETTERS[1:4]),c('col1','col2'),c('first','second')))
#访问
my_matrix <- matrix(1:8, nrow = 4)
my_matrix[4,2]
my_matrix[,2]
my_matrix[3,]
my_array2[2,2,1]
my_array2[16]



###数据框###
my_df <- data.frame(name = c('tom','andy','marry'),
                    age=c(24,25,26),height=c(178,168,180))
#不合法的变量名：纯数字、数字开头、运算符。
#变量名区分大小写
dim(my_df)
View(my_df)
nrow(my_df)

#鸢尾花数据集（R中内置数据集）
View(iris)

my_df <- data.frame(one = c(1,2,3,4,5,5),
                    two = c('张三','李四','王五','赵六','田七','韩八'),
                    three=c(T,F,T,T,F,T),stringsAsFactors = T)
#如果变量间数据量不同，会报错
#stringsAsFactors默认值为F，改为T会将str改为factor形式
str(my_df) #查看每个变量的具体信息
#去除变量
my_df[,-2] #去除my_df中的第二列
my_df$two <- NULL
#$符号可以用来快捷查看和提取那些变量
#NULL的赋值用于删除某些，区别于NA的删除后保留空位（即缺失值）
#新增变量
my_df$four <- letters[1:6]

#修改变量值
edit(my_df) #一次性操作返回修改完的值，不会改变原始的数据框，用于直接赋值
my_df2 <- edit(my_df)
fix(my_df) #对原始数据库进行改变的操作，不返回值

head(iris,n=5) #返回数据集的前几行数据，n默认值为6
tail(iris,n=5) #返回数据集的后几行数据，n默认值为6

install.packages('psych')
library(psych)
describe(iris) #psych包中的函数，用于描述每个变量的情况
#trimmed表示去除极端值的均数，mad为众数，skew为偏度，kurtosis为峰度

names(iris) #可以用于查看names，也可以通过对names进行赋值修改变量名

Sys.setlocale(category = "LC_CTYPE", locale = "zh_CN.UTF-8")
#此条代码仅适用于mac系统，将数据框中的中文变量名在R中可视
Sys.setlocale(locale = "chinese")
#适用于windows系统的将数据框中的中文变量名在R中可视



###数据框基本操作###
##合并数据框
my_df <- data.frame(one = c(1,2,3,4,5,5),
                    two = c('张三','李四','王五','赵六','田七','韩八'),
                    three=c(T,F,T,T,F,T),stringsAsFactors = T)
my_df2 <- data.frame(four = c(1,2,3,4,5,5),
                    two = c('张三','李四','王五','赵六','田七','韩八'),
                    six=c(T,F,T,T,F,T),stringsAsFactors = T)
my_df3 <- cbind(my_df,my_df2)
#cbind为列合并，要求结合的两个数据框行数一致，否则报错
#两个数据框变量名可以存在相同，最后会共同存在，此条区别于merge函数
my_df4 <- data.frame(one = c(1,2,3,4,5,5),
                    two = c('张三','李四','王五','赵六','田七','嘤嘤嘤'),
                    three=c(T,F,T,T,F,T),stringsAsFactors = T)
my_df5 <- rbind(my_df,my_df4)
#rbind为行合并，要求结合的两个数据框变量名一致，否则报错
my_df6 <- data.frame(one = c(1,2,3,4,5,5),
                    seven = c('张三','李四','王五','赵六','田七','嘤嘤嘤'),
                    eight=c(T,F,T,T,F,T),stringsAsFactors = T)
my_df7 <- merge(my_df,my_df6,by = )
#merge可以合并一致的变量、行，后面可以加by=/by.x=/by.y=以设置合并的依据

##分割数据框
View(iris)
iris_sub <- iris[sample(1:nrow(iris),30),]
#sample函数用于随机抽样，如果抽样数30大于1:nrow(iris)，后面应当加参数replace
set.seed(2021)
#设置种子后，与后面的sample函数一起运行可使得每次抽样相同，相当于抽样标签？
sample(1:nrow(iris),30)

iris_sub2 <- split(iris,f=iris$Species) #此时分割后iris_sub2类型为list
setosa <- as.data.frame(iris_sub2[1]) #调用列表进行赋值生成数据框

iris_sub3 <- iris[iris$Species=='setosa'&iris$Sepal.Length>4.5,1:2]
range(iris$Sepal.Width) #返回函数某个变量的范围

iris_sub4 <- subset(iris,iris$Species=='setosa'&iris$Sepal.Length>4.5,select = 1:2)
#subset参数用于输入条件，select用于筛选列



###条件和循环###
i <- 1
if (i>0) print('i is positive') else
  print('i is negative')

#条件中执行语句的两种写法(换行与不换行)
if (i>2)
{y=3*i;z=y*8} else
{y=5
z=y+i*2}
y;z

x <- 10
y <- c(8,10,11,12,14)
if (x<y) x else y
#if语句的判断中不认向量
#会出现‘the condition has length > 1 and only the first element will be used’的警告
#仅比较向量中第一位，仅输出一位，区分与单独的x<y的判断(会出现5位输出)

#循环
i <- 5
repeat {if (i>25) break else
{print(i)
  i <- i+5}
  }

i <- 5
while (i<=25) {
  print(i)
  i <- i+5
}

for (i in 1:10) {
  print(i)
}

set.seed(2021)
x <- sample(10:100,10)
y <- sample(1:100,10)
z <- NULL
for (i in 1:10) {
  if (x[i]>y[i]) {
    z = append(z,x[i])
  }
}
#z [1] 67 79 73 99 78



###自定义函数###
mean(c(1:5,NA)) #存在NA时输出为NA
mean(c(1:5,NA),na.rm = T) #na.rm参数设置为T后会自动舍去NA

my_fun1 <- function(x,y){
  x+y
}
my_fun2 <- function(x,y=3){
  x+y
} #设置函数中的默认值

values <- c(sqrt(1:100))
my_fun3 <- function(x,...){
  print(x)
  summary(...)
} #...表示缺省参数，可以指代运用已有函数中的参数

normalize <- function(x,m=mean(x,...),s=sd(x,...),...){
  (x-m)/s
}
