#批量读取Xlsx文件+合并
#①list.files(路径,文件格式,嵌套)
files1=list.files("Data/Sample",full.names = TRUE)
files2=list.files("Data/Sample",pattern="xlsx",full.names = TRUE)
files3=list.files("Data/Sample",pattern="xlsx",full.names = TRUE,recursive = TRUE)
#②read_xlsx()+map_dfr()
library(readxl)
df=map_dfr(files3,read_xlsx)

#写出到excel
library(writexl)
write_xlsx(df,"Data/Sample/temp.xlsx")

#数据连接(匹配合并)
#左连接(以第一个表为基础,把第二个表中的数据匹配到第一个表中)
band_members
band_instruments
band=left_join(band_members,band_instruments,by="name")
#使用管道运算
band=band_members %>%
  left_join(band_instruments,by="name")

#数据操作

#创建函数function()
Cir=function(r){
  S=pi*r*r
  return(S)
}
Cir(1)
#代入一组数据
rs=c(1,2,3,pi)
map_dbl(rs,C)

#向量化编程example
y=c(rep("是",8),rep("否",9))
table(y)#计算频数
p=table(y)/length(y)
log(p)
p*log(p)
-sum(p*log(p))

#基本运算 + - * / %%(模) %/%(整除)
identical(sqrt(2)^2,2)#比较两者是否完全一致
all.equal(sqrt(2)^2,2)
dplyr::near(sqrt(2)^2,2)#比较两浮点数是否近似相等(误差1.5e-8)

#逻辑运算 | & ！ xor()

#查看对象的类型m<c<t
x=c("是","否")
x=factor(x)
mode(x)
class(x)
typeof(x)

#保存和载入数据
save(x,file="example.Rda")
load("example.Rda")

#创建向量
#数值向量
x = c(1,2,3)
x=numeric(10)#创建长度为10的全为0的向量
x1=c(1,2,3,x)
x1=c(1,2,3,c(4,5,6))#合并向量
1:5
seq(1,10,2)#1~10,步长为2
seq(3,length.out=10)#3开始，数10个
x=1:3
rep(x,2)#重复x两次
rep(c(1,2,3),3,length.out=50,each=2,times=3)
#逻辑向量
c(1,2)>c(2,1)#等价于c(1>2,2>1)
c(2,3)>c(1,2,-1,3)#等价于c(2>1,3>2,2>-1,3>3)
c(1,4)%in%c(1,2,3)#判断元素是否属于后面的集合
#字符向量
writeLines("Is \"You\" a Chinese name?")#字符串中需要""用转义符\隔开

#访问向量子集
x=c(7,2,1,4)
x[1]#第1个元素
x[2:3]#第2-3个元素
x[-3]#除了第3个元素
x[c(1,3)]#第1和第3个元素
x[c(1,-1)]#同时正负数会报错
x[5]#可以访问不存在的位置，返回NA
x[c(TRUE,TRUE,FALSE,TRUE)]#输入与向量相同长度的逻辑向量，确定每个元素是否需要获取
x[x<=3]
x[x^2-x>=2]
which.max(x)#返回最大值所在位置
which.min(x)#返回最小值所在位置

#对子集赋值
x[2]=3
x[2:4]=c(0,1,3)
x[c(TRUE, FALSE, TRUE, FALSE)] = c(3, 2)
x[x<=2]=0
x[6]=1#对不存在位置赋值,其前用NA补齐

#对元素命名
x=c(a=1,b=2,c=3)
x[c("a","c","a","d")]#可以重复访问,不存在的命名返回NA
names(x)#获取命名
names(x)=c("x","y","z")#改名
names(x)=NULL#移除命名
x["a"]#提取命名为a的子集
x[["a"]]#提取a中的元素(限1个)
x=c(a=c(1,2,3),b=2,c=3)#自动编号a1,a2,a3

#向量排序
#sort(x,decreasing,na.last,...)
#decreasing:默认FALSE为升序,TRUE为降序
#na.last:默认FALSE去除NA,TRUE则将NA放置末尾
x=c(1,5,8,2,NA,9,7,4)
sort(x)
sort(x,decreasing = TRUE)
sort(x,na.last=TRUE)
order(x)#默认升序,获得"这个位置排的是原向量的哪个角标",如第2位应该排原向量里第4位的"2"
x[order(x)]#同sort(x,na.last=TRUE)
rank(x)#默认升序,获得"这个值排第几",如原向量里第2位的"5"应该排第4位
order(order(x))#同rank(X)

#创建矩阵
#matrix(数据源,行数,列数,是否按行排列,命名)
x1 = matrix(1:16,nrow=4,ncol=4,byrow=TRUE)
x2 = matrix(1:16,nrow=4,ncol=4,byrow=FALSE)
matrix(1:9,nrow=3,ncol=3,byrow=TRUE,
       dimnames = list(c("r1","r2","r3"),c("c1","c2","c3")))
m=matrix(1:9,nrow=3,ncol=3,byrow=TRUE)
rownames(m)=c("r1","r2","r3")
colnames(m)=c("c1","c2","c3")
x=diag(1:5)#对角矩阵
as.vector(x)#将矩阵转化为向量,按列读取

#访问矩阵子集
m[1,2]#第1行第2列
m[1:2,2:3]#1-2行,2-3列
m[c("r1","r3"),c("c1","c3")]
m[1,]#第1行
m[,2:3]#2-3列
m[,-1]#除第1列
m[-c(1,2),]#除第1、2行
m > 3#逻辑运算
m[m>3]#输出的结果为向量

#矩阵运算
x1+x2
x1-x2
x1*x2
x1/x2#点运算
x1 %*% x2#矩阵乘法,要求A列数=B行数

#创建数组
#array(数据源,各维度最大下标,标题) 
dim1=c("A1","A2")
dim2=c("B1","B2","B3")
dim3=c("C1","C2","C3","C4")
x=array(1:24,dim=c(2,3,4),dimnames = list(dim1,dim2,dim3))
a1=array(1:24,dim=c(3,4,2),
         dimnames=list(c("r1","r2","r3"),
               c("c1","c2","c3","c4"), c("k1","k2")))
#创建后再命名
a1=array(1:24,dim=c(3,4,2))
dimnames(a1)=list(c("r1","r2","r3"),
                  c("c1","c2","c3","c4"), c("k1","k2"))
#访问数组子集
a1[2,4,2]#第2行第4列第2页
a1["r2","c4","k2"]
a1[1,2:4,1:2]
a1[,,2]
dim(a1)#返回各维度维数

#创建列表
l0=list(1,c(TRUE,FALSE),c("a","b","c"))
l1=list(A=1,B=c(TRUE,FALSE),C=c("a","b","c"))#创建时命名
names(l1)=NULL
names(l1)=c("x","y","z")
#列表中提取成分(下一级元素)
l1$y
l1$m#提取不存在的值返回NULL
l1[[2]]
l1[["y"]]
#列表中提取子集(仍是列表)
l1[2]
l1[-2]
l1[c("x","z")]
#对列表成分赋值
l1$x=0
l1[c("x","y")]=list(x="new value for x",y=c(3,1))
l1[c("z","m")]=NULL#删除元素
#列表函数
l2=as.list(c(a=1,b=2))#向量转化为列表
unlist(l2)#列表转化为向量
#tidyverse中的purrr包
pluck(l1,"A")#等同于l1[["A"]]
keep()#保留满足条件的元素
diacard()#删除满足条件的元素
compact()#删除空元素
append()#列表末尾增加元素
flatten()#摊平列表

#创建数据框
x1=c(1,2,3)
x2=c("一","二","三")
x3=c(TRUE,TRUE,FALSE)
x=data.frame(x1,x2,x3)
str(x)
#因子化
x2=factor(x2)
x=data.frame(x1,x2,x3)
str(x)
#改用tidyverse中更现代的tibble
persons=tibble(
  Name=c("Ken", "Ashley", "Jennifer"),
  Gender=c("Male","Female","Female"),
  Age=c(24,25,23),
  Major=c("Finance", "Statistics", "Computer Science")
)
persons=tribble(
  ~Name,~Gender,~Age,~Major,
  "Ken","Male",24,"Finance",
  "Ashley","Female",25,"statistics",
  "Jennifer","Female",23,"Computer Science"
)
x=as_tibble(a1)#将data.frame或matrix或各成分长度相等的list转化为tibble
#行列重命名
df=tibble(
  id=1:4,
  level=c(0,2,1,-1),
  score=c(0.5,0.2,0.1,0.5)
)
names(df)=c("id","x","y")
#提取数据框的元素和子集
#列表方式提取
df$x
df[["x"]]
df[[2]]#，提取一个元素,以上三者等价
df[1]
df["id"]#提取子集，二者等价
df[1:2]
df[c("id","x")]
df[c(TRUE,TRUE,FALSE)]#三者等价
#矩阵方式提取
df[,"x"]
df[,c("x","y")]
df[1:3,]
df[1:3,c("id","y")]
df[df$y>=0.5,c("id","y")]
ind=names(df) %in% c("x","y","w")
df[1:2,ind]
#给数据框赋值
#以列表方式赋值
df$y=c(0.6,0.3,0.2,0.4)
df$z=df$x+df$y#利用现有列创建新列
df$z=as.character(df$z)#转换列的类型
df[c("x","y")]=list(level=c(1,2,1,0),
                    score=c(0.1,0.2,0.3,0.4))
#矩阵方式赋值
df[1:3,"y"]=c(-1,0,1)
df[1:2,c("x","y")] = list(level = c(0,0),
                          score = c(0.9,1.0))
#一些函数
str(df)
glimpse(df)#显示结构
summary(df)#平均值、中位数、最大最小、四分位数
#增加一条记录
rbind(persons,
      tibble(Name="John",Gender="Male",
             Age=25,Major="Statistics"))
#添加新列
cbind(persons,Registered=c(TRUE,TRUE,FALSE),
              Projects=c(3,2,3))
#生成多个属性水平值所有组合的数据框
expand.grid(type=c("A","B"),class=c("M","L","XL"))

#创建因子
x=c("优", "中", "良", "优", "良", "良") 
x1=factor(x,levels = c("中","良","优"))
as.numeric(x1)
#因子化之后具有顺序
sort(x1)
table(x1)
ggplot(tibble(x1),aes(x1))+geom_bar()
#访问或修改水平值
levels(x1)=c("Excellent","Good","Fair")
levels(x1)=unique(x)#水平的顺序与数据集中首次出现的次序匹配
#有序因子与无序因子
x2 = factor(x, levels = c("中", "良", "优"), ordered = TRUE)
#有用函数
table(x1)#统计频次
Age=c(23,15,36,47,65,53)
#连续变量离散化
cut(Age,breaks=c(0,18,45,100),
    labels=c("Young","Middle","Old"),
    right=TRUE,include.lowest=TRUE,
    ordered_result=TRUE)
#生成有规律的水平值组合因子
tibble(
  Sex=gl(2,3,length=12,labels=c("男","女")),
  Class=gl(3,2,length=12,labels=c("甲","乙","丙")),
  Score=gl(4,3,length=12,labels=c("优","良","中","及格"),ordered=TRUE))
#gl(因子水平个数,重复次数,水平值,是否有序)

#forcats包(处理因子)
x1=c("中","良","中","优","中","优")
x2=as.factor(c("良","中","优","合格",NA,NA))
x1=as.factor(x1)#转化为因子,默认顺序为出现的次序
fc=fct_c(x1,x2)#合并多个因子的水平
fct_count(fc)#计算各水平的频数
#改变因子水平的顺序
fct_relevel(fc,"中",after=0)#手动调整,0是第一位
fct_infreq(fc)#按高频优先排列
fct_inorder(fc)#按水平出现的顺序排列
fct_rev(fc)#顺序反转
fct_reorder()#绘图时用,按其他函数的结果排序
#修改水平
fc1=fct_recode(fc,"合格"="中","不合格"="合格")#把"中"改为"合格","合格"改为"不合格"
fc1=fct_collapse(fc1,"合格"=c("合格","不合格"))#手动合并部分水平
fct_lump_min(fc1,min = 4)#合并频数小于4的为Other
fct_lump_lowfreq(fc1)#合并最小的几个,得到的Other仍为最小的
fct_lump_n(fc1,n=2)#剩下n个,其余合并为Other
fct_lump_prop(fc1,prop=0.3)#合并占总频数小于一定比例的
fct_other(fc1,keep="优")
fct_other(fc1,drop=c("合格","不合格"))
#增加或删除水平
fct_expand(fc1,"及格")
fct_drop(fc1)#删除频数为0的水平,only=仅删除该水平
fct_explicit_na(fc,"无数据")#为NA设置水平

#操作数据框中的因子
count(mpg,class)
mpg1=mpg%>%
  mutate(class=fct_lump_lowfreq(class))
count(mpg1,class)
p1=ggplot(mpg1,aes(class))+
  geom_bar()
p2=ggplot(mpg1,aes(fct_rev(fct_infreq(class))))+
  geom_bar()
library(patchwork)
p1|p2

#字符串的长度
str_length(c("a","R for data science",NA))#获得每个字符串的长度
str_pad(c("a","ab","abc"),3)#填充长度到3
str_trunc("R for data science",10)#截断长度到10
str_trim(c("  a","b  ","a b"))#移除前后的空格
#以上三个函数可加side=c("both", "left", "right")设定操作的方向

#字符串合并
str_c("x",1:3,sep="")#sep设置间隔符，默认为空字符
str_c("x",1:3,collapse="/")#collapse将字符合并为字符串,并添加间隔符
str_dup(c("A","B"),3)#每个字符重复三次
str_dup(c("A","B"),c(3,2))#A重复3次，B重复2次

#字符串拆分
x="10,8,7"
str_split(x,",")#返回列表
str_split_fixed(x,",",3)#返回矩阵

#字符串格式化输出,可引入数据框、列表等
str_glue("Pi={pi}")
name=list("李明","张三","王五")
tele=list("13912345678","15868835031","18953117199")
str_glue("姓名:{name}","电话:{tele}",.sep=";")

#字符串排序
x=c("banana","apple","pear")
str_sort(x,decreasing=TRUE)#默认升序,返回排好序的元素
str_order(x)#返回索引
str_sort(c("橘子","香蕉","苹果"),locale="ch")#locale设定语言

#检测匹配
str_detect(x,"p")#字符中是否含有p,返回逻辑值
str_which(x,"p")#查找匹配的索引
str_count(x,"p")#计算匹配次数
str_locate(x,"p")#定位匹配的位置
str_starts(x,"p")#是否以p开头
str_ends(x,"p")#是否以p结尾

#提取字符串子集
str_sub(string,start=1,end=-1)
str_sub(x,1,3)
str_sub(x,1,5)#长度不足则取到最后一位
str_sub(x,-3,-1)#倒数第一到倒数第三位
str_subset(x,"p")#提取包含“p”的字符串,negate=TRUE则提取不包含的

#提取匹配的内容（第一个）,函数名加_all查找所有匹配
x=c("1978-2000","2011-2020-2099")
pat="\\d{4}"#正则表达式，表示匹配四位数字
str_extract(x,pat)
str_match(x,pat)#返回矩阵

#修改字符串（第一个）,函数名加_all修改所有
str_replace(x,"-","/")

#大小写转换
str_to_lower("I love r language")
str_to_upper("I love r language")
str_to_title("I love r language")#每个单词首字母大写

#日期时间
library(lubridate)
#识别日期时间
today()
now()
as_datetime(today())#日期型转日期时间型
as_date(now())#日期时间型转日期型
ymd("2020/03!01")#无论什么符号连接都可识别
myd(03202001)
dmy("03012020")
ymd_hms("2020/03~01121345",tz=cn)
#ymdhms任意组合

#从日期组件创建日期时间
make_date(2020,8,27)
make_datetime(2020,8,27,21,27,15)

#格式化输出日期时间
d=make_date(2020,3,5)
format(d,"%Y/%m/%d")#以/分割
t=make_datetime(2020,3,5,21,7,15)
fmt=stamp("Create on Monday, Feb 3,1999 3:32 上午")#根据制定模板输出
fmt(t)

#提取日期时间数据的组件
t=ymd_hms("2020/08/27 21:30:27")
year(t)
quarter(t)#季度
month(t)
week(t)
day(t)
yday(t)#当年的第几天
hour(t)
minute(t)
second(t)
weekdays(t)
wday(t)#一周内的第几天,默认周日为第一天
wday(t,label=TRUE)#因子型表示本周第几天
tz(t)
with_tz(t,tz="America/New_York")#转换时区
force_tz(t,tz="America/New_York")#强制转换时区,时间不变
round_date(t,unit="hour")#四舍五入至小时
floor_date(t,unit="hour")#向下取整
ceiling_date(t,unit="hour")#向上取整
rollback(t,roll_to_first = TRUE,preserve_hms = TRUE)#回滚到上月最后一天或本月第一天

#时间段数据
begin=ymd_hm("2019/08/10 14:00")
end=ymd_hm("2020-03-05 18:15")
gap=interval(begin,end)#返回时间段数据
time_length(gap,"day")
time_length(gap,"minute")
t %within% gap #判断t是否属于该时间段
duration(100,units="day")#以数值＋时间单位储存时段长度
as.duration(gap)
period(100,unit="day")
#duration和period的区别：duration 是基于数值线，不考虑闰年和闰秒；period 是基于时间线，考虑闰年和闰秒。
#比如，duration 中的 1 年总是 365.25 天，而 period 的平年 365 天闰年 366 天。
#period 时间段：years(), months(), weeks(), days(), hours(), minutes(), seconds();
#duration 时间段：dyears(), dmonths(), dweeks(), ddays(), dhours(), dminutes(), dseconds()

#日期的时间计算
t+int#时间点+时间段生成一个新的时间点
leap_year(2020)#判断是否为闰年
ymd(20190305)+years(1)#加period的一年
ymd(20190305)+dyears(1)#加duration的一年
t+weeks(1:3)
gap/ddays(1)#等同time_length(gap,"day")
gap %/% ddays(1)#整除
gap %% ddays(1)#余数
as.period(gap %% ddays(1))
#月份加运算，表示日期按照月数增加
date=as_date("2019-01-01")
date %m+% months(0:11)#生成每月同一天数据
x=seq.Date(as_date("2019-08-02"),by="years",length.out = 3)
pretty_dates(x,12)#生成近似的时间刻度

# 时间序列
ts(data=1:10,start=2010,end=2019,frequency=4)#frequency=1年数据/4季度数据/12月数据/52周数据/365日数据
load("datas/stocks.rda")
library(fpp3)
stocks=as_tsibble(stocks,key=Stock,index=Date)#key分组索引,index时间索引
x=stocks%>%
  group_by_key()%>%
  index_by(months=~yearmonth(.))%>%
  summarise(mean_week=mean(Close))#取月度均值
autoplot(stocks)#自动画时间为横轴的折线图

#正则表达式(太难了,暂时放放)

#控制结构
#分支结构
#一个分支
if(条件){
  执行体
}
#两个分支
if(条件){
  执行体1
}else{
  执行体2
}
#多个分支
if(条件1){
  执行体1
}else if(条件2){
  执行体2
}else{
  执行体n
}
x="b"
switch(x,"a"="apple","b"="banana","c"="cherry")
#条件必须是逻辑值，多个逻辑值可用：
any()
all()

ifelse(条件,TRUE时的取值,FALSE时的取值)

#循环结构
#for循环
df=as_tibble(iris[,1:4])
output=vector("double",4) #输出(分配储存空间)
for(i in 4){              #迭代器
  output[i]=mean(df[[i]]) #循环体
}
#循环模式
for(i in seq_along(xs))#根据数值索引,迭代中使用x[i]
for(x in xs)#根据元素值索引,迭代中使用x
for(nm in names(xs)){}#根据名字索引,迭代中使用x[nm]
#最常用数值索引,因为名字和元素都可以根据索引提取
for(i in seq_along(x)){
  name=names(x)[i]
  value=x[i]
}
#将每次循环结果合并为一个整体对象
output=NULL
for(i in 1:3){
  output[[i]]=c(i,i^2)
}
unlist(output)
flatten_dbl(output)

#while循环(用于迭代次数未知)
while(condition){
  #循环体
}

set.seed(100)#设置随机种子,使结果可重现
while(TRUE){
  x=rnorm(1)
  print(x)
  if(x>1) break
}

#repeat循环
repeat{
  #循环体
  if(退出条件)break
}

s=1.0
x=1
k=0
repeat{
  k=k+1
  x=x/k
  s=s+x
  if(x<1e-100)break
}
str_glue("迭代{k}次,得到e={s}")

#apply族函数 apply(x,MARGIN,FUN,...)
x=matrix(1:6,ncol=3)
apply(x,1,mean)#按行求平均值
apply(x,2,mean)#按列求平均值

#tapply按照因子分组统计
height=c(165,170,168,172,159)
sex=factor(c("男","女","男","男","女"))
tapply(height,sex,mean)

#lapply对vector list data.frame逐元、逐成分、逐列应用函数
lapply(df,mean)#输出为list

#sapply lapply的简化版 simplify=TRUE输出简化的向量矩阵
sapply(df,mean,simplify = TRUE)#输出为vector


#purrr泛函式编程
#一元函数
map_chr(.x,.f)#返回字符型变量
map_lgl(.x,.f)#返回逻辑型变量
map_dbl(.x,.f)#返回实数型变量
map_int(.x,.f)#返回整数型变量
map_dfr(.x,.f)#返回数据框列表,再bind_rows按行合并为一个数据框
map_dfc(.x,.f)#返回数据框列表,再bind_cols按列合并为一个数据框

map_dbl(df,mean)
map_dbl(df,mean,na.rm=TRUE)
map_dbl(df,~mean(.x,na.rm=TRUE))

#对于只接受标量的一元函数,无需改造原函数即可支持向量输入
map_dbl(xs,.f)#xs表示若干个x构成的序列

#二元函数
height=c(1.58,1.76,1.64)
weight=c(52,73,68)
map2_dbl(height,weight,~.y/.x^2)

#多元函数,在数据框的每一行中进行迭代
df=tibble(
  n=c(1,3,5),
  mean=c(5,10,-3),
  sd=c(1,5,10)
)
set.seed(1)
pmap(df,rnorm)#此处df中的列名必须与rnorm中的参数同名
pmap(df,~rnorm(..1,..2,..3))
pmap(df,~rnorm(...))#功能同上，但无列名要求

pmap_dbl(df,~mean(c(...)))#按行求均值
pmap_chr(df,str_c,sep="/")

#自定义函数
Score_Conv=function(score){
  if(score>=90){
    res="优"
  }else if(score>=80){
    res="良"
  }else if(score>=70){
    res="中"
  }else if(score>=60){
    res="及格"
  }else{
    res="不及格"
  }
  res
}
#向量化改进
#1.修改自定义函数
Score_Conv2=function(score){
  n=length(score)
  res=vector("character",n)
  for(i in 1:n){
  if(score[i]>=90){
    res[i]="优"
  }else if(score[i]>=80){
    res[i]="良"
  }else if(score[i]>=70){
    res[i]="中"
  }else if(score[i]>=60){
    res[i]="及格"
  }else{
    res[i]="不及格"
  }}
  res
}
scores=c(35,67,100)
Score_Conv2(scores)
#2.借用apply or map族函数
sapply(scores,Score_Conv)
map_chr(scores,Score_Conv)

#处理多个返回值,将输出放入一个列表
MeanStd=function(x){
  mu=mean(x)
  std=sqrt(sum((x-mu)^2)/(length(x)-1))
  list(mu=mu,std=std)
}
MeanStd(c(2,6,4,8,12))

#默认参数值
MeanStd2=function(x,type=1){
  mu=mean(x)
  n=length(x)
  if(type==1){
    std=sqrt(sum((x-mu)^2)/(n-1))
  }else{
  std=sqrt(sum((x-mu)^2)/n)
  }
  list(mu=mu,std=std)
}
MeanStd2(c(2,6,4,8,12),2)
#用type=1表意不明确，可用字符串代替
MeanStd3=function(x,type="sample"){
  mu=mean(x)
  n=length(x)
  switch(type,
         "sample"={
           std=sqrt(sum((x-mu)^2)/(n-1))
         },
         "population"={
           std=sqrt(sum((x-mu)^2)/n)
         })
  list(mu=mu,std=std)
}
MeanStd3(c(2,6,4,8,12),"population")
#“...”参数
my_sum=function(x,y){
  sum(x,y)
}
my_sum(1,2)#只能传入两个参数，无法计算三个及以上的加和
dots_sum=function(...){
  sum(...)
}
dots_sum(1,2,3,4,5,6)#可以输入任意多个参数

#R自带函数
#1.基本数学函数
round(1.234555,3)#四舍五入保留n位小数
signif(1.234555,3)#四舍五入保留n位有效数字
ceiling(pi)#向上取整
floor(pi)#向下取整
sign(-4:7)#符号函数，负数输出-1，0为0，正数为1
abs(-1)#取绝对值
sqrt(4.4232)#取平方根
exp(5.21)#e的x次幂
log(13.4976,3)
log2(12.421)
log10(100)
Re(4+1i)#返回实部
Im(9.31+24.4i)#返回虚部
Mod(9.31+24.4i)#复数的模
Arg(9.31+24.4i)#复数的辐角
Conj(9.31+24.4i)#共轭复数
#2.三角函数与双曲函数
sin(1)
cos(1)
tan(1)
asin(1)
acos(1)
atan(1)
sinh(1)
cosh(1)
tanh(1)
asinh(1)
acosh(1)
atanh(0.35)
#3.矩阵函数
a=matrix(50:70,nrow=5)
b=matrix(1:20,nrow=5)
nrow(a)
ncol(a)
dim(a)#维数
colSums(a)#对各列求和
rowSums(a)#对各行求和
colMeans(a)
rowMeans(a)
t(a)#转置
det(a)#方阵的行列式
crossprod(a,b)#计算a和b的内积
outer(a,b)#叉积
solve(b)#逆矩阵
solve(a,b)#解ax=b
ginv(a)#求广义逆
eigen(a)#返回特征值
keonecker(a,b)#Kroneker积
svd(a)#奇异值分解
qr(a)#QR分解
chol(a)#Choleski分解
a[upper.tri(a)]#上三角矩阵
a[lower.tri(a)]#下三角矩阵
#4.概率函数
factorial(3.53)
choose(12.21,4)
gamma(31)
beta(12,42)
combn(c("甲","乙","丙"),2)#生成x中任意取m个元的所有组合
#d=密度函数
#p=分布函数
#q=分位数函数
#r=生成随机数
dnorm(3,0,2)#正态分布N(0,4)在3处的密度值
pnorm(3,0,2)#正态分布N(0,4)在3处的函数值
1-sum(dbinom(0:1,400,0.02))#命中率为0.02,独立射击400次,至少击中2次的概率
pnorm(2,1,2)-pnorm(0,1,2)#x~N(0,1),求P{0<x≤2}
qnorm(1-0.25,0,1)#N(0,1)的上0.25分位数
set.seed(1)
rnorm(5,0,1)#生成5个服从N(0,1)分布的随机数
#随机抽样
set.seed(2)
sample(c("正","反"),10,replace=TRUE)#抛10次硬币,可重复抽样
sample(1:10,10,replace=FALSE)
#5.统计函数
min(x)
cummin(x)#累计最小值
max(x)
cummax(x)#累计最大值
range(x)#x的范围
sum(x)
cumsum(x)#累计和
prod(x)#求积
cumprod(x)#累计积
mean(x)
median(x)
x=rnorm(1000000000,0,1)
quantile(x,0.90)#求分位数,pr为概率
sd(x)#标准差
var(x)#方差
cov(x)#协方差
cor(x)#相关系数
scale(x,center=TRUE,scale=FALSE)#对数据做中心化,减去均值
scale(x,center=TRUE,scale=TRUE)#对数据做标准化
#自定义极差标准化函数
rescale=function(x,type="正向指标"){
  rng=range(x,na.rm=TRUE)
  switch(type,
         "正向指标"={
           (x - rng[1]) / (rng[2] - rng[1])
         },
         "负向指标"={
           (rng[2] - x) / (rng[2] - rng[1])
         })
}
x=c(1,2,3,NA,5)
rescale(x,"负向指标")

#时间序列函数
x=ts(1:8,frequency = 4,start = 2015)
stats::lag(x,8)#避免被dplyr::lag覆盖,计算时间滞后
x=c(1,3,6,8,10)
diff(x,differences = 1)#一阶差分
diff(x,differences=2)#二阶差分
diff(x,lag=2,differences = 1)#间隔两年求一阶差分

#其他函数
unique(x)#去除重复元素
duplicated(c(1,1,2,1,2,1))#是否是重复值
anyDuplicated(c(1,2,3,2,1))#一个个重复元素的索引
rle(c(1,1,1,1,1,2,2,2,1,2,1))#统计向量中连续相同值的长度
inverse.rle(x)#rle(x)的反函数

#数据操作
#管道操作
mtcars %>% 
  group_by(cyl) %>%  
  summarise(mpg_avg=mean(mpg))
c(1,3,4,5) %>% 
  plot(main=str_c(.,collapse=","))
mtcars %>% plot(mpg~disp,data=.)
iris %>% .$Species
iris %>% .[1:3]
mtcars %>% 
  group_split(cyl) %>% 
  map(.,~lm(mpg~wt,data=.x))#或省略成map(~lm(mpg~wt,data=.))
#建议区分.用于管道操作中代替数据,.x用于purrr匿名函数
 
#数据读写
#readr包,读写带有分隔符的文本文件
read_csv()
read_tsv()#读入数据到数据框
read_csv2()
read_tsv2()#读入欧式格式数据
read_rds()
write_rds()#读写rds数据
write_csv()
write_tsv()#写出到文件
parse_number()
parse_logical()
parse_factor()#转化数据类型

#readxl包
read_excel()#自动检测xls/xlsx文件
read_xls()
read_xlsx()

#haven包,读写SPSS,Stata,SAS数据
read_spss()
read_dta()
read_sas()
write_sas()

#jsonlite包,读写JSON数据,与R数据结构互换
read_json()
fromJSON()
write_json()
toJSON()

#readtext包,读写文本文件的所有内容,每个文件变为一行
readtext()
document=readtext("datas/十年一觉.txt")

#实例
df=read_csv("datas/六1班学生成绩.csv")
files=fs::dir_ls("datas/read_datas",recurse = T,glob = "*.xlsx")#fs包,获取路径
library(readxl)
df=map_dfr(files,read_excel)
head(df)#看前六个
df=map_dfr(files,read_excel,.id="来源")#标记来自哪个文件
head(df)
set_names(files)#files 是文件路径构成的字符向量（未命名，只有索引访问），set_names(files) 是将该字符向量，变成命名字符向量，名字就用元素值
df=map_dfr(set_names(files),read_xlsx,sheet=1,.id="来源")
df=map_dfr(set_names(files),~read_xlsx(.x,sheet=1),.id="来源")

#批量读取+合并csv
files=fs::dir_ls("datas/read_datas",recurse=TRUE,glob = "*.csv")
df=read_csv(files)

#写出到excel
library(writexl)
write_xlsx(df,"datas/output_file.xlsx")

#批量写出到excel
df=iris %>% 
  group_split(Species)
files=str_c("datas/",levels(iris$Species),".xlsx")#准备文件名
walk2(df,files,write_xlsx)#写出到多个文件

df=df %>% 
  set_names(levels(iris$Species))
write_xlsx(df,"datas/iris.xlsx")#写出到同个文件的不同sheet

#保存与载入rds数据
save()
load()
write_rds(iris,"my_iris.rds")
dat=read_rds("my_iris.rds")

#连接数据库（略）
#中文编码（略）

#数据连接
load("datas/planes.rda")
planes %>% 
  count(tailnum) %>% 
  filter(n>1)#查找键列(即可以唯一识别某行的值,查找有无重复)

load("datas/weather.rda")
weather %>% 
  count(year,month,day,hour,origin) %>% 
  filter(n>1)

#合并行和合并列
x=bind_rows(
  slice_sample(iris,n=2),
  slice_sample(iris,n=2),
  slice_sample(iris,n=2)
)

one=mtcars[1:4,1:3]
two=mtcars[1:4,4:5]
bind_cols(one,two)

#根据数据值合并数据框
left_join(x,y,by)#左连接,保留x的所有行,合并匹配y中的列
right_join(x,y,by)#右连接,保留y的所有行,合并匹配x中的列
full_join(x,y,by)#全连接,保留所有行,合并匹配x和y中的列
inner_join(x,y,by)#内连接,保留两表中共有的观测
semi_join(x,y,by)#半连接,根据在y中，筛选x的行
anti_join(x,y,by)#反连接,根据不在y中，筛选x的行
#实例
band=band_members
instrument=band_instruments

#左连接
band %>% 
  left_join(instrument,by="name")
#若两表中列名不同
band %>% 
  left_join(instrument,by=c("name1"="name2"))
#多个键列匹配
band %>% 
  left_join(instrument,by=c("name1","name2"))
#右连接
band %>% 
  right_join(instrument,by="name")
#全连接
band %>% 
  full_join(instrument,by="name")
#内连接
band %>% 
  inner_join(instrument,by="name")
#半连接
band %>% 
  semi_join(instrument,by="name")
#反连接
band %>% 
  anti_join(instrument,by="name")

#批量连接
files=list.files("datas/achieves/",pattern = "xlsx",full.names = TRUE)
map(files,read_xlsx) %>% 
  reduce(full_join,by="人名")
#若在一个文件的多个sheet中
path="datas/3-5月业绩.xlsx"
map(excel_sheets(path),~read_excel(path,sheet=.x)) %>% 
  reduce(full_join,by="人名")

#集合运算
intersect(x,y)#返回x和y共同包含的观测
union(x,y)#返回x和y中所有的（唯一）观测
setdiff(x,y)#返回在x中但不在y中的观测

#数据重塑
dt = tribble(
  ~observation, ~A_count, ~B_count, ~A_dbh, ~B_dbh,
  "Richmond(Sam)", 7, 2, 100, 110,
  "Windsor(Ash)", 10, 5, 80, 87,
  "Bilpin(Jules)", 5, 8, 95, 90)
knitr::kable(dt, align="c")

tidy_dt=dt %>% 
  pivot_longer(-observation,
               names_to = c("speices",".value"),
               names_sep = c("_")) %>% 
  separate(observation, into = c("site", "surveyor"))
knitr::kable(tidy_dt, align="c")

#宽表变长表
#每行只有1个观测的情形
df=read_csv("datas/分省年度GDP.csv")#每个地区每年的值应该作为一个观测
df %>% 
  pivot_longer(-地区,names_to = "年份",values_to = "GDP")

#每一行有多个观测的情形
load("datas/family.rda")
knitr::kable(family, align="c")
family %>% 
  pivot_longer(-family,
               names_to = c(".value","child"),
               names_sep = "_",
               values_drop_na = TRUE)

df=read_csv("datas/参赛队信息.csv")
df %>% 
  pivot_longer(everything(),
               names_to = c("队员",".value"),
               names_pattern = "(.*\\d)(.*)")

#长表变宽表
#只有一个列名和一个列值
load("datas/animals.rda")
#用names_from指定列名来源;values_from制定值的来源
animals %>% 
  pivot_wider(names_from = Type,values_from = Heads,values_fill = 0)

#多个列名和多个值的情况
x=us_rent_income %>% 
  pivot_wider(names_from = variable,values_from = c(estimate,moe))

#存在的问题
df = tibble(
  x = 1:6,
  y = c("A","A","B","B","C","C"),
  z = c(2.13,3.65,1.88,2.30,6.55,4.21))
#想让y列提供变量名,z提供值
df %>% 
  pivot_wider(names_from = y,values_from = z)
#由于x列的存在,无法压缩,只能填充,因此长变宽的时候不能带有类似x这种唯一识别各行的ID列
#但是去掉x后又会遇到第二个问题
df=df[-1]
df %>% 
  pivot_wider(names_from = y,values_from = z)
#值不能唯一识别,结果变成了列表列,因此可以添加一个识别符
df=df %>% 
  group_by(y) %>% 
  mutate(n=row_number())
df %>% 
  pivot_wider(names_from = y,values_from = z)

#实例
contacts = tribble( ~field, ~value,
                    " 姓名", " 张三",
                    " 公司", " 百度",
                    " 姓名", " 李四",
                    " 公司", " 腾讯",
                    "Email", "Lisi@163.com",
                    " 姓名", " 王五")
contacts = contacts %>%
  mutate(ID = cumsum(field == " 姓名"))
contacts %>% 
  pivot_wider(names_from = field,values_from = value,values_fill = "暂无")

#拆分列
table3 %>% 
  separate(rate,into = c("cases","population"),sep="/",
           convert = T)#转化为数值型
#拆分不定长列
df = tibble(Class = c("1 班", "2 班"),
            Name = c("张三，李四，王五", "赵六，钱七"))
df1=df %>% 
  separate_rows(Name,sep = "，")
#逆操作
df1 %>% 
  group_by(Class) %>% 
  summarize(Name=str_c(Name,collapse = "，"))

#合并列
table5 %>% 
  unite(birth,century,year,sep="")

#综合示例
pop2=world_bank_pop %>% 
  pivot_longer("2000":"2017",names_to = "year",values_to = "value")
pop3=pop2 %>% 
  separate(indicator,c(NA,"area","variable"))
pop4=pop3 %>% 
  pivot_wider(names_from = variable,values_from = value)

#方形化,将一个深度嵌套的列表驯服成一个整齐的行和列的数据集
library(repurrrsive)
chars=tibble(char=got_chars)
#方法1
chars1=chars %>% 
  unnest_wider(char)#纵向展开，提取每个元按列存放
chars1 %>% 
  select(name,title=titles) %>% 
  unnest_longer(title)#横向展开，提取每个元按行存放
#方法2
chars %>% 
  hoist(char,name="name",title="titles") %>% #从内层提取想要的列
  unnest_longer(title)

#数据操作
library(tidyverse)
library(readxl)
df = read_xlsx("datas/ExamDatas_NAs.xlsx")
#选择列
#用列名或索引选择
df %>% 
  select(name,sex,math)
#用运算符选择列
df %>% select(1:3)#选择连续若干列
df %>% select(!2:5)#反选
df %>% select(2:3|6)#选择并集
df %>% select(3:7&1:4)#选择交集
df %>% select(c(3:7&1:4,!2:5))#合并多个选择
#选择助手函数
#选择指定列
df %>% select(everything())#选择所有列
df %>% select(last_col(5))#选择倒数第6列,空缺为选择最后一列
#选择列名匹配的列
df %>% select(starts_with("n"))#选择以某前缀开头的列
df %>% select(ends_with("e"))#选择以某后缀结尾的列
df %>% select(contains("a"))#选择包含某字符串的列
df %>% select(matches("m.*a"))#选择匹配正则表达式的列
#结合函数选择列
df %>% select(where(is.numeric))
df[,4:8] %>% 
  select(where(~sum(.x,na.rm=TRUE)>3000))#支持purrr写法
df %>%  
  select(where(~n_distinct(.x)<10))#选择唯一值数目<10的列

#用-删除列
df %>% 
  select(-c(name,chinese,science))
df %>% 
  select(math,everything(),-ends_with("e"))

#调整列的顺序,按选择的顺序排列
df %>% 
  select(ends_with("e"),math,name,class,sex)
#everything()返回未被选择的所有列,将某列移至第一列时很有用
df %>% 
  select(math,everything())
#用relocate函数将选择的列移动到某列前或后
df %>% 
  relocate(where(is.numeric),.after = name)

#重命名列
df %>% 
  set_names("班级","姓名","性别","语文",
            "数学","英语","品德","科学")#为所有列设置列明
df %>% 
  rename(数学=math,科学=science)#重命名部分列,新名=旧名
df %>% 
  rename_with(~paste0("new_",.x),matches("m"))#m开头的列名加上new_

#across()函数,同时对所选择的多列应用若干函数
across(.cols = everything(),.fns = NULL,.names)
#.fns可以为NULL、一个函数、一个purrr的匿名函数或多个函数组成的列表
across(everything(),.fns)#可以替代后缀_all
across(where(),.fns)#可以替代后缀_if
across(.cols,.fns)#可以替代后缀_at

#修改列
#创建新列
df %>%
  mutate(new_col=5)
df %>% 
  mutate(new_col=1:n())
#计算新列
df %>% 
  mutate(total=chinese+math+english+moral+science)
df %>% 
  mutate(med=median(math,na.rm = TRUE),
         label=math>med,
         label=as.numeric(label))
#修改多列
#应用函数到所有列
df %>% 
  mutate(across(everything(),as.character))
#应用函数到满足条件的列
rescale = function(x) {
  rng = range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
df %>% 
  mutate(across(where(is.numeric),rescale))
#应用函数到指定列
as_tibble(iris) %>% 
  mutate(across(contains("length")|contains("width"),~.x*10))

#替换NA
starwars %>% 
  replace_na(list(hair_color = "UNKNOWN",
                  height = mean(.$height,na.rm = TRUE)))
#用上一个或下一个非缺失值填充
load("datas/gap_data.rda")
gap_data %>% 
  fill(site,species)

#重新编码
#两种情形
df %>% 
  mutate(sex=if_else(sex=="男","M","F"))
#多种情形
df %>% 
  mutate(math=case_when(math>=75~"High",
                        math>=60~"Middle",
                        TRUE~"Low"))
#更强大的重编码函数
library(sjmisc)
df %>% 
  rec(math,rec="min:59=不及格;60:74=及格;75:85=良;85:max=优",
      append=FALSE) %>% 
  frq()#频数表

#筛选行
set.seed(123)
df_dup = df %>%
  slice_sample(n = 60, replace = TRUE)
#filter()根据条件筛选
df_dup %>% 
  filter(sex=="男",math>80)
df_dup %>% 
  filter(sex=="女",(is.na(english)|math>80))
df_dup %>% 
  filter(between(math,70,89))#闭区间
#在限定列范围内根据条件筛选行
df %>% 
  filter(if_all(4:6,~.x>75))
df %>% 
  filter(if_all(everything(),~!is.na(.x)))#筛选出所有值都不是NA的行
#在限定列的范围内,筛选"存在值满足某条件的行"
starwars %>% 
  filter(if_any(everything(), ~ str_detect(.x, "bl")))
df %>% 
  filter(if_any(where(is.numeric),~.x>90))
df %>% 
  filter(if_any(where(is.character),is.na))

#对行切片
slice(df, 3:7) # 选择 3-7 行
slice_head(df, n, prop) # 从前面开始选择若干行
slice_tail(df, n, prop) # 从后面开始选择若干行
slice_min(df, order_by, n, prop) # 根据 order_by 选择最小的若干行
slice_max(df, order_by, n, prop) # 根据 order_by 选择最大的若干行
slice_sample(df, n, prop) # 随机选择若干行
df %>% 
  slice_max(math,n=5)

#删除行
#删除重复行
df_dup %>% 
  distinct()
df_dup %>% 
 distinct(sex,math,.keep_all = TRUE)#只根据sex和math判断
#默认只返回选择的列，若要返回所有列，.keep_all = TRUE
#删除包含NA的行
df_dup %>% 
  drop_na()
df_dup %>% 
  drop_na(sex:math)#删除某些列包含NA的行
df_dup %>% 
  filter(!if_all(where(is.numeric),is.na))

#对行排序
df_dup %>% 
  arrange(math,sex)
df_dup %>% 
  arrange(-math,sex)#递减排序

#创建分组
df_grp=df %>% 
  group_by(sex)
group_keys(df_grp)#分组键值
group_indices(df_grp)#查看每一行属于哪一分组
group_rows(df_grp)#查看每一组包括哪些行
ungroup(df_grp)#解除分组

#其他分组函数
df_dup %>% 
  group_split(sex)#生成一个列表，其中包含分组的数据框
df_dup %>% 
  group_nest(sex)#先分组后嵌套，生成嵌套数据框
#purrr风格分组迭代，将函数.f分别应用到每个分组上
df_dup %>% 
  group_by(sex) %>% 
  group_map(~head(.x,2))#提取每组的前两行

#分组修改
load("datas/stocks.rda")
stocks %>% 
  group_by(Stock) %>% 
  mutate(delta=Close-lag(Close))#分别计算每个公司每天收盘的差价

#分组汇总
#汇总函数
summarise() 
#可以与许多汇总函数连用
n()
n_distinct()#变量的不重复数目值有几个
sum()
max()
min()
mean()
median()
sd()
IQR()
#对某列汇总
df %>% 
  group_by(sex) %>% 
  summarise(n=n(),
            math_dst=n_distinct(math),
            math_avg=mean(math,na.rm=T),
            math_med=median(math,na.rm=T))
#对某些列做汇总
df %>% 
  group_by(sex,class) %>% 
  summarise(across(contains("h"),mean,na.rm=T))
#对所有列做汇总
df %>% 
  select(-name) %>% 
  group_by(class,sex) %>% 
  summarise(across(everything(),mean,na.rm=T))
#对满足条件的列做多种汇总
df_grp=df %>% 
  group_by(class) %>% 
  summarise(across(where(is.numeric),list(sum=sum,mean=mean,min=min),na.rm=T)) %>% 
  pivot_longer(-class,names_to=c("Vars",".value"),names_sep = "_")
#支持多返回值的汇总函数
qs=c(0.25,0.5,0.75)
df_q=df %>% 
  group_by(sex) %>% 
  summarise(math_qs=quantile(math,qs,na.rm=T),q=qs) %>% 
  pivot_wider(names_from = q,values_from = math_qs,names_prefix = "q_")

#分组计数
df %>% 
  count(class,sex,sort=T)
#对已经分组的数据框用tally()计数
df %>%
  group_by(math_level=cut(math,breaks=c(0,60,75,80,100),right=F)) %>% 
  tally()

#增加一列按分组变量的计数
df %>% 
  add_count(class,sex)

#按行汇总
df = readxl::read_xlsx("datas/ExamDatas.xlsx")
rf=df %>% 
  rowwise()
rf %>% 
  mutate(total=sum(chinese,math,english))
#按行方式c_across()汇总
rf %>% 
  mutate(total=sum(c_across(where(is.numeric))))
df %>% 
  mutate(total=rowSums(across(where(is.numeric))))

df %>% 
  rowwise(name) %>% 
  summarise(total=sum(c_across(where(is.numeric))))

#逐行迭代总结
iris[1:4] %>% 
  mutate(avg=apply(.,1,mean))#apply,1表示行
iris[1:4] %>% 
  rowwise() %>% 
  mutate(avg=mean(c_across()))#rowwise,速度较慢
iris[1:4] %>% 
  mutate(avg=pmap_dbl(.,~mean(c(...))))#pmap
iris[1:4] %>% 
  mutate(avg=map_dbl(asplit(.,1),mean))#aspilt逐行分割+map

#窗口函数
#汇总函数sum()、mean()接受n个输入，返回1个值
#窗口函数接受n个输入返回n个值

#排名和排序函数
df %>% 
  mutate(ranks=min_rank(desc(math))) %>% 
  arrange(ranks)

#移位函数
lag()#取前一个值
lead()#取后一个值
library(lubridate)
dt=tibble(
  day=as_date("2019-08-30")+c(0,4:6),
  wday=weekdays(day),
  sales=c(2,6,2,3),
  balance=c(30,25,-40,30)
)
dt %>% 
  mutate(sales_lag=lag(sales),sales_delta=sales-lag(sales))

#累计汇总
cumany(x)#选择第一个满足条件后的所有行
cumany(!x)#选择第一个不满足条件后的所有行
cumall(x)#选择所有行直到第一个不满足条件的行
cumall(!x)#选择所有行直到第一个满足条件的行

dt %>% 
  filter(cumany(balance<0))#选择第一次透支后的所有行
dt %>% 
  filter(cumall(!balance<0))#选择第一次透支前的所有行

#滑窗迭代
library(slider)
dt %>% 
  mutate(avg_3=slide_dbl(sales,mean,.before=1,.after=1))
dt %>% 
  mutate(avg_3=slide_index_dbl(sales,day,mean,.before=1,.after=1))

#整洁计算
#数据屏蔽与整洁选择
var_summary=function(data,var){
  data %>% 
    summarise(n=n(),mean=mean({{var}}))#数据屏蔽以便直接使用列名
}
mtcars %>% 
  group_by(cyl) %>% 
  var_summary(mpg)
#字符型向量作为数据变量
var_summary=function(data,var){
  data %>% 
    summarise(n=n(),mean=mean(.data[[var]]))#.data[[]]用于改变字符向量
}
mtcars %>% 
  group_by(cyl) %>% 
  var_summary("mpg")

mtcars[,9:10] %>% 
  names() %>% 
  map(~count(mtcars,.data[[.x]]))#.data[[]]用于列名循环

summarise_mean=function(data,var){
  data %>% 
    summarise(n=n(),across({{var}},mean))
}
mtcars %>% 
  group_by(cyl) %>% 
  summarise_mean(where(is.numeric))
#字符向量形式
vars=c("mpg","vs")
mtcars %>% select(all_of(vars))
mtcars %>% select(!all_of(vars))
#使用数据屏蔽或整洁选择同时修改列名
my_summarise=function(data,mean_var,sd_var){
  data %>% 
    summarise("mean_{{mean_var}}":=mean({{mean_var}}),
              "sd_{{sd_var}}":=mean({{sd_var}}))#注意这里的冒号
}
mtcars %>% 
  group_by(cyl) %>% 
  my_summarise(mpg,disp)

my_summarise=function(data,group_var,summarise_var){
  data %>% 
    group_by(across({{group_var}})) %>% 
    summarise(across({{summarise_var}},mean,.names="mean_{.col}"))
}
mtcars %>% 
  my_summarise(c(am,cyl),where(is.numeric))

#字符串列名的同时修改方法
var_summary=function(data,var){
  data %>% 
    summarise(n=n(),
              !!enquo(var):=mean(.data[[var]]))
}
mtcars %>% 
  group_by(cyl) %>% 
  var_summary("mpg")

var_summary=function(data,var){
  data %>% 
    summarise(n=n(),
              !!str_c("mean_",var):=mean(.data[[var]]))
}
mtcars %>% 
  group_by(cyl) %>% 
  var_summary("mpg")

#引用与反引用,enquo()自动引用,!!反引用
grouped_mean=function(data,summary_var,group_var){
  summary_var=enquo(summary_var)
  group_var=enquo(group_var)
  data %>% 
    group_by(!!group_var) %>% 
    summarise(mean = mean(!!summary_var))
}
grouped_mean(mtcars,mpg,cyl)
#修改结果列名
grouped_mean=function(data,summary_var,group_var){
  summary_var=enquo(summary_var)
  group_var=enquo(group_var)
  summary_nm=str_c("mean_",as_label(summary_var))
  group_nm = str_c("group_", as_label(group_var))
  data %>% 
    group_by(!!group_nm:=!!group_var) %>% 
    summarise(!!summary_nm:=mean(!!summary_var))
}
grouped_mean(mtcars,mpg,cyl)

#传递多个参数...
group_mean=function(.data,.summary_var,...){#...放最后,其他参数加.可以避免冲突
  summary_var=enquo(.summary_var)
  .data %>% 
    group_by(...) %>% #...无需引用和反引用即可工作
    summarise(mean=mean(!!summary_var))
}
group_mean(mtcars,disp,cyl,am)
#多个参数修改结果列名,...需要用enquos()和!!!
group_mean=function(.data,.summary_var,...){
  summary_var=enquo(.summary_var)
  group_vars=enquos(...,.named=T)
  summary_nm=str_c("avg_",as_label(summary_var))
  names(group_vars)=str_c("groups_",names(group_vars))#注意这里的names()
  .data %>% 
    group_by(!!!group_vars) %>% 
    summarise(!!summary_nm:=mean(!!summary_var))
}
group_mean(mtcars,disp,cyl,am)
#...也可以传递表达式
filter_fun=function(df,...){
  filter(df,...)
}
mtcars %>%
  filter_fun(mpg > 25 & disp > 90)

#自定义函数绘制散点图
scatter_plot=function(df,x_var,y_var){
  x_var=enquo(x_var)
  y_var=enquo(y_var)
  ggplot(data=df)+
    aes(x=!!x_var,y=!!y_var)+
    geom_point()+
    theme_bw()+
    theme(plot.title = element_text(lineheight = 1,face = "bold",hjust = 0.5))+
    geom_smooth()+
    ggtitle(str_c(as_label(y_var)," vs. ",as_label(x_var)))
}
scatter_plot(mtcars,disp,hp)

#data.table包
library(data.table)
dt[i,j,by]#i选择行,j操作列,根据by分组

#创建data.table
dt=data.table(
  x=1:2,
  y=c("A","b")
)
dt
as.data.table(mtcars)#转化成data.table
mtcars_=copy(mtcars)
setDT(mtcars_)#将原表修改为data.table

#引用语法,:=就地引用,DT2=copy(DT1)修改数据本身

#键和索引
setkey(dt,v1,v3)#设置键
setindex(dt,v1,v3)#设置索引

#特殊符号
.()#相当于list()
:=#按引用方式增加修改列
.N#行数
.SD#每个分组的数据子集,除by或keyby的列
.SDcols#与.SD连用选择其中的列
.BY#包含所有by分组变量的list
.I#整数向量seq_len(nrow())
.GRP#分组索引
.NGRP#分组数
.EACHI#用于by/keyby=.EACHI表示根据i表达式的每一行分组

#链式操作
DT[...][...][...]

#数据读取
fread("DT.csv")
fread("DT.csv",sep="\t")
#选择部分行读取
fread("DT.csv",select=c("v1","V4"))
fread("DT.csv",drop="V4",nrows=100)
#读取压缩文件
fread(cmd="unzip -cq myfile.zip")
fread("myfile.gz")
#批量读取
c("DT.csv","DT.csv") %>% 
  lapply(fread) %>% 
  rbindlist()#多个数据框按行合并
#数据连接
#左连接,保留x所有行,匹配y中的列,以下三种方式
y[x,on="v1"]#x为左表
y[x]#若v1是键
merge(x,y,all.x=T,by="v1")
#右连接,保留y所有行,匹配x中的列
merge(x,y,all.y=T,by="v1")
#全连接,保留所有行
merge(x,y,by="v1")
#半连接,根据在y中,来筛选x的行
x[y$v1,on="v1",nomatch=0]
#反连接,根据不在y中,来筛选x的行
x[!y,on="v1"]
#集合运算
fintersect()
fsetdiff()
funion()
fsetequal()
#非等连接,不需要完全一致才能匹配,而是按照一定的条件匹配
#滚动连接

#数据重塑
#宽变长
DT = fread("datas/分省年度GDP.csv", encoding = "UTF-8")
DT %>% 
  melt(measure=2:4,variable="年份",value="GDP")
#tidyr中的实现方法
DT %>% 
  pivot_longer(-地区,names_to = "年份",values_to = "GDP")
#每行存在多个观测的情况
load("datas/family.rda")
DT=as.data.table(family)
DT %>% 
  melt(measure=patterns("^dob","^gender"),value=c("dob","gender"),na.rm=T)

#长变宽
load("datas/animals.rda")
DT = as.data.table(animals)
DT %>% 
  dcast(Year~Type,value="Heads",fill=NA)
#tidyr中的实现方法
DT %>% 
  pivot_wider(names_from = Type,values_from = Heads,values_fill = NA)
#多个列名列和多个值列的情况
us_rent_income %>% 
  as.data.table() %>% 
  dcast(GEOID+NAME~variable,value=c("estimate","moe"))

#数据分割合并
#拆分列
DT=as.data.table(table3)
DT[,c("cases","population"):=tstrsplit(DT$rate,split="/")][,rate:=NULL]
#合并列
DT=as.data.table(table5)
DT[,years:=paste0(century,year)][,c("century","year"):=NULL]

#数据操作
#选择行
dt[3:4,]
dt[!3:7,]#根据索引
dt[v2>5,]
dt[v4 %chin% c("A","C")]
dt[v1==4&v4=="A"]#根据逻辑表达式
unique(dt)
unique(dt,by=c("v1","v4"))#删除重复行
na.omit(dt,cols=1:4)#删除包含NA的行
#行切片
dt[sample(.N,3)]#随机抽取3行
dt[sample(.N,.N*0.5)]#随机抽取50%的行
dt[frankv(-v1,ties.method = "dense")<2]#v1值最大的行
#其他
dt[v4 %like% "^B"]#v4值以B开头
dt[v2 %between% c(3,5)]#闭区间
dt[between(v2,3,5,incbounds = F)]#开区间
dt[v2 %inrange% list(-1:1,1:3)]#v2值属于多个区间的某个
dt[inrange(v2,-1:1,1:3,incbounds = T)]

#排序行
dt[order(v1)]#默认按v1从小到大
dt[order(-v1)]#从大到小
dt[order(v1,-v2)]
setorder(DT,v1,-v2) #按引用重新排序

#操作列
#选择一列或多列
#根据索引
dt[[3]]#返回向量
dt[,3]#返回data.table
#根据列名
dt[,.(v3)]
dt[,.(v2,v3,v4)]
dt[,v2:v4]
dt[,!c("v2","v3")]
#反引用列名，先构建列名向量,再反引用
cols=c("v2","v3")
dt[,..cols]
dt[,!..cols]
cols=paste0("v",1:3)#v1,v2,...
cols=union("v4",names(dt))#v4提到第一列
cols=grep("v",names(dt))#列名包含v
cols=grep("^(a)",names(dt))#列名以a开头
cols=grep("b$",names(dt))#以b结尾
cols=grep(".2",names(dt))#正则匹配.2列
cols=grep("v1|X",names(dt))
#调整列序
cols=rev(names(DT))
setcolorder(DT,cols)
#修改列名
setnames(DT,old,new)
#修改因子水平
DT[,setattr(sex,"levels",c("M","F"))]
#修改或增加一列
dt[,v1:=v1^2][]#加[]输出结果
dt[,v2:=log(v1)]
dt[,.(v2=log(v1),v3=v2+1)]#只保留新列,计算v3的v2是原始v2而非修改后的
dt[,c(v2,v3)=.(temp=log(v1),v3=temp+1)]#用新计算的v2
#增加多列
dt[,c("v6","v7"):=.(sqrt(v1),"x")]
dt[,':='(v6=sqrt(v1),v7="x")]
#同时修改多列
DT = readxl::read_xlsx("datas/ExamDatas.xlsx") %>%
  as.data.table()
#应用函数到所有列
DT[,lapply(.SD,as.character)]#.SD为每个分组的数据子集,除了by或keyby的列
#应用函数到满足条件的列
DT[,lapply(.SD,as.character),.SDcols=is.numeric]#.SDcols选择包含在.SD中的列
#应用函数到指定列
DT=as.data.frame.table(iris)
DT[,.SD*10,.SDcols=patterns("(Length)|(Width)")]



#可视化与建模技术
#数据
ggplot(data=mpg)#只提供数据,仅会创建一个空图形
#映射,即指明数据和图形的关联
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv))#color设定为分类变量即进行分组
#几何对象
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv))+
  geom_smooth()
#美学映射也可以放在几何对象中
ggplot(mpg,aes(displ,hwy))+
  geom_smooth(aes(color=drv))
ggplot(mpg,aes(displ,hwy))+
  geom_smooth(color="purple")#可以设置特定值,但不能在映射aes()中
#图层叠加
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))+#单独对图层设定
  geom_smooth()
ggplot(mpg,aes(displ,hwy,color=drv))+#对整体设定
  geom_point()+
  geom_smooth()
#分组映射
load("datas/ecostats.rda")
ggplot(ecostats,aes(Year,gdpPercap))+
  geom_line(aes(group=Region),alpha=0.2)+
  geom_smooth(se=F,size=1.2)
#标度,控制坐标轴刻度（图例）方案

#修改坐标轴刻度和标签
#连续坐标轴
ggplot(mpg, aes(displ, hwy)) +
  geom_point()+
  scale_y_continuous(breaks=seq(15,40,by=10),#breaks设置刻度位置
                     labels=c("一五","二五","三五"))#labels设置刻度标签
#离散坐标轴
ggplot(mpg, aes(x = drv)) +
  geom_bar()+
  scale_x_discrete(labels=c("4"="四驱","f"="前驱","r"="后驱"))

ggplot(head(economics,45),aes(date,uempmed / 100))+
  geom_line()+
  scale_x_date(date_breaks = "6 months",date_labels="%Y年%b")+
  scale_y_continuous(labels=scales::percent)

#修改坐标轴标签、图例名和图例位置
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))+
  labs(x="引擎大小(L)",y="高速燃油率(mpg)",color="驱动类型")+
  theme(legend.position = "top")
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))+
  xlab("引擎大小(L)")+ylab("高速燃油率(mpg)")+labs(color="驱动类型")+
  theme(legend.position = "top")

#设置坐标轴范围
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))+
  coord_cartesian(xlim=c(5,7),ylim=c(10,30))
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))+
  xlim(5,7)+ylim(10,30)

#变换坐标轴
load("datas/gapminder.rda")
p=ggplot(gapminder,aes(gdpPercap,lifeExp))+
  geom_point()+
  geom_smooth()
p+scale_x_continuous(labels=scales::dollar)
p+scale_x_log10(labels=scales::dollar)

#设置图形标题
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))+
  geom_smooth(se=F,color="orange")+
  labs(title = "燃油效率随引擎大小的变化图",
       subtitle = "两座车 (跑车) 因重量小而符合预期",
       caption = "数据来自 fueleconomy.gov")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=1))#标题居中,副标题居右

#设置fill,color颜色
#离散变量
ggplot(mpg,aes(displ,hwy,color=drv))+
  geom_point()+
  #手动设置颜色
  scale_color_manual("驱动方式",#修改图例名
                     values=c("red","blue","green"),
                     breaks=c("4","f","r"),
                     labels=c("四驱","前驱","后驱"))

ggplot(mpg,aes(x=class,fill=class))+
  geom_bar()+
  scale_fill_brewer(palette="Dark2")#Dark2调色板
#查看所有可用调色板
RColorBrewer::display.brewer.all()
#查看所有可用的颜色空间
colorspace::hcl_palettes(plot=T)

#连续变量
ggplot(mpg,aes(displ,hwy,color=hwy))+
  geom_point()+
  scale_color_gradient2(low="green",mid="blue",high="red")

ggplot(mpg,aes(displ,hwy,color=hwy))+
  geom_point()+
  scale_color_distiller(palette="Purples")

#添加文字标注
library(ggrepel)
best_in_class=mpg %>% 
  group_by(class) %>% 
  slice_max(hwy,n=1)
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_label_repel(data=best_in_class,aes(label=model))
ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  annotate(geom="text",x=6,y=40,
           label="引擎越大\n燃油效率越高!",size=4,color="red")

#统计变换、坐标系、位置调整
#统计变换
ggplot(mpg,aes(class,hwy))+
  geom_violin(trim=F,alpha=0.5,color="green")+
  stat_summary(fun=mean,
               fun.min=function(x){mean(x)-sd(x)},
               fun.max=function(x){mean(x)+sd(x)},
               geom="pointrange",color="red")
ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  stat_smooth(method="lm",
              formula=y~splines::bs(x,3))

#坐标系
ggplot(mpg,aes(class,hwy))+
  geom_boxplot()+
  coord_flip()#从竖直变为水平
ggplot(mpg,aes(class,fill=drv))+
  geom_bar()+
  coord_polar()#转换为极坐标


#位置调整
#条形图
ggplot(mpg,aes(class,fill=drv))+
  geom_bar(position=position_dodge(preserve="single"))#竖直堆叠改为水平堆叠
ggplot(mpg,aes(class,fill=drv))+
  geom_bar(position="dodge")

#散点图散点位置调整
ggplot(mpg,aes(displ,hwy))+
  geom_point(position="jitter")#增加随机噪声使散点不重合

#多张图排版
library(patchwork)
p1=ggplot(mpg,aes(displ,hwy))+
  geom_point()
p2=ggplot(mpg,aes(drv,displ))+
  geom_boxplot()
p3=ggplot(mpg,aes(drv))+
  geom_bar()
p1|(p2/p3)
p1+p2/(p3|p1)

#分面、主题、输出
#分面
#封装分面
ggplot(mpg,aes(displ,hwy))+
  geom_point(position="jitter")+
  facet_wrap(~drv,scales="free")#free单独坐标轴,fixed统一坐标轴

ggplot(mpg,aes(displ,hwy))+
  geom_point(position="jitter")+
  facet_wrap(~drv+cyl)
  
#网格分面
ggplot(mpg,aes(displ,hwy))+
  geom_point(position="jitter")+
  facet_grid(drv~cyl)

#主题
ggplot(mpg,aes(displ,hwy))+
  geom_point(position="jitter")+
  facet_wrap(~drv,scales="free")+
  theme_bw()

#输出
ggsave("test.tiff",width=6,height=6,dpi=1200)
#中文字体
library(showtext)
font_add("heiti","simhei.ttf")
font_add("kaiti","simkai.ttf")
showtext_auto()
ggplot(mpg,aes(displ,hwy,color=drv))+
  geom_point(position="jitter")+
  theme_bw()+
  theme(axis.title=element_text(family="heiti"),
        plot.title=element_text(family="kaiti"))+
  xlab("发动机排量(L)")+
  ylab("高速里程数(mpg)")+
  ggtitle("汽车发动机排量与高速里程数")+
  annotate("text",6,40,family="kaiti",size=8,
           label="测试中文字体",color="red")
ggsave("font_example.pdf",width=7,height=4)

df=mpg %>% 
  mutate(across(c(class,drv),as.factor)) %>% 
  count(class,drv,.drop=F)
df %>% 
  ggplot(aes(class,drv))+
  geom_tile(aes(fill=n))+
  geom_text(aes(label=n))+
  scale_fill_gradient(low="white",high="darkorange")


load("datas/phone_call.rda")
library(visNetwork)
visNetwork(nodes,edges)

#人口金字塔图
pops=read_csv("datas/hljPops.csv")%>% 
  mutate(Age=as_factor(Age)) %>% 
  pivot_longer(-Age,names_to = "性别",values_to="Pops")
ggplot(pops,aes(x=Age,y=ifelse(性别=="男",-Pops,Pops),fill=性别))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_y_continuous(labels=abs,limits=c(-200,200))+
  xlab("年龄")+ylab("人口数(万)")

#折线图和面积图
library(patchwork)
p1=ggplot(economics,aes(date,uempmed))+
  geom_line(color="blue")+
  theme_bw()
p2=ggplot(economics,aes(date,uempmed))+
  geom_area(color="red",fill="yellow")+
  theme_bw()
p1/p2

#饼图
piedat=mpg %>% 
  group_by(class) %>% 
  summarize(n=n(),labels=str_c(round(100*n/nrow(.),2),"%"))
ggplot(piedat,aes(x="",y=n,fill=class))+
  geom_bar(stat="identity",width=1)+
  coord_polar("y",start=0)+
  geom_text(aes(label=labels),position=position_stack(vjust=0.5))+
  theme_void()

#动态交互图
library(plotly)
p=gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(gdpPercap,lifeExp,size=pop,color=continent))+
  geom_point()+
  theme_bw()
ggplotly(p)

#动态散点图
library(gganimate)
load("datas/gapminder.rda")
anim=ggplot(gapminder,aes(gdpPercap,lifeExp,size=pop))+
  geom_point(aes(color=continent))+
  scale_x_log10()+
  labs(title="年份:{frame_time}",x="人均GDP",y="预期寿命")+
  transition_time(year)
library(gifski)
animate(anim,nframes=300,fps=24,end_pause=20,renderer=gifski_renderer())
anim_save("test.gif")


#整洁建模技术
#整洁模型结果
library(broom)
tidy()#模型系数估计及其统计量
glance()#模型诊断信息
augment()#增加预测值列、残差列
model=lm(mpg~wt,data=mtcars)
model %>% tidy()
model %>% glance()
model %>% augment(mtcars)
#根据模型信息绘图
model %>% augment() %>% 
  ggplot(aes(wt,mpg))+
  geom_point()+
  geom_line(aes(y=.fitted),color="red")+
  geom_segment(aes(xend=wt,yend=.fitted),color="blue")
#残差图
model %>% augment() %>% 
  ggplot(aes(x=wt,y=.resid))+
  geom_point()+
  geom_hline(yintercept=0,color="blue")

#辅助建模
#重抽样
resample()
#模型性能度量
#生成模型数据
#增加预测值列、残差列
library(modelr)
ex=resample_partition(mtcars,c(test=0.3,train=0.7))
mod=lm(mpg~wt,data=ex$train)
rmse(mod,ex$test)#求模型在测试集中的均方根误差

mod=lm(mpg~wt+cyl+vs,data=mtcars)
data_grid(mtcars,wt=seq_range(wt,10),cyl,vs) %>% 
  add_predictions(mod)

#交叉验证法示例
cv10=crossv_kfold(mtcars,10)
cv10 %>% 
  mutate(models=map(train,~lm(mpg~wt,data=.)),
         rmse=map2_dbl(models,test,rmse))

#批量建模
load("datas/ecostats.rda")
#利用map函数
by_region=ecostats %>% 
  group_nest(Region)#根据region建立嵌套数据框
by_region$data[[1]]#查看列表列的第一个元素的内容
unnest(by_region,data)#解除嵌套

by_region=by_region %>% 
  mutate(model=map(data,~lm(Consumption~gdpPercap,data=.x)))

library(modelr)
library(broom)
by_region %>%
  mutate(rmse=map2_dbl(model,data,rmse),          #方根误差
         rsq=map2_dbl(model,data,rsquare),        #R^2
         slope=map_dbl(model,~coef(.x)[[2]]),     #斜率
         pval=map_dbl(model,~glance(.x)$p.value)) #P值
by_region %>%
  mutate(results=map(model,tidy)) %>% #批量提取模型系数估计及其统计量
  select(Region,results) %>% 
  unnest(results)
         
by_region %>%
  mutate(results=map(model,glance)) %>% #批量提取模型诊断信息
  select(Region,results) %>% 
  unnest(results)

by_region %>%
  mutate(result=map(model,augment)) %>%
  select(Region,result) %>%
  unnest(result)

#利用rowwise
by_region=ecostats %>% 
  nest_by(Region)#建立rowwise化的嵌套数据框
by_region=by_region %>% 
  mutate(model=list(lm(Consumption~gdpPercap,data)))
by_region %>% 
  mutate(rmse=rmse(model,data),
         rsq=rsquare(model,data),
         slope=coef(model)[[2]],
         pval=glance(model)$p.value)
by_region %>% 
  summarise(tidy(model))
by_region %>% 
  summarise(glance(model))
by_region %>% 
  summarise(augment(model))

#滚动回归
library(lubridate)
library(slider)
load("datas/stocks.rda")
df=stocks %>% 
  pivot_wider(names_from=Stock,values_from=Close) %>% 
  mutate(season=quarter(Date))
df %>% 
  ggplot(aes(Amazon,Google))+
  geom_point(color="blue",size=1.1)#大致符合线性关系
df_roll=df %>% 
  group_by(season) %>% 
  mutate(models=slide(cur_data(),
                      ~lm(Google~Amazon,.x),
                      .before=2,.after=2,
                      .complete=T)) %>% 
  ungroup()
library(modelr)
library(broom)
df_roll %>% 
  filter(!map_lgl(models,is.null)) %>% 
  mutate(rsq=map_dbl(models,~glance(.x)$r.squared),
         sigma=map_dbl(models,~glance(.x)$sigma),
         slope=map_dbl(models,~tidy(.x)$estimate[2]))

#概率分布
tibble(
  x=seq(-4,4,length.out=100),
  "μ=0, σ=0.5"=dnorm(x,0,0.5),
  "μ=0, σ=1"=dnorm(x,0,1),
  "μ=0, σ=2"=dnorm(x,0,2),
  "μ=-2, σ=1"=dnorm(x,-2,1)
) %>% 
  pivot_longer(-x,names_to = "参数",values_to = 'p(x)') %>% 
  ggplot(aes(x,`p(x)`,color=参数))+
  geom_line()

#数据位置的统计量
mean(x)
median(x)
quantile(x,p)#计算p分位数
retatix::get_mode(x)#众数
#数据分散程度的统计量
max(x)-min(x)#极差
IQR()#四分位距
var()#方差
sd(x)#标准差
100*sd(x)/mean(x)#变异系数
#数据分布形状的统计量
library(datawizard)
skewness(x)#偏度,是否对称
kurtosis(x)#峰度,是否尖峰

#常见统计量分组汇总
iris %>% 
  group_by(Species) %>% 
  rstatix::get_summary_stats(type="full")
iris %>% 
  group_by(Species) %>% 
  dlookr::describe()

#统计图
#条形图
df=starwars %>% 
  mutate(skin_color=fct_lump(skin_color,n=5)) %>% #频数小于5的合并
  count(skin_color,sort=T) %>% 
  mutate(p=n/sum(n))
ggplot(df,aes(fct_reorder(skin_color,p),p))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="皮肤颜色",y="占比")+
  geom_text(aes(y=p+0.03,
            label=str_c(round(p*100,1),"%")),
            size=5,color="red")+
  coord_flip()
#直方图
set.seed(1)
df=tibble(heights=rnorm(10000,170,2.5))
ggplot(df,aes(x=heights))+
  geom_histogram(fill="steelblue",color="black",
                 binwidth = 0.5)+
  stat_function(
    fun=~dnorm(.x,mean=170,sd=2.5)*0.5*10000,color="red"#绘制密度曲线：放大条形宽度*样本数倍
  )
#箱线图
ggplot(mpg,aes(drv,hwy))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()
#均值线与误差棒图
my_summary=function(data,.summary_var,...){
  summary_var=enquo(.summary_var)
  data %>% 
    group_by(...) %>% 
    summarize(mean=mean(!!summary_var,na.rm=T),
              sd=sd(!!summary_var,na.rm=T)) %>% 
    mutate(se=sd/sqrt(n()))
}
df=my_summary(ToothGrowth,len,supp,dose)
pd=position_dodge(0.1)
ggplot(df,aes(dose,mean,color=supp,group=supp))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                color="black",width=0.1,position=pd)+
  geom_line(position=pd)+
  geom_point(position=pd,size=3,shape=21,fill="white")+
  xlab("剂量(mg)")+ylab("牙齿生长")+
  scale_color_hue(name="喂养类型",breaks=c("OJ","VC"),
                  labels=c("橘子汁","维生素C"),l=40)+
  scale_y_continuous(breaks=0:20*5)

#列联表
library(janitor)
#一维列联表
mpg %>% 
  tabyl(drv) %>% 
  adorn_totals("row") %>% #添加合计行
  adorn_pct_formatting() #设置百分比
#二维列联表
mpg %>% 
  tabyl(drv,cyl) %>% 
  adorn_percentages("col") %>% #添加列占比
  adorn_pct_formatting(digits = 2) %>% #设置百分比格式
  adorn_ns()#添加频数

#参数估计
#bootstrap法
library(tidymodels)#or library(infer)
specify()#设定感兴趣的变量或变量关系
hypothesise()#设定零假设
generate()#基于零假设生成数据
calculate()#计算统计量
visualize()#可视化

df=tibble(height = c(167,155,166,161,168,163,179,164,178,156,
             161,163,168,163,163,169,162,174,172,172))
mu=mean(df$height)#点估计,样本均值
se=sd(df$height)/sqrt(nrow(df))#标准误
#基于标准误的置信区间
c(mu-qnorm(1-0.05/2)*se,mu+qnorm(1-0.05/2)*se)
#基于bootstrap法的置信区间
library(infer)
boot_means=df %>% 
  specify(response=height) %>% 
  generate(reps=1000,type="bootstrap") %>% 
  calculate(stat="mean")#均值
boot_ci=boot_means %>% 
  get_ci(level=0.95,type="percentile")#置信区间
visualize(boot_means)+
  shade_ci(endpoints = boot_ci)

#最小二乘估计OLS
sales = tibble(
  cost = c(30,40,40,50,60,70,70,70,80,90),
  sale = c(143.5,192.2,204.7,266,318.2,457,333.8,312.1,
           386.4,503.9))
#绘制散点图
ggplot(sales,aes(cost,sale))+geom_point()
#一元线性回归
m=lm(sale~cost,sales)
sales1=sales[c(6,9),] %>% 
  mutate(p=predict(m,.))
ggplot(sales,aes(cost,sale))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_segment(aes(x=cost,y=sale,xend=cost,yend=p),
               data=sales1,linetype=2,color="red")

#非线性拟合
df = readxl::read_xlsx("datas/历年累计票房.xlsx") %>%
  mutate(年份 = 年份 - 2002)
p=ggplot(df,aes(年份,累计票房))+
  geom_point(color="red")
#logit变换获得较优的初始参数(对拟合效果影响很大)
lm.fit=lm(car::logit(累计票房/800)~年份,df)
coef(lm.fit)
log.fit=nls(累计票房~phi1/(1+exp(-(phi2+phi3*年份))),
            data=df,
            start=list(phi1=800,phi2=-5.145,phi3=0.391))
coefs=coef(log.fit)
logFit=function(x){
  coefs[1]/(1+exp(-(coefs[2]+coefs[3]*x)))
}
p+geom_function(fun=logFit,color="steelblue",size=1.2)

#最大似然估计MLE

#离散情形
#抛10次硬币,有3次正面,求概率
loglik=function(p){3*log(p)+7*log(1-p)}
library(maxLik)
m=maxLik(loglik,start=0.5)
coef(m)
stdEr(m)

#连续情形
loglik=function(theta){
  mu=theta[1]
  sigma=theta[2]
  n=nrow(mtcars)
  -n*log(sigma)-1/(2*sigma^2)*sum((mtcars$mpg-mu)^2)
}
m=maxLik(loglik,start=c(mu=30,sigma=10))
coef(m)
stdEr(m)
ggplot(mtcars,aes(mpg))+
  geom_histogram(binwidth = 1,fill="steelblue")+
  stat_function(fun=~dnorm(.x,mean=20.09,sd=5.93)*32,
                color="red",size=1.2)

#假设检验功效
library(pwr)
pwr.t.test(n=50,d=0.5,sig.level=0.05,alternative="greater")#计算功效
pwr.t.test(power=0.8,d=0.5,sig.level=0.05,alternative="greater")#计算样本量

#假设检验
library(rstatix)
#比较均值
t_test()
wilcox_test()#Wilcoxon检验
sign_test()#符号秩和检验
anova_test()
kruskal_test()#KW秩和检验
friedman_test()
#比较比例
prop_test()#z检验
fisher_test()
chisq_test()
binom_test()
multinom_test()
mcnemar_test()
cochran_qtest()
prop_trend_test()#趋势卡方
shapiro_test()
mshapiro_test()#正态性检验
levene_test()#方差齐性检验
cor_test()#相关性检验

#方差分析
library(rstatix)
df=ToothGrowth %>% 
  mutate(dose=factor(dose))
#1.正态性检验
shapiro_test(df,len)
#2.方差齐性检验
levene_test(df,len~supp)
levene_test(df,len~dose)
#3.两因素混合模型方差分析
anova_test(df,len~supp*dose)
#4.多重比较
tukey_hsd(df,len~supp*dose)G

#重复测量方差分析(一个个体测量多次)
x=df %>% 
  mutate(ID=rep(1:10,6)) %>% #10只小数每只重复测量6次
  anova_test(len~supp*dose+Error(ID/(supp*dose)))

#bruceR包中的
MANOVA(df,dv="len",between=c("supp","dose")) %>% 
  EMMEANS("supp",by="dose") %>% 
  EMMEANS("dose",by="supp")

#卡方检验
titanic=read_rds("datas/titanic.rds")
tbl=titanic %>% 
  janitor::tabyl(Survived,Pclass)#列联表
library(rstatix)
chisq_test(titanic$Survived,titanic$Pclass)
#多重比较
pairwise_prop_test(as.matrix(tbl[,-1]))


#基于重排的假设检验
#t检验
load("datas/movies_sample.rda")
#点估计
movies_sample %>% 
  group_by(genre) %>% 
  summarise(n=n(),avg_rat=mean(rating),sd_rat=sd(rating))
library(infer)
null_distribution=movies_sample %>% 
  specify(formula=rating~genre) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000,type="permute") %>% 
  calculate(stat="diff in means",
            order=c("Romance","Action"))
null_distribution %>% 
  get_p_value(obs_stat=tibble(stat=1.047),direction="both")
visualize(null_distribution,bins=15)+
  shade_p_value(obs_stat=tibble(stat=1.047),direction="both")

#多元线性回归实例
penguins=read_csv("datas/penguins.csv")
#1.探索因变量body_mass的分布
ggplot(penguins,aes(body_mass))+
  geom_histogram(bins=20,fill="steelblue",color="black")
#2.构建多元线性回归
mdl0=lm(body_mass~.,penguins)
#3.多重共线性诊断与逐步回归
car::vif(mdl0)#诊断,vif值大于10存在
mdl1=step(mdl0,direction="backward",trace=0)#逐步回归
confint(mdl1)#回归系数的置信区间
GLM_summary(mdl1)#bruceR包中的查看模型信息
library(modelr)
rmse(mdl1,penguins)#计算均方根误差
#4.分类变量的处理
table(penguins$species)
library(modelr)
model_matrix(penguins,~species-1)#species变量改为虚拟变量的效果
#三列直接存在线性相关,因此要去掉一个冗余列
model_matrix(penguins,~species)#默认是去掉第一个水平
penguins$species=relevel(penguins$species,ref="Gentoo")#把某个水平修改为第一水平后可以再去除
model_matrix(penguins,body_mass~.)#fomula填回归方程则返回真正纳入回归的改为虚拟变量的自变量信息
#5.模型改进
mdl2=lm(body_mass~species+sex*island+bill_length
        +I(bill_length^2)+bill_depth+I(bill_depth^2)
        +flipper_length+I(flipper_length^2),penguins) %>% #纳入三个数值变量的二次项和交互项sex:island
  step(direction="backward",trace=0)#逐步回归剔除
#检验模型差异
anova(mdl1,mdl2)#方差分析
lmtest::lrtest(mdl1,mdl2)#似然比检验
#6.回归诊断
library(ggfortify)
autoplot(mdl2,which=c(1:6))#绘制残差诊断,有6个图形可选
library(rstatix)
shapiro_test(mdl2$residuals)#残差正态性检验
library(lmtest)
dwtest(mdl2)#残差独立性检验
bptest(mdl2)#残差异方差检验
#7.回归模型预测
newdat=slice_sample(penguins[,-6],n=5)
predict(mdl2,newdat,interval="confidence")

#梯度下降法求线性回归
gd=function(X,y,init,eta=1e-3,err=1e-3,maxit=1000,adapt=FALSE){
  X=cbind(Intercept=1,X)
  beta=init
  names(beta)=colnames(X)
  loss=crossprod(X%*%beta-y)
  tol=1
  iter=1
  while(tol>err&&iter<maxit){#迭代
    LP=X%*%beta
    grad=t(X)%*%(LP-y)
    betaC=beta-eta*grad
    tol=max(abs(betaC-beta))
    beta=betaC
    loss=append(loss,crossprod(LP-y))
    iter=iter+1
    if(adapt)
      eta=ifelse(loss[iter]<loss[iter-1],eta*1.2,eta*0.8)
  }
  list(beta=beta,loss=loss,iter=iter,fitted=LP,
       RMSE=sqrt(crossprod(LP-y)/(nrow(X)-ncol(X))))
}
n=1000
set.seed(123)
x1=rnorm(n)
x2=rnorm(n)
y=1+0.6*x1-0.2*x2+rnorm(n)
X=cbind(x1,x2)
gd_rlt=gd(X,y,rep(0,3),err=1e-8,eta=1e-4,adapt=T)
gd_rlt$iter#迭代次数
plot(gd_rlt$loss,xlab="迭代次数",ylab="损失")


#数据清洗
#探索性数据分析
#缺失值
library(naniar)
replace_with_na(df,replace=list(x=9999))#数据集中人为标记的特殊符号替换为NA
mean(c(1,2,NA,4))
mean(c(1,2,NA,4),na.rm=T)#NA具有传染性,可以用na.rm参数忽略NA
#1.探索缺失值
library(naniar)
mcar_test(airquality)#检验是否为完全随机缺失,P<0.05则不是MCAR
vis_miss(airquality)#可视化缺失值
#2.缺失值统计
n_miss(airquality)#缺失样本个数
n_complete(airquality)#完整样本个数
prop_miss_case(airquality)#确实样本占比
prop_miss_var(airquality)#缺失变量占比
miss_case_summary(airquality)#每行缺失情况排序
miss_case_table(airquality)#行缺失汇总表
miss_var_summary(airquality)#每个变量缺失情况排序
miss_var_table(airquality)#变量缺失汇总表
#每个缺失汇总函数都有对应的可视化函数
gg_miss_var(airquality)
gg_miss_case(airquality)
#3.对比缺失与非缺失
aq_shadow=bind_shadow(airquality)#影子矩阵,不缺失标记为!na
aq_shadow %>% #根据Ozone是否缺失,统计Solar.R
  group_by(Ozone_NA) %>% 
  summarise_at(.vars="Solar.R",.funs=c("mean","sd","var","min","max"),na.rm=T)
ggplot(aq_shadow,aes(Temp,color=Ozone_NA))+#根据Ozone是否缺失绘制温度分布
  geom_density()
#4.缺失插补
library(simputation)
na.omit(airquality)#直接删除包含NA的样本
drop_na(airquality,Ozone)#删除某些列包含NA的hang
airquality %>% 
  filter(pmap_lgl(.,~mean(is.na(c(...)))<0.6))#删除缺失超过60%的行
airquality %>% 
  select(where(~mean(is.na(.x))<0.6))#删除缺失超过60%的列
#单重插补
library(naniar)
airquality %>% 
  group_by(Month) %>% 
  mutate(Ozone=impute_mean(Ozone))#均值插补
impute_median(airquality,Ozone~Month)#中位数插补
airquality %>% 
  select(Ozone) %>% 
  map_dfc(~replace_na(.x,rstatix::get_mode(.x)[1]))#众数插补
impute_lm(airquality,Ozone~Solar.R+Wind+Temp,
          add_residual="normal")#线性回归插补,添加随机误差
#例子
library(simputation)
airquality %>% 
  bind_shadow() %>% as.data.frame() %>% 
  impute_cart(Ozone~Solar.R+Wind+Temp) %>% #随机森林插补
  add_label_shadow() %>% 
  ggplot(aes(Solar.R,Ozone,color=any_missing))+
  geom_point()+
  theme(legend.position="top")

#多重插补
library(mice)
ap_imp=mice(airquality,m=5,maxit=10,method="pmm",
            seed=1,print=F)#m设置数据集副本数,maxit设置插补的最大迭代次数,连续pmm,二分类logreg,多分类polyreg
aq_dat=complete(ap_imp)

#插值法插补(时间序列)
library(imputeTS)
imp=na_interpolation(tsAirgap,option="spline")
imp=na_seadec(tsAirgap)
ggplot_na_imputations(tsAirgap,imp,tsAirgapComplete)

#异常值
#单变量异常值
univ_outliers=function(x,method="boxplot",k=NULL,
                       coef=NULL,lp=NULL,up=NULL){
  switch(method,
         "sd"={
           if(is.null(k))k=3
           mu=mean(x,na.rm=T)
           sd=sd(x,na.rm=T)
           LL=mu-k*sd
           UL=mu+k*sd},
         "boxplot"={
           if(is.null(coef))coef=1.5
         Q1=quantile(x,0.25,na.rm=T)
         Q3=quantile(x,0.75,na.rm=T)
         iqr=Q3-Q1
         LL=Q1-coef*iqr
         UL=Q3+coef*iqr},
         "percentiles"={
           if(is.null(lp))lp=0.025
           if(is.null(up))up=0.975
           LL=quantile(x,lp)
           UL=quantile(x,up)
         })
  idx=which(x<LL|x>UL)
  n=length(idx)
  list(outliers=x[idx],outlier_idx=idx,outlier_num=n)
}
x=mpg$hwy
univ_outliers(x)#箱线图法
univ_outliers(x,method="sd")
univ_outliers(x,method="percentiles")

#多变量异常值
#1.局部异常因子(LOF)法(异常值周围较稀疏)
library(DMwR2)
lofs=lofactor(iris[,1:4],k=10)#k为邻居数
order(lofs,decreasing=T)[1:5]#选择LOF值最大的5个索引认为是异常样本
#或
library(Rlof)
lofs=lof(iris[,1:4],k=10)
order(lofs,decreasing=T)[1:5]

#2.基于聚类算法
library(DMwR2)
rlt=outliers.ranking(iris[,1:4])
sort(rlt$prob.outliers,decreasing=T)[1:5]

#3.基于模型的异常值
mod=lm(mpg~wt,mtcars)
cooks.distance(mod)
hatvalues(mod)
influence.measures(mod)#同时输出四个强影响度量值
library(car)
outlierTest(mod)#bonferroni法

#4.随机森林法异常值检测
library(outForest)
#用iris数据生成若干异常值
irisWithOut=generateOutliers(iris,p=0.02,seed=123)
#检查除Sepal.Length外的异常值,异常值数为3
out=outForest(irisWithOut,.-Sepal.Length~.,
              max_n_outliers=3,verbose=0)
outliers(out)
plot(out,what="scores")
Data(out)#去除替换异常值后的数据

#特征工程
#特征缩放
#1.标准化
scale(x)#标准化
scale(x,scale=F)#中心化:减去均值
#2.归一化
rescale=function(x,type="pos",a=0,b=1){
  rng=range(x,na.rm=T)
  switch (type,
    "pos"=(b-a)*(x-rng[1])/(rng[2]-rng[1])+a,
    "neg"=(b-a)*(rng[2]-x)/(rng[2]-rng[1])+a)
}
iris %>% 
  as_tibble() %>% 
  mutate(across(where(is.numeric),rescale,type="neg",b=100))
#3.行规范化(使每行向量长度相同)
iris[1:3,-5] %>% 
  pmap_dfr(~c(...)/norm(c(...),"2"))
#4.数据平滑(去除噪声)
library(slider)
library(patchwork)
p1=economics %>% 
  ggplot(aes(date,uempmed))+
  geom_line()
p2=economics %>% #五点移动平均
  mutate(uempmed=slide_dbl(uempmed,mean,.before=2,.after=2)) %>% 
  ggplot(aes(date,uempmed))+
  geom_line()
p1|p2

#特征变换
#1.非线性变换
library(tidymodels)
recipe(hwy~displ+cty,data=mpg) %>% #准备数据和模型变量
  step_poly(all_predictors(),degree=2,#degree设置多项式次数
            options=list(raw=T)) %>% 
  prep() %>% #用数据估计特征工程步
  bake(new_data=NULL)#应用到原数据
#2.正态性变换
df=mlr3data::kc_housing
p1=ggplot(df,aes(price))+
  geom_histogram()
p2=ggplot(df,aes(log10(price)))+#右偏态做对数或开方变换
  geom_histogram()
library(patchwork)
p1|p2
#Box-Cox变换(适用于非负)
library(bestNormalize)
x=rgamma(100,1,1)
bc_obj=boxcox(x)
bc_obj$lambda#最优lambda
p=predict(bc_obj)#变换
x2=predict(bc_obj,newdat=p,inverse=T)
#Yeo-Johnson变换
library(bestNormalize)
yj_obj=yeojohnson(x)
yj_obj$lambda#最优lambda
p=predict(yj_obj)#变换
x2=predict(yj_obj,newdat=p,inverse=T)

#3.连续变量离散化
library(rbin)
df=readxl::read_xlsx("datas/hyper.xlsx")
bins=df %>% 
  rbin_equal_length(hyper,age,bins=3)#将age切分为三个数量一样的类
rbin_create(df,age,bins)#创建虚拟变量

#特征降维
#主成分分析
library(recipes)
recipe(~.,data=iris) %>% 
  step_normalize(all_numeric()) %>% 
  step_pca(all_numeric(),num_comp=2) %>%#num_comp=3设置成分个数或用threshold=0.85设置保留信息的阈值
  prep() %>% 
  bake(new_data=NULL)


#探索变量间关系
#1.两个分类变量
titanic=read_rds("datas/titanic.rds")
titanic %>% 
  ggplot(aes(Pclass,fill=Survived))+
  geom_bar(position="dodge")#可视化
library(rstatix)
tbl=table(titanic$Pclass,titanic$Survived)#交叉表
cramer_v(tbl)#Cramer'V检验,0.3~0.7弱相关,>0.7强相关
prop_test(tbl)#比例检验
chisq_test(tbl)#卡方检验
#2.分类变量与连续变量
ggplot(mpg,aes(displ,color=drv))+
  geom_density()#可视化,概率密度曲线
mpg %>% 
  group_by(drv) %>% 
  get_summary_stats(displ,type="five_number")#分组描述
mpg %>% 
  anova_test(displ~drv)#方差分析
MANOVA(mpg,dv="displ",between="drv")#bruceR
#3.两个连续变量
iris[-5] %>% #相关性分析
  cor_mat() %>% #相关系数矩阵
  replace_triangle(by=NA) %>% #将下三角替换为NA
  cor_gather() %>% #宽变长
  arrange(-abs(cor))#绝对值降序
library(GGally)
ggpairs(iris,columns=names(iris))#可视化因变量和多个自变量的关系

#R机器学习
library(mlr3verse)
#创建分类任务
task=as_task_classif(iris,target="Species")
#选择学习器,设置超参数
learner=lrn("classif.rpart",maxdepth=3,minsplit=10)
#划分训练集和测试集
set.seed(123)
split=partition(task,ratio=0.7)
#训练模型
learner$train(task,row_ids=split$train)
#模型预测
predictions=learner$predict(task,row_ids=split$test)
#模型评估
predictions$confusion#混淆矩阵
predictions$score(msr("classif.acc"))#预测精度
