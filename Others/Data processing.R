###缺失值处理###
x <- c(1,2,3,NA,NA,4)
mean(x,na.rm = T)
sum(x,na.rm = T)
sum(is.na(x)) #求取变量中缺失值的数量
x[!is.na(x)] #排除缺失值

iris_na <- iris
for (i in 1:4){
  iris_na[sample(1:nrow(iris),5),i] = NA
}

sapply(iris_na[,1:4],function(x)which(is.na(x)))
sapply(iris_na[,1:4],function(x)sum(is.na(x)))

install.packages('psych')
library(psych)
describe(iris_na) #通过看n的数量观察缺失值

sapply(iris_na[,1:4], function(x)(sum(is.na(x)/nrow(iris_na))))

lm(Sepal.Length~Sepal.Width, data = iris_na, na.action = na.omit)
#回归中na.action默认为na.omit即会自动忽略缺失值

#1.简单地用所有数据的均数去填补缺失值
mean_value <- sapply(iris_na[,1:4],mean,na.rm=T)
for (i in 1:4){
  iris_na[is.na(iris_na[,i]),i]= mean_value[i]
}
View(iris_na)
summary(iris_na) #通过summary函数对其描述观察是否有缺失值

#2.用各组数据的均值分别去填补缺失值
cancer <- data.frame(id=1:1000,sur_days=sample(100:1000,1000,replace = T),
                     type=sample(c('colon','liver','lung'),1000,replace = T),
                     treatment=sample(c('chemo','sugr'),1000,replace = T))
set.seed(20210723)
cancer[sample(1:1000,90),2] <- NA

mean_value <- tapply(cancer$sur_days,list(cancer$type,cancer$treatment),mean,na.rm=T)
for (i in 1:3){
  for (j in 1:2){
    cancer$sur_days[is.na(cancer$sur_days)&cancer$type==rownames(mean_value)[i] & 
                      cancer$treatment==colnames(mean_value)[j]]=mean_value[i,j]
  }
}

mean_value <- tapply(cancer$sur_days,list(cancer$treatment,cancer$type),mean,na.rm=T)
for (i in 1:2){
  for (j in 1:3){
    cancer$sur_days[
      is.na(cancer$sur_days) & cancer$treatment == rownames(mean_value)[i] & 
        cancer$type == colnames(mean_value)[j]
    ]=mean_value[i,j]
  }
}

#3.mice包和Hmisc包
install.packages('mlbench')
library(mlbench)
data('BostonHousing') #仅使用包中的数据集作演示
head(BostonHousing)

original_data <- BostonHousing
set.seed(07231406)
BostonHousing[sample(1:nrow(BostonHousing),80),'rad'] <- NA
BostonHousing[sample(1:nrow(BostonHousing),80),'ptratio'] <- NA

install.packages('mice')
library(mice)
md.pattern(BostonHousing) #返回缺失值的模式

install.packages('Hmisc')
library(Hmisc)
im_mean <- impute(BostonHousing$ptratio, mean)
#第二个参数为填补的数据,可以是mean、median、sd等,也可以是具体值，比较粗略，不推荐
#返回向量形式,可以直接用新增变量方式加入数据框
BostonHousing$im_mean <- im_mean
BostonHousing$ptratio <- NULL

mice_mod <- mice(BostonHousing[,!names(BostonHousing)%in%'medv'],method = 'rf')
mice_output <- complete(mice_mod) #用来生成完整数据框的函数,用于接受类似mice方法的返回值
anyNA(mice_output) #检测是否有缺失值
View(mice_output)

actuals <- original_data$rad[is.na(BostonHousing$rad)]
predicts <- mice_output[is.na(BostonHousing$rad),'rad']

mean(actuals != predicts)

#4.VIM包
install.packages('VIM')
library(VIM)
data('airquality') #自带缺失值
md.pattern(airquality)
aggr_plot <- aggr(airquality,col=c('red','green'),numbers=T,sortVars=T,
                  labels=names(airquality),cex.axis=0.7,gap=3) #缺失值的可视化
marginplot(airquality[1:2]) #缺失值的可视化

data(sleep)
#VIM包中用回归的方法进行差补

sleepIm <- regressionImp(Sleep+Gest+Span+NonD ~ BodyWgt+BrainWgt, data=sleep)
#有缺失值的变量作为应变量,无缺失值的变量作为自变量
#多出来的列中TRUE表示相应位置为填补的缺失值
sleepIm <- regressionImp(Sleep+Gest+Span+NonD ~ BodyWgt+BrainWgt, data=sleep, family = 'auto')
#family参数用于选择回归的方法



###异常值的处理###
set.seed(07231654)
mmhg <- sample(60:250,1000,replace = T)
range(mmhg)
max(mmhg)
min(mmhg)
quantile(mmhg)
fivenum(mmhg)
#以上五个函数对异常值敏感,需要增加na.rm=T

#自定义函数处理离群值
outlierKD <- function(dt,var){
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name,na.rm=T)
  par(mfrow=c(2,2),oma=c(0,0,3,0))
  boxplot(var_name,main='With outliers')
  hist(var_name,main='With outliers',xlab=NA,ylab=NA)
  outlier <- var_name[var_name > 230] #此条为设定的离群值范围,不同情况下修改此条即可
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main='Without outliers')
  hist(var_name,main='Without outliers',xlab=NA,ylab=NA)
  title('Outlier Check',outer = T)
  na2 <- sum(is.na(var_name))
  cat('Outliers identified:',na2-na1,'\n')
  cat('Propotion (%) of outliers:',round((na2-na1)/tot*100, 1), '\n')
  cat('Mean of the outliers:',round(mo,2),'\n')
  m2 <- mean(var_name, na.rm=T)
  cat('Mean without removing outliers:',round(m1,2),'\n')
  cat('Mean if we remove outliers:',round(mo,2),'\n')
  response <- readline(prompt = 'Do you want to remove outliers and to replace with NA? [yes/no]: ')
  if (response == 'y'| response == 'yes'){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt),dt,envir = .GlobalEnv)
    cat('Outliers successfully removed', '\n')
    return(invisible(dt))
  } else {
    cat('Nothing changed','\n')
    return(invisible(var_name))
  }
}
#dt为传入的dataframe,var为观测的变量

set.seed(07260903)
df <- data.frame(bp = c(sample(80:250, 1000, replace = T), NA, 390, 100))
outlierKD(df,bp)



###重复值的处理###
x <- c(1,2,3,4,5,1,2,3)
unique(x) #返回不重复值的向量
duplicated(x) #返回逻辑值,判定条件为是否与前面的数重复,用于提取
x[!duplicated(x)] #效果等同于unique()
anyDuplicated(x) #返回第一个重复值的index,如没有则返回0

install.packages('readxl')
library(readxl)
#my_data <- read_excel('路径') #读取文件
#my_data[!(duplicated(my_data$var1) & (duplicated(my_data$var2)))] #判断var1和var2是否重复并返回不重复的值
#my_data$test <- paste(my_data$var1, my_data$var2)
#new_data <- my_data[!duplicated(my_data$test),]
#先把需要检测的变量统一生成为一个字符串变量,再对其进行重复的判断



###字符串的处理###
x <- c('aaaaaaaa','aaaaa','aaaaaaa')
nchar(x) #返回向量每个字符串的字符数
length(x) #返回向量元素个数
toupper('AAAbbb') #将字符串改为大写
tolower('AAAbbb') #将字符串改为小写

stringa <- LETTERS[1:6]
stringb <- 1:5
paste(stringa,stringb,sep = '-') #将两个向量用分隔符sep连接,返回个数为大个数向量的数量
paste(stringa,stringb,collapse = '-') #将向量粘贴后用collapse连接,仅返回一个字符串

paste0(stringa,stringb) #两向量粘贴后紧密无其他符号,分隔符sep在结合后的字符串后

stringc <- paste(stringa,stringb,sep = '/')
strsplit(stringc,split = '/') #拆分字符串,返回数组

stringd <- c('python','java','c++0++','hahahhzzzhaha')
substr(stringd, start = 2, stop = 4) <- '666666' #截取某段,可以用于直接赋值,但只会按照旧值的长度取新值

seq_names <- c('EU_FRA02_C1_S2008','AF_COM12_B0_2004','AF_COM17_S2008','NAUSA02E02005','eu_fra_a2_s98')
grep(pattern = 'FRA|fra', x = seq_names, value = T) #返回index,value取T时返回值
grepl(pattern = 'FRA', x = seq_names, ignore.case = T) #返回逻辑值,ignore.case取T无视大小写

grepl(pattern = '[s|S][0-9]{2,4}\\b', seq_names) #返回边界2-4位前是否是s或S的逻辑值



###正则表达式###
#1.原义表达式,即表达本身的意思
mystr <- c('apple','orange','av^bb','^dh','9ahdhh','positive','active','agsq7dh','1000','apple','application','abblt','1220','2289','12223','10002','fod','foot','foul','fans','kobe','messi','neymar')
grep('p',mystr)

#2.转义表达式
grep('.',mystr) #'.'表示所有字符
grep('[7-9]',mystr) #[]表示数字取值
grep('^ap',mystr)
grep('[^0-1]',mystr) #^在字符前表示寻找以这些字符开头的字符串,在方括号内表示非的意思
grep('2{2,}',mystr) #{}在正则表达式前,表示寻找出现正则表达式出现次数≥2次的字符串
grep('fo+',mystr) #等同于fo{1,},{}重复的是紧跟在前的o而非fo整体,取整体可写为(fo){1,}
grep('fo*',mystr) #等同于fo{0,}
grep('^k|^m',mystr) #|将两个条件结合起来
grep('ive$', mystr) #$表示取末尾
grep('ive\\b',mystr)#\\b表示匹配边界

#3.保义字符,\\
grep('\\^',mystr)

#4.特殊字符
#\\b匹配边界,\\B匹配非边界
#\\d相当于[0-9],\\D相当于[^0-9]
#\\s表示匹配空格、制表符、换行符在内的空白符号,\\S匹配非空字符(空字符即'')
#\\w相当于[a-z0-9A-Z],\\W相当于[^a-z0-9A-Z]
#\\<匹配空白字符开始的文本,\\>匹配以空白字符结尾的文本
mystr2 <- c('the cashaha','theshaudh','ais the')
grep('\\<the\\>',mystr2) #返回the单独存在的字符index



###stringr和stringi包###
install.packages('stringr')
install.packages('stringi')
install.packages('tidyverse') #安装stringi包的时候出错,通过依赖包的形式安装
library(stringr)
library(stringi)

#stringr包
stri_c() #相当于paste,连接紧密型,但sep分隔符不会插在末尾
stri_length() #计算有多少个字符

wbh <- 'wang bing han'
stri_sub(wbh,c(1,6,11),c(4,9,14))

fruit <- c('apple','banana','peach')
stri_dup(fruit,2:4) #字符串重复2:4次后返回

mystr2 <- ' Eternal love for YanQ   '
str_trim(mystr2,side = 'both') #去除字符串的空格，side参数为设定除左还是除右还是两边

phones <- c('219 733 8965','329-293-8753','bannan','595 381 1231','apple','234.123.3456','lalala')
str_extract(phones,'([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})') #通过正则表达式筛选匹配的字符串,不匹配的用NA代替

str_replace(fruit,'[aeiou]','-') #替换每个字符串中的第一个匹配值
str_replace_all(fruit,'[aeiou]','-') #替换每个字符串中所有的匹配值

#stringi包
stri_join() #作用与stri_c相同
stri_cmp_eq() #比较两个字符串是否相同
stri_cmp_neq() #比较两个字符串是否不同
stri_cmp_lt() #比较前一个字符串是否小于后一个字符串
stri_cmp_gt() #比较前一个字符串是否大于后一个字符串

language <- c('python','R','php','ruby','java','javascript','c','c++','room','spark','go','R2R')
stri_count(language,fixed = 'R') #计数每个字符串出现的R的次数
stri_count(language,regex = '^j') #每个字符串中正则表达式匹配的次数

test <- 'The\u00a0above-mentioned    features are very useful.
Warm thanks to their developers. Tomorrow is a, new$% day###'
stri_count_boundaries(test,type='word')
stri_count_boundaries(test,type='sentence')
stri_count_boundaries(test,type='character')

str_dup(c('abc','pqrst'),c(4,2)) #重复,与stringi包中的stri_dup()相同

stri_duplicated(c('a','b','a',NA,'a',NA),from_last = T) #判断某字符与是否与前面字符重复,NA也计入重复;当from_last为T时,倒序判断
stri_duplicated_any(c('b','b','b','b','c',NA,'c','c')) #返回第一个重复元素的index,返回0表示所有元素都唯一

stri_detect_regex(c('above','abort','about','abnormal','abandon'),'t\\b') #返回逻辑值
stri_detect_regex(c('ABOUT','abort','AboVE'),'^ab',case_insensitive=T) #case_insensitive参数为T不区分大小写

stri_startswith_fixed(c('aa1','aa2','b3','abb'),'a',from = 2) #从开头的第from位检索，返回逻辑值
stri_endswith_fixed(c('aaccc1','accaca2a','b3','abccb'),'a',to = 2) #返回是否以给定值结尾,to表示选择到前几位

seq_names <- c('EU_FRA02_C1_S2008','AF_COM12_B0_2004','AF_COM17_S2008','NAUSA02E02005','eu_fra_a2_s98')
stri_extract_all(seq_names,regex = '[0-9]{2,4}\\b') #提取所有符合要求的部分,并非完整的字符串

stri_extract_all_fixed('abaBAba','ABa',case_insensitive=T,overlap=T) #overlap设置成T时会重复地对同一个字符串进行检索

stri_isempty() #判断字符串是否为空字符
stri_locate_all('I want to learn R to promote my hahahhdahsdto asd',fixed = 'to') #返回所有检索值的index

stri_extract_all_boundaries('string: the string ing ths sakdj awk: 12313,8') #以空格为边界切分,返回值含有空格
stri_extract_all_words('string: the string, ing,ths sakdj awk: 12313,8') #提取所有单词,数字及其中的标点会被视为一个单词



###时间与日期数据的处理###
Sys.setlocale('LC_TIME', 'English') #若出现as.Date一直返回NA值,则可能是Rstudio中默认编码问题,运行此行代码即可

as.Date('2021-07-27')
class(as.Date('2021-07-27')) #返回值为Data
as.Date('07/27/21', format = '%m/%d/%y')
#%m表示数字月份,%b表示英文缩写月份,%B表示英文全写月份,%d表示天,%y表示两位数表达的年份,%Y表示四位数表达的年份
as.Date('March 10, 2007', format = '%B %d, %y')
as.Date(100,origin='2021-7-27') #计算origin日期后100天的日期

ISOdate(1993,3,10,16,14,20) #年月日时分秒,返回值会增加时区

dts <- c('2005-10-21 19:23:22','2017-02-23 17:20:20')
as.POSIXlt(dts) #format参数较多,要用的时候再查

time1 <- strptime('04/3/2016:16:18:34', format = '%d/%m/%Y:%H:%M:%S')
strftime(time1,'Now is %H:%M on %Y %m %d')

julian(as.Date('2021-07-27'),origin = as.Date('2020-07-27')) #计算两个日期的天数差,origin默认为"1970-01-01"
difftime(as.Date('2021-07-27'),as.Date('2020-07-27'),units = 'hours') #units可以用于设置单位,如weeks、hours、seconds
mean(c(as.Date('2021-07-27'),as.Date('2020-07-27'))) #计算中间日期

seq(as.Date('2021-07-27'),by='2 weeks',length.out=10) #从设定时间开始,以by参数为步长，输出length.out个日期

#stringi包中对时间进行处理的方法
stri_datetime_add(as.Date('2021-07-27'),value = 10,units = 'days') #默认时间为08:00:00 CST
stri_datetime_create(2014,4,20) #默认时间为12:00:00 CST
stri_datetime_parse(c('2015-02-12','2015-02-29'),'yyyy-MM-dd',lenient = T)
#解析日期,返回的时分秒为计算机当前时间,若日期错误返回NA,lenient参数设置为T时自动对错误日期进行矫正
stri_datetime_parse('9/dec/16','d/MMM/dd') #详细的format方式见help

##lubridate包对时间日期数据的处理
install.packages('lubridate')
library(lubridate)

ymd('020221')
mdy('020221')
ymd_hms('20210721172145')

x <- c('2009s01s01','2009-07-23','1997 09 21','2009 12','asda 09 12 10','!09 ## 12 $$ 12')
xtime <- ymd(x)
month(xtime, label = T, abbr = F) #label为T输出英文月份、F输出数字月份,abbr默认为T表示输出缩写
day(xtime)
mday(xtime) #返回在这个月份的第几天
wday(xtime) #返回在这周的第几天

new_date <- now() #返回现在的时间
month(new_date) <- 12

dates <- make_date(year = 2010:2021, month = 1:4, day = 1:5) #规律地生成时间
make_datetime() #相比make_date多了时分秒的信息

x_time <- as.POSIXlt('2009-8-3 12:01:59')
round_date(x_time, unit = 'minute') #时间的近似,取值可取halfyear

time_t <- c('2017-02','201609','2017/5')
ymd(time_t,truncated=1) #补全日期都为设定的1

time_length(interval(start = ymd('19000101'), end = ymd('19991231')),unit = 'day') #计算日期间隔,unit可以换为其他值

x_time + days(100) - hours(11) #时间的加减



###时间序列###
ts(1:10, frequency = 12, start = c(1998,2), end = c(2021,4)) #frequency参数设置为1表示年、设置成4表示季度、设置成12表示月、设置成365表示天
#生成时间序列

value <- ts(data = sample(0:300,366,replace = T),start = as.Date('2016-01-01'))
datetest <- seq(from = as.Date('2016-01-01'), by = 1, length.out = 366)
df=data.frame(value = value, time = datetest)

plot((ts(cumsum(1+round(rnorm(100),2)), start = c(1954,7), frequency = 12)))
plot(value)

install.packages('xts')
library(xts)

value2 <- sample(0:100,365,replace = T)
times <- seq(from = as.Date('2017-01-01'),by = 1,length=365)
myts <- xts(value2,times)

window(myts, start = as.Date('2017-01-10'), end = as.Date('2017-02-15')) #提取子集,可以通过这种方法直接赋值

lag(myts) #滞后值,把前一项的值赋给后一项,故第一项为NA
diff(myts) #离差值,此项与前一项的差值，故第一项为NA

##时间序列分析
data(co2)
View(co2)

training <- co2[1:400]
ts_training <- ts(training, start = start(co2), frequency = frequency(co2))
plot(ts_training)

de_co2 <- decompose(ts_training) #将训练集中的时间序列拆分为随即项、季节项、趋势项
plot(de_co2)

install.packages('tseries')
library(tseries)
kpss.test(ts_training) #p value小于0.05,表示不平稳

ts_training_diff <- diff(ts_training) #对训练集进行一阶差分,即确定ARIMA中的D值为1
kpss.test(ts_training_diff)

acf(ts_training) #自相关系数函数,确定ARIMA中的P值
pacf(ts_training) #偏自相关系数函数,确定ARIMA中的Q值

install.packages('forecast')
library(forecast)
co2_fit <- Arima(ts_training, order = c(1,1,1),
                 seasonal = list(order = c(1,1,1), period = 12))

co2_fore <- forecast(co2_fit,68)
plot(co2, col='red')
par(new=T)
plot(co2_fore)
