library(tidyverse)
#1.创建数据框
df=tibble(#按列录入
  grammer=c("Python","C","Java","GO",NA,"SQL","PHP","Python"),
  score=c(1,2,NA,4,5,6,7,10)
)
df=tribble(~grammer,~score,
           "Python",1,
           "C",2,
           "Java",NA,
           "GO",4,
           NA,5,
           "SQL",6,
           "PHP",7,
           "Python",10)
#2.提取行
df %>% 
  filter(grammer=="Python")
#3.查看列名
names(df)
#4.修改列名
df=df %>% 
  rename(popularity=score)
#5.统计频数
table(df$grammer) #or
df %>% 
  count(grammer)
#6.缺失值处理(上下平均值填补)
df=df %>% 
  mutate(popularity=zoo::na.approx(popularity))
#7.筛选行
df %>% 
  filter(popularity>3)
#8.按grammer列去重
df %>% 
  distinct(grammer,.keep_all=T)
#9.计算popularity平均值
df %>% 
  summarise(popularity_avg=mean(popularity))
#10.将grammar列转换为序列
df$grammer #or
df %>% 
  .$grammer #or
df %>% 
  pull(grammer)
#11.将数据框保存为excel
writexl::write_xlsx(df,"datas/filename.xlsx")
#12.查看行数列数
dim(df)
#13.提取popularity值大于3小于7的
df %>% 
  filter(popularity>3&popularity<7)
#14.互换两列的位置
df %>% 
  select(popularity,grammer) #or
df %>% 
  relocate(popularity,grammer)
#15.提取popularity最大值所在的行
df %>% 
  slice_max(popularity) #or
#16.查看最后几行
df %>% 
  tail(n=3) #默认6行,or
df %>% 
  slice_tail(n=3)
#17.删除最后一行
df %>% 
  slice(-n())
#18.添加一行数据
df %>% 
  bind_rows(tibble(grammer="Perl",popularity=6)) #or
df %>% 
  add_row(grammer="Perl",popularity=6)
#19.populariy列从大到小排列
df %>% 
  arrange(-popularity)
#20.统计grammer列的字符串长度
df %>% 
  mutate(strlen=str_length(grammer))
#21.读取本地excel
df=readxl::read_xlsx("datas/21-50数据.xlsx")
#22.查看df数据前几行
head(df,8) #or
slice_head(df,n=8)
#23.将salary列转换为最大值和最小值的平均值
df=df %>% 
  separate(salary,into=c("low","high"),sep="-") %>% 
  mutate(salary=c(parse_number(low)+parse_number(high))*1000/2) %>% 
  select(-c(low,high))
#24.根据学历分组,计算平均薪资
df %>% 
  group_by(education) %>% 
  summarise(salary_avg=mean(salary))
#25.将creatTime转换为“月-日”
library(lubridate)
df=df %>% 
  mutate(createTime=str_sub(createTime,6,10))
#25.查看数据结构
df %>% 
  glimpse() #or str()
object.size(df)#查看占用内存
#26.查看数据汇总
summary(df)
#27.将salary离散化为三水平值
df=df %>% 
  mutate(class=case_when(salary>0&salary<5000~"低",
                         salary>=5000&salary<20000~"中",
                         TRUE~"高")) #or
df=df %>% 
  mutate(class=cut(salary,
                   breaks=c(0,5000,20000,Inf),
                   labels=c("低","中","高"),
                   right=FALSE)) #or
df=df %>% 
  mutate(class=sjmisc::rec(salary,
                           rec="min:5000=低;5000:20000=中;20000:max=高"))
#29.对salary降序排列
df %>% 
  arrange(-salary) #or
df %>% 
  arrange(desc(salary))
#30.提取第33行
df[33,] #or
df %>% 
  slice(33)
#31.计算salary的中位数
df %>% 
  summarise(salary_med=median(salary)) #or
median(df$salary)
#32.绘制salary频率直方图
ggplot(df,aes(salary))+
  geom_histogram(bins=10)+
  theme_bw()
#33.绘制salary频率密度曲线
ggplot(df,aes(salary))+
  geom_density()+
  theme_bw()
#34.删除最后一列
df %>% 
  select(-class)
#35.将第一第二列合并为一列
df %>% 
  unite("Time_edu",1:2,sep="/")
#36.将salary和edu合并为一列
df %>% 
  unite("edu_sal",c(education,salary),sep="/")
#37.计算salary的极差
max(df$salary)-min(df$salary) #or
df %>% 
  summarise(range=max(salary)-min(salary))
#38.第一行与最后一行拼接
df %>% 
  slice(1,n())
#39.将第八行添加到末尾
df %>% 
  bind_rows(slice(.,8))
#40.查看每一列的数据类型
df %>% 
  glimpse()
#41.将createTime设为索引列
df %>% 
  distinct(createTime,.keep_all=T) %>% #索引列不能有重复,因此先去重
  column_to_rownames("createTime") %>% 
  head()
#42.生成一个长度和df相同的随机数据框
df1=tibble(rnums=sample.int(10,nrow(df),replace=T))
#43.将df1与df按列合并
df=df %>% 
  bind_cols(df1)
#44.设置新列new为salary-新列
df=df %>% 
  mutate(new=salary-rnums)
#45.检查缺失值
anyNA(df) #or
naniar::miss_var_summary(df)
#46.将salary转换为浮点数
df %>% 
  mutate(salary=as.double(salary))
#47.计算salary大于10000的次数
df %>% 
  summarise(n=sum(salary>10000))
df %>% 
  count(salary>10000)
#48.查看每种学历出现次数
df %>% 
  count(education)
#49.查看有几种学历
df %>% 
  distinct(education)
#50.提取salary与new列之和大于60000的最后3行
df %>% 
  filter(salary+new>6000) %>% 
  tail(3)
#51.绝对路径读取本地excel
df=readxl::read_xls("C:/Users/buyuxianshi/OneDrive - mnvky/桌面/R/练习/datas//51-80数据.xls")
#52.查看数据框前三行
head(df,3)
#53.查看每行的缺失情况
naniar::miss_var_summary(df)
#54.查看日期缺失的行
df %>% 
  filter(is.na(日期)) #or
which(is.na(df$日期))#日期缺失的行号
#55.查看每列缺失值在哪行
naIdx=df %>% 
  naniar::where_na()
split(naIdx[,1],naIdx[,2])
#56.删除所有存在缺失的行
df %>% 
  drop_na()
#57.绘制收盘价折线图
ggplot(df,aes(日期,`收盘价(元)`))+
  geom_line()
#58.绘制开盘价与收盘价
df %>% 
  select(日期,`开盘价(元)`,`收盘价(元)`) %>% 
  pivot_longer(-日期,names_to="type",values_to="price") %>% #为了自动添加图例做了宽变长变换
  ggplot(aes(日期,price,color=type))+
  geom_line()
#59.绘制涨跌直方图
ggplot(df,aes(`涨跌幅(%)`))+
  geom_histogram()
#60.直方图更细致
ggplot(df,aes(`涨跌幅(%)`))+
  geom_histogram(bins=100)
#61.用df的列名
names(df) %>% 
  as_tibble()
#62.输出换手率非数字的行
df %>%
  mutate(`换手率(%)`=parse_number(`换手率(%)`)) %>%
  filter(is.na(`换手率(%)`))
#63.输出所有换手率为-的行
df %>% 
  filter(`换手率(%)`=="--")
#64.重置df行号
rownames(df)=NULL#R中无行号即为数字索引
#65.删除所有换手率为非数字的行
df %>% 
  mutate(`换手率(%)`=parse_number(`换手率(%)`)) %>% 
  filter(!is.na(`换手率(%)`))#为了便于后续处理做数值型转化,并转化为tsibble对象
library(tsibble)#时间序列
df=df %>% 
  mutate_at(vars(4:18),as.numeric) %>% 
  mutate(日期=lubridate::as_date(日期)) %>% 
  as_tsibble(index=日期,key=c(代码,简称))
#66.绘制换手率密度曲线
ggplot(df,aes(`换手率(%)`))+
  geom_density()
#67.计算前后两天收盘价差值
df %>% 
  mutate(delta=`收盘价(元)`-lag(`收盘价(元)`)) %>% 
  select(日期,`收盘价(元)`,delta)
#68.计算前后两天收盘价的变化率
df %>% 
  mutate(change=(`收盘价(元)`-lag(`收盘价(元)`))/lag(`收盘价(元)`)) %>% 
  select(日期,`收盘价(元)`,change)
#69.设置日期为索引
df %>% 
  column_to_rownames("日期") %>% 
  head()
#70.对收盘价做步长为5的滑动平均
df %>% 
  mutate(avg_5=slider::slide_dbl(`收盘价(元)`,mean,na.rm=T,.before=2,.ater=2)) %>% 
  select(日期,`收盘价(元)`,avg_5)
#71.对收盘价做步长为5的滑动求和
df %>% 
  mutate(sum_5=slider::slide_dbl(`收盘价(元)`,sum,na.rm=T,.before=2,.ater=2)) %>%
  select(日期,`收盘价(元)`,sum_5)
#72.将收盘价及其5日均线、20日均线绘制在同一个图上
df %>% mutate(avg_5=slider::slide_dbl(`收盘价(元)`,mean,na.rm=T,.before=2,.ater=2),
              avg_20=slider::slide_dbl(`收盘价(元)`,mean,na.rm=T,.before=10,.ater=9)) %>% 
  pivot_longer(c(`收盘价(元)`,avg_5,avg_20),
               names_to="type",
               values_to="price") %>% 
  ggplot(aes(日期,price,color=type))+
  geom_line()
#73.按周为采样规律,计算一周收盘价最大值
weekmax=df %>% 
  index_by(weeks=~yearweek(.)) %>% 
  summarise(max_week=max(`收盘价(元)`,na.rm=T))
#74.绘制重采样数据与原始数据
ggplot()+
  geom_line(data=weekmax,aes(weeks,max_week),color="red")+
  geom_line(data=df,aes(日期,`收盘价(元)`),color="steelblue")
#75.将数据往后移动5天
df %>% 
  mutate(across(4:18,~lag(.x,5)))
#76.将数据往前移动5天
df %>% 
  mutate(across(4:18,~lead(.x,5)))
#77.计算开盘价的累计平均
rlt=df %>% 
  mutate(累计平均=cummean(`开盘价(元)`)) %>% 
  select(日期,`开盘价(元)`,累计平均)
#78.绘制开盘价和积累平均的折线图
rlt %>% 
  pivot_longer(-日期,names_to="type",values_to="price") %>% 
  ggplot(aes(日期,price,color=type))+
  geom_line()
#79.计算布林指标
boll=df %>% 
  mutate(avg_20=slider::slide_dbl(`收盘价(元)`,mean,na.rm=T,.before=10,.after=9),
         sd_20=slider::slide_dbl(`收盘价(元)`,sd,na.rm=T,.before=10,.after=9),
         up=avg_20+2*sd_20,down=avg_20-2*sd_20) %>% 
  select(日期,`收盘价(元)`,avg_20,up,down)
#80.绘制布林指标
boll %>% 
  pivot_longer(-日期,names_to="type",values_to="price") %>% 
  ggplot(aes(日期,price,color=type))+
  geom_line()
#81.加载并查看tidyverse版本
library(tidyverse)
#82.生成20个0-100的随机数
set.seed(123)
df1=tibble(nums=sample.int(100,20))
#83.生成20个0-100固定步长的数
df2=tibble(nums=seq(0,99,by=5))
#84.生成20个标准正态分布的随机数
df3=tibble(nums=rnorm(20,0,1))
#85.将df1 df2 df3按行合并
bind_rows(df1,df2,df3)
#86.将df1 df2 df3按列合并
df=bind_cols(df1,df2,df3)
#87.查看df所有数据的最小值、25% 分位数、中位数、75% 分位数、最大值
unlist(df) %>% 
  summary()
#88.修改列名为col1,col2,col3
df=df %>% 
  set_names(str_c("col",1:3))#修改个别列名用rename
#89.提取在第一列而不在第二列的数
setdiff(df$col1,df$col2)
#90.提取第一列和第二列中频率最高的三个数
df %>% 
  pivot_longer(1:2,
               names_to="col",
               values_to="value") %>% 
  count(value,sort=T) %>% 
  slice_max(n,n=3) #or
c(df$col1,df$col2) %>% 
  table() %>% 
  sort(decreasing=T) %>% 
  .[1:3] #or
rlt=tibble(nums=c(df$col1,df$col2)) %>% 
  sjmisc::frq(nums,sort.frq="desc")
rlt[[1]][1:3,]
#91.提取第一列和第二列中可以整除5的位置
which(df$col1%%5==0) #or
df %>% 
  mutate(id=row_number()) %>% 
  filter(col1%%5==0) %>% 
  pull(id)
#直接选出对应数值
df %>% 
  filter(col1%%5==0) %>% 
  select(col1)
#92.计算第一列的一阶差分
diff(df$col1) #or
df %>% 
  mutate(diff1=col1-lag(col1))
#93.将三列顺序颠倒
df %>% 
  select(rev(names(df)))
#94.提取第一列位置在1,10,15的数
df[c(1,10,15),1]
#95.查找第一列的局部最大值位置
df %>% 
  mutate(diff=sign(col1-lag(col1))+sign(col1-lead(col1)),
         id=row_number()) %>% 
  filter(diff==2) %>% 
  pull(id)
#直接得到局部最大值
df %>% 
  mutate(diff=sign(col1-lag(col1))+sign(col1-lead(col1))) %>% 
  filter(diff==2)
#96.计算每一行的均值
df %>% 
  mutate(row_avg=(col1+col2+col3)/3) #or
rowMeans(df) #or
apply(df,1,mean) #or
df %>% 
  rowwise() %>% 
  mutate(row_avg=mean(c_across(1:3))) #or
df %>% 
  mutate(row_avg=pmap_dbl(.,~mean(c(...))))
#97.计算第二列步长为3的滑动平均
df %>% 
  mutate(avg_3=slider::slide_dbl(col2,mean,.before=1,.after=1))
#98.按第三列大小升序排列
df %>% 
  arrange(col3)
#99.将第一列大于50的数修改为高
df %>% 
  mutate(col1=ifelse(col1>50,"高",col1)) #or
df %>% 
  mutate(col1=sjmisc::rec(col1,rec="50:max=高;else=copy"))
#100.计算第一列与第二列的欧氏距离
dist(t(df[,1:2]))
#101.从 csv 文件中读取指定数据:读取前10行,positionName和salary列
read.csv("datas/数据1_101-120涉及.csv",fileEncoding="GBK",nrows=10) %>% 
  select(positionName,salary)
#102.从csv文件中读取数据,将薪资大于10000的改为 “高”
df=read_csv("datas/数据2_101-120涉及.csv") %>% 
  mutate(薪资水平=ifelse(薪资水平>10000,"高","低"))
#103.从df中对薪资每隔20行抽样
df[seq(1,nrow(df),20),] #or
df %>% 
  slice(seq(1,n(),20))
#104.取消科学计数法
set.seed(123)
df=tibble(val=runif(10)^10)
df %>% 
  mutate(val=scales::number(val,accuracy=0.001))
#科学计数法
df %>% 
  mutate(val=scales::scientific(val,2))
#105.转化为百分数
df %>% 
  mutate(val=scales::percent(val,0.01))
#106.查找第三大值的行号
order(df$val,decreasing=T)[3]
df %>% 
  mutate(id=row_number()) %>% 
  arrange(-val) %>% 
  slice(3) %>% 
  pull(id)
#直接获得第三大值
df %>% 
  arrange(-val) %>% 
  slice(3)
#107.反转df的行
df %>% 
  slice(rev(1:n()))
#108.根据多列匹配合并数据,保留df1和df2的观测
df1=tibble(
  key1=c("K0","K0","K1","K2"),
  key2=c("K0","K1","K0","K1"),
  A=str_c("A",0:3),
  B=str_c("B",0:3)
)
df2=tibble(
  key1=c("K0","K1","K1","K2"),
  key2=str_c("K",rep(0,4)),
  A=str_c("C",0:3),
  B=str_c("D",0:3)
)
df1 %>% 
  full_join(df2,by=c("key1","key2"))
#109.根据多列匹配合并数据,保留df1的观测
df1 %>% 
  left_join(df2,by=c("key1","key2"))
#110.读取数据1,并展示所有列
df=read_csv("datas/数据1_101-120涉及.csv",locale=locale(encoding="GBK")) %>% 
  glimpse()
#111.查找secondType与thirdType值相等的行号
which(df$secondType==df$thirdType) #or
df %>% 
  mutate(id=row_number()) %>% 
  filter(secondType==thirdType) %>% 
  pull(id)
#直接输出行
df %>% 
  filter(secondType==thirdType)
#112.查找薪资大于平均薪资的第三个数据
df %>% 
  filter(salary>mean(salary)) %>% 
  slice(3)
#113.salary开根号
df %>% 
  mutate(salary_sqrt=sqrt(salary)) %>% 
  select(salary,salary_sqrt)
#114.将上一题的linestation列按_拆分
df %>% 
  separate(linestaion,into=c("line","station"),sep="_",remove=F) %>% 
  select(linestaion,line,station)
#115.查看df一共有多少列
ncol(df)
#116.提取industryField列以"数据"开头的行
df %>% 
  filter(str_detect(industryField,"^数据"))
#117.以salary score和positionId做数据透视表
df %>% 
  group_by(positionId) %>% 
  summarise(salary_avg=mean(salary),
            score_avg=mean(score))
#118.同时对salary、score两列进行汇总
df %>% 
  summarise(across(c(salary,score),
                   list(sum=sum,mean=mean,min=min),
                   .names="{.col}_{.fn}"))
#119.同时对不同列进行不同的汇总计算:对salary求平均,对score求和
df %>% 
  summarise(salary_avg=mean(salary),
            score_sum=sum(score))
#120.计算并提取平均薪资最高的区
df %>% 
  group_by(district) %>% 
  summarise(salary_avg=mean(salary)) %>% 
  slice_max(salary_avg)
