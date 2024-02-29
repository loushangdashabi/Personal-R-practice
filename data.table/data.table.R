library(data.table)
setDTthreads(threads = 20)

#创建一个data.table
dt=data.table(
  x=1:2,
  y=c("A","B")
)

#转化为data.table
df=data.frame(
  x=1:2,
  y=c("A","B")
)

df=as.data.table(df)#复制一份到内存中并转化
setDT(df)#引用转化(浅复制)

#数据重塑
DT = fread("Tidyverse/Datas/分省年度GDP.csv", encoding = "UTF-8")

#宽变长
melt(DT,measure=2:4,variable="年份",value="GDP")

#每行有多个观测
load("Tidyverse/Datas/family.rda")
setDT(family)
melt(family,
     measure=patterns("^dob","^gender"),
     value=c("dob","gender"),na.rm=T)
#tidyverse的实现方法
family %>% 
  pivot_longer(-family,
               names_to=c(".value","child"),
               names_sep="_",
               values_drop_na=TRUE)
