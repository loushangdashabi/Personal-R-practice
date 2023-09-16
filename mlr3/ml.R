library(tidyverse)
#1.宽表转长表
#值列只有一个变量
df=read_csv("datas/分省年度GDP.csv")
df %>% 
  pivot_longer(-地区,names_to="年份",values_to="GDP")
#值列有多个变量
load("datas/family.rda")
family %>% 
  pivot_longer(-family,
               names_sep="_",
               names_to=c(".value","child"),
               values_drop_na=T)
df=read_csv("datas/参赛队信息.csv")
#正则表达式
knitr::kable(df,align="c")
df %>% 
  pivot_longer(everything(),
               names_pattern="(.*\\d)(.*)",
               names_to=c("队员",".value"))
#sample
dat=read_csv("datas/demo_t.test.csv")
dat %>% 
  pivot_longer(-1,names_pattern="(.*)_",
               names_to=".value") %>% 
  pivot_longer(-1,names_to="grp",
               values_to="val") %>% 
  group_by(compoundID) %>% 
  rstatix::t_test(val~grp)

#2.长表转宽表
#单个值列
load("datas/animals.rda")
animals %>% 
  pivot_wider(names_from=Type,values_from=Heads,values_fill=0)
#多个值列
us_rent_income %>% 
  pivot_wider(names_from=variable,values_from=c(estimate,moe))
#经常存在的问题,1.长变宽行数没变 2.值不能唯一识别,变为列表列
df=tibble(
  x=1:6,y=c("A","A","B","B","C","C"),
  z=c(2.13,3.65,1.88,2.30,6.55,4.21)
)
df %>% 
  pivot_wider(names_from=y,values_from=z)#由于x的存在行数无法压缩
df %>% 
  pivot_wider(-x,names_from=y,values_from=z)#值不能唯一识别,变为列表列
df %>% 
  group_by(y) %>% 
  mutate(n=row_number()) %>% 
  pivot_wider(id_cols=-x,names_from=y,values_from=z)

#整理电话号码
df=tibble(
  ID=c("A","B","B","C","D","D"),
  Tel=sample(13900000000:14000000000, 6)
)
df %>% 
  group_by(ID) %>% 
  mutate(n=row_number()) %>% 
  pivot_wider(names_from=n,values_from=Tel,names_prefix="Tel")
#or
df %>% 
  group_by(ID) %>% 
  summarise(Tel=list(Tel)) %>% 
  unnest_wider(Tel,names_sep="")


#拆分列
table3 %>% 
  separate(rate,into=c("cases","population"),sep="/",convert=T)
df=tibble(Class=c("1班", "2班"),
            Name = c("张三，李四，王五", "赵六，钱七"))
df1=df %>% 
  separate_rows(Name,sep="，")#对不定长的列分列,堆叠放置
df1 %>% 
  group_by(Class) %>% 
  summarise(Name=str_c(Name,collapse="，"))#逆操作
df=tibble(
  observation=c("Richmond(Sam)","Windsor(Ash)","Bilpin(Jules)"),
  A_count=c(7,10,5),B_count=c(2,5,8),A_dbh=c(100,90,95),B_dbh=c(110,87,90)
)
df %>% 
  extract(observation,into=c("site","surveyor"),regex="(.*)\\((.*)\\)")#利用正则表达式直接提取多个列

extract(observation, into = c("site", "surveyor"),
        regex = "(.*)\\((.*)\\)")
#合并列
table5 %>% 
  unite(new,century,year,sep="")


#特征工程
library(mlr3verse)
#1.建立管道
gr=po("scale") %>>% po("pca",rank.=2)
#2.调试,查看特征工程步对输入数据做了什么
gr$train(tsk("iris"))[[1]]$data()#训练特征工程管道:提供任务,访问特征工程之后的数据
gr$predict(tsk("iris")$filter(1:5))[[1]]$data()
#3.用于机器学习
gr=gr %>>% lrn("classif.rpart")#接一个学习器
gr_learner=as_learner(gr)

#缺失值插补
#1.简单插补
task=tsk("pima")
task$missings()
po=po("imputeconstant",param_vals=list(
  constant=-999,affect_columns=selector_name("glucose")))#常数项填补
new_task=po$train(list(task=task))[[1]]
new_task$missings()

po=po("imputemean")#均值插补
new_task=po$train(list(task))[[1]]
new_task$missings()

po=po("imputehist")#直方图插补
new_task=po$train(list(task))[[1]]
new_task$missings()

po=po("imputelearner",lrn("regr.rpart"))#学习器插补,regr-整数/数值,classif-因子/逻辑
new_task=po$train(list(task))[[1]]
new_task$missings()

po=po("imputelearner",po("imputehist") %>>% lrn("regr.kknn")) 
new_task=po$train(list(task))[[1]]
new_task$missings()

po=po("imputeoor")#超出范围插补
new_task=po$train(list(task))[[1]]
new_task$missings()

#特征工程
#1.特征缩放
pos=po("scale",scale=F)#中心化
pos=po("scale")#标准化
pop=po("scalerange",param_vals=list(lower=0,upper=1))#归一化
pop=po("spatialsign")#行规范化
pop$train(list(task))[[1]]$data()
#2.特征变换
#非线性特征
pop=po("modelmatrix",formula=~.^2+I(x1^2)+log(x2))
pop$train(list(task))[[1]]$data()
pop = po("modelmatrix",formula=~splines::ns(x1,5))
pop$train(list(task))[[1]]$data()
#计算新特征
pom=po("mutate",mutation=list(
  x1_p=~x1+1,
  Area1=~x1*x2,
  Area2=~x3*x4,
  Area=~Area1+Area2))
pom=po("mutate",mutation=list( #做五点移动平均
  x1_s=~slide_dbl(x1,mean,.before=2,.after=2)))
poca=po("colapply",applicator=as.character)#应用函数到每一列
pop=po("renamecolumns",
       param_vals=list(renaming=c("Petal.Length"="PL")))#修改列名
#正态性变换
pop=po("boxcox")
pop=po("yeojohnson")#数据包含0或负数
#连续特征分箱
pop=po("histbin",breaks=4)#等宽分箱
pop=po("quantilebin",numsplits=4)#分位数分箱
#3.特征降维
pop=po("pca",rank.=2)#主成分降维(线性降维)
pop$train(list(tsk("iris")))[[1]]$data()
pop=po("kernelpca",features=3)#核主成分降维(非线性降维)
pop=po("ica",n.comp=3)#独立成分分析
pop=po("nmf",rank=3) #非负矩阵分解
po=po("removeconstants")#剔除常量特征
#4.分类特征
#因子折叠
poc=po("collapsefactors",target_level_count=5)#合并较少的类
poc$train(list(task))[[1]]$data()
#因子修正
op=po("fixfactors")
op$train(list(tsaktrain))
op$predict(list(tasktest))[[1]]$data()
#因子编码
poe=po("encode",method="one-hot")#读热编码
poe$train(list(task))[[1]]$data()
poe=po("encode",method="treatment")#虚拟编码
poe$train(list(task))[[1]]$data()
#效应编码
poe=po("encodeimpact")#将未出现的水平视为缺失值
#日期时间特征
dat=tsk("bike_sharing")$data()
dat[,date:=as.POSIXct(paste0(date," ",hour,":00:00"),
                      tz="GMT",format="%Y-%m-%d %H:%M:%S")]
task=as_task_regr(dat,target="count")
pipeop_date=po("datefeatures",cyclic=T,minute=F,second=F)
pipeop_date$train(list(task))[[1]]$data() %>% colnames()
#文本特征
library(quanteda)
library(readtext)
docs=readtext("datas/itemsets.txt",docvarsfrom ="filenames",encoding="UTF-8")
task=as_task_regr(docs,target="docvar1")
po_text=po("textvectorizer",param_vals=list(
  stopwords_language="en", # 不支持"ch"
  scheme_df="inverse",
  remove_punct=TRUE,
  remove_symbols=TRUE,
  remove_numbers=TRUE),
  affect_columns=selector_name("text"))
dat=po_text$train(list(task))[[1]]$data()

#处理不均衡数据
#1.欠采样
task=tsk("german_credit")
table(task$truth())
opb_down=po("classbalancing",reference="minor",adjust="major")
result=opb_down$train(list(task))[[1]]
table(result$truth())
#2.过采样
opb_up=po("classbalancing",reference="major",adjust="minor")
result=opb_up$train(list(task))[[1]]
table(result$truth())
#3.SMOTE法
pop=po("colapply",applicator=as.numeric,affect_columns=selector_type("integer")) %>>%
  po("encodeimpact") %>>% 
  po("smote",K=5,dup_size=1)
result = pop$train(task)[[1]]
table(result$truth())

#目标变换
task=tsk("mtcars")
learner=lrn("regr.lm")
g_ppl=ppl("targettrafo",graph=learner)
g_ppl$param_set$values$targetmutate.trafo=function(x)log(x)
g_ppl$param_set$values$targetmutate.inverter=function(x)list(response=exp(x$response))
g_ppl$plot()
gl=as_learner(g_ppl)
gl$train(task)
gl$predict(task)


#正则化回归
library(tidyverse)
library(mlr3verse)
library(glmnet)#正则化广义回归模型
#1.准备数据
boston=tsk("boston_housing")$data()
glimpse(boston)
boston=boston[,-"town"]#town过于离散,为了简单去除
x=modelr::model_matrix(boston,medv~.)[,-1]#将自变量转为模型数据,并去除截距列
boston=bind_cols(x,boston[,"medv"])
#2.创建任务
bh_task=as_task_regr(boston,target="medv")
bh_task
#3.划分训练集与测试集
set.seed(123)
spilt=partition(bh_task,ratio=0.8)
#4.岭回归模型
learner=lrn("regr.glmnet",alpha=0)#选择学习器
learner
learner$param_set#查看所有超参数
library(paradox)
search_space=ps(s=p_dbl(lower=0.001,upper=10))
at=AutoTuner$new(
  learner=learner,
  resampling=rsmp("cv",folds=3L),#三折交叉验证
  measure=msr("regr.rmse"),#评估指标选rmse
  search_space=search_space,
  terminator=trm("evals",n_evals=10),#计算10次终止
  tuner=tnr("random_search"))#随机搜索
at$train(bh_task,row_ids=spilt$train)#启动自动调参
at$tuning_result#查看最优超参数
learner$param_set$values=at$tuning_result$learner_param_vals[[1]]#用最优参数更新参数集
learner=lrn("regr.glmnet",alpha=0,s=at$tuning_result$s)#或者用最优超参数重新构建学习器
learner$train(bh_task,row_ids=split$train)
coef(learner$model,s=at$tuning_result$s)#查看回归系数
plot(learner$model,s=at$tuning_result$s)
predictions=learner$predict(bh_task,row_ids=split$test)#模型预测

