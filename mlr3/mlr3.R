library(mlr3verse)
#1.R6面向对象
learner$param_set#访问属性
learner$train()#调用方法
#2.任务：封装数据
tsks()#查看自带任务
dat=tsk("german_credit")$data()
task=as_task_classif(dat,target="credit_risk")
task$select(cols=setdiff(task$feature_names,"telephone"))
set.seed(123)
spilt=partition(task,ratio=0.8)#划分训练集和测试集
#3.学习器:封装算法
lrns()#查看自带学习器的名字
learner=lrn("classif.ranger",num.trees=100,predict_type="prob")#随机森林模型
learner$model#训练前为NULL

learner$train(task,row_ids=spilt$train)
learner$model

prediction=learner$predict(task,row_ids=spilt$test)
prediction
#4.性能评估
msrs()#所有性能度量指标
prediction$score(msr("classif.acc"))#准确率
autoplot(prediction,type="roc")#ROC曲线
prediction$score(msr("classif.auc"))#AUC曲线下面积
#5.重抽样
rsmps()#所有重抽样模型
cv10=rsmp("cv",folds=10)#10折交叉验证
#查看重抽样效果
cv10$instantiate(task)#实例化
cv10$iters#数据副本数
cv10$train_set(1)#第1个数据副本的训练集索引
cv10$test_set(1)#第1个数据副本的测试集索引
#使用重抽样
rr=resample(task,learner,cv10,store_models=T)#store_models=T保存每个副本的模型和性能结果
rr$aggregate(msr("classif.acc"))#平均准确率
rr$score(msr("classif.acc"))#每个重抽样的准确率

#查看第1折上的结果
rr$resampling$train_set(1)#训练集索引
rr$learners[[1]]$model#拟合模型
rr$predictions()[[1]]#预测结果

#所有预测合并为一个预测对象
rr$prediction()

#6.基准测试(比较性能)
tasks=tsk("sonar")
learners=lrns(c("classif.rpart","classif.kknn","classif.ranger","classif.svm"),predict_type="prob")
design=benchmark_grid(tasks,learners,rsmps("cv",folds=10))
design
bmr=benchmark(design)
bmr$aggregate(list(msr("classif.acc"),msr("classif.auc")))#汇总基准测试结果
autoplot(bmr,type="roc")#roc曲线
autoplot(bmr,measure=msr("classif.auc"))#auc箱线图

#7.可视化
#可视化任务
#分类任务
task=tsk("penguins")
task$select(c("body_mass","bill_length"))
autoplot(task,type="target")#展示目标变量各类的频数
autoplot(task,type="duo")#展示多个特征的分布
autoplot(task,type="pairs")#多个特征的成对比较
#回归任务
task=tsk("mtcars")
task$select(c("am","carb"))
autoplot(task,type="target")
autoplot(task,type="pairs")

#可视化学习器
#glmnet回归学习器
task=tsk("mtcars")
learner=lrn("regr.glmnet")
learner$train(task)
autoplot(learner)
#决策树学习器
task=tsk("penguins")
learner=lrn("classif.rpart",keep_model=T)#分类树
learner$train(task)
autoplot(learner)

task=tsk("mtcars")
learner=lrn("regr.rpart",keep_model=T)#回归树
learner$train(task)
autoplot(learner)
#层次聚类学习器
task=tsk("usarrests")
learner=lrn("clust.hclust")#层次聚类树
learner$train(task)
autoplot(learner,type="dend",task=task)
autoplot(learner,type="scree")#碎石图

#可视化预测对象
#分类预测对象
task=tsk("spam")
learner=lrn("classif.rpart",predict_type="prob")
pred=learner$train(task)$predict(task)
autoplot(pred,type="stacked")
autoplot(pred,type="roc")#ROC曲线(真阳率&假阳率)
autoplot(pred,type="prc")#PR曲线(查准率&召回率)
autoplot(pred,type="threshold")#threshold图(二元分类在不同阈值下的性能)
#回归预测对象
task=tsk("boston_housing")
learner=lrn("regr.rpart")
pred=learner$train(task)$predict(task)
autoplot(pred,type="xy")#xy图,真实值与预测值
autoplot(pred,type="residual")#响应残差图
autoplot(pred,type="histogram")
#聚类预测对象
task=tsk("usarrests")
learner=lrn("clust.kmeans",centers=3)
pred=learner$train(task)$predict(task)
autoplot(pred,task,type="scatter")#按聚类预测结果着色的散点图
autoplot(pred,task,type="sil")#展示聚类的silhouette 宽度
autoplot(pred,task,type="pca")#展示数据的前两个主成分

#可视化重抽样结果
#分类重抽样结果
task=tsk("sonar")
learner=lrn("classif.rpart",predict_type="prob")
resampling=rsmp("cv")
rr=resample(task,learner,resampling)
autoplot(rr,type="boxplot")#性能度量的分布
autoplot(rr,type="histogram")
autoplot(rr,type="roc")
autoplot(rr,type="prc")

task=tsk("pima")
task$filter(seq(100))
task$select(c("age","glucose"))
learner=lrn("classif.rpart")
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")#prediction图,展示两个特征表示的测试集样本点和以背景色区分的预测类别

learner=lrn("classif.rpart",predict_type="prob")
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")#预测类型为prob,则以颜色深浅表示概率值

learner=lrn("classif.rpart",predict_type="prob",predict_sets=c("train","test"))
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction",predict_sets=c("train","test"))#同时展示训练集

task=tsk("german_credit")
task$filter(seq(100))
task$select(c("housing","employment_duration"))
learner=lrn("classif.rpart")
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")

#回归重抽样结果
task=tsk("boston_housing")
task$select("age")
task$filter(seq(100))
learner=lrn("regr.rpart")
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")

learner=lrn("regr.lm",predict_type="se")#线性回归,包含置信带
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")

learner=lrn("regr.lm",predict_type="se",predict_sets=c("train","test"))
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction",predict_sets=c("train","test"))#包含训练集

task=tsk("boston_housing")
task$select(c("age","rm"))
task$filter(seq(100))
learner=lrn("regr.rpart")
resampling=rsmp("cv",folds=3)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")#加入预测面

#可视化基准测试结果
tasks=tsks(c("pima","sonar"))
learner=lrns(c("classif.featureless","classif.rpart","classif.xgboost"),predict_type="prob")
resampling=rsmp("cv")
bmr=benchmark(benchmark_grid(tasks,learners,resampling))
autoplot(bmr,type="boxplot")

tasks=tsk("pima")
learner=lrns(c("classif.featureless","classif.rpart","classif.xgboost"),predict_type="prob")
resampling=rsmp("cv")
bmr=benchmark(benchmark_grid(tasks,learner,resampling))
autoplot(bmr,type="roc")#绘制多个学习器的roc曲线(适用于均衡数据集)
autoplot(bmr,type="prc")#(适用于不均衡数据集)

#可视化调参
instance=tune(
  tuner=tnr("gensa"),
  task=tsk("sonar"),
  learner=lts(lrn("classif.rpart")),
  resampling=rsmp("holdout"),
  measures=msr("classif.ce"),
  term_evals=100)
autoplot(instance,type="performance")#散点折线图展示随批次的模型性能变化
autoplot(instance,type="parameter",col_x=c("cp","minsplit"))#散点图展示每个超参数取值与模型性能变化
autoplot(instance,type="marginal",cols_x="cp")#展示不同超参数值的性能，颜色表示批次
autoplot(instance,type="parallel")#可视化超参数之间的关系
autoplot(instance,type="points",cols_x=c("cp","minsplit"))#散点热力图展示两个超参数的性能对比
autoplot(instance,type="pairs")#展示所有超参数成对对比
autoplot(instance,type="surface",cols_x=c("cp","minsplit"),learner=lrn("regr.ranger"))

#可视化特征过滤器
task=tsk("mtcars")
f=flt("correlation")
f$calculate(task)
autoplot(f,n=5)


#图学习器
#1.线性图
graph=po("scale") %>>%
  po("encode") %>>%
  po("imputemedian") %>>% 
  lrn("classif.rpart")#搭建图
graph$plot()#可视化图
#转化为图学习器
gl=as_learner(graph)
#训练任务
task=tsk("iris")
gl$train(task)
#调试图
graph$pipeops$scale$param_set$values=list(robust=T)#取出或设置超参数

graph$keep_results=T
graph$train(task)
graph$pipeops$scale$state$scale#获取单独PipeOp的$state

graph$pipeops$encode$.result[[1]]$head()#查看图的中间结果,需提前设置graph$keep_results=T

#2.非线性图
#(1)分支,即只执行若干备选路径中的一条
graph_branch=ppl("branch",list(
  pca=po("pca"),ica=po("ica"))) %>>%
  lrn("classif.kknn")
graph_branch$plot()
#(2)分块训练,防止内存不足
graph_chunks=po("chunk",4) %>>% 
  ppl("greplicate",lrn("classif.rpart"),4) %>>%
  po("classifavg")
graph_chunks$plot()
chunks_lrn=as_learner(graph_chunks)
chunks_lrn$train(tsk("iris"))
#(3)集成学习
#袋装法(并行)
single_path=po("subsample") %>>% lrn("classif.rpart") #单分支
graph_bag=ppl("greplicate",single_path,n=10) %>>% po("classifavg")
graph_bag$plot()
baglrn=as_learner(graph_bag)
baglrn$train(tsk("iris"))

#提升法(串行),如Adaboost,GBDT,XGboost,LightGBM,catBoost

#堆叠法(分阶段)
graph_stack=gunion(list(
  po("learner_cv",lrn("regr.lm")),
  po("learner_cv",lrn("regr.svm")),
  po("nop"))) %>>%
  po("featureunion") %>>% 
  lrn("regr.ranger")
stacklrn=as_learner(graph_stack)
stacklrn$train(tsk("mtcars"))


#特征工程
#标准化
gr=po("scale") %>>% po("pca",rank.=2) 
gr$plot()

#1.调试
task=tsk("iris")
gr$train(task)[[1]]$data()#访问特征工程后的数据
gr$predict(task$filter(1:5))[[1]]$data()#训练好的特征工程用于新数据
#2.用于机器学习,原始任务经过特征工程后变成预处理之后的任务
newtask=gr$train(task)[[1]]
newtask
#3.用于机器学习,再接一个学习器,转化为图学习器
gr=gr %>>% lrn("classif.rpart") 
gr$plot()
gr_learner=as_learner(gr)
gr_learner

#缺失值插补
#1.简单插补
task=tsk("pima")
task$missings()#查看缺失
po=po("imputeconstant",param_vals=list(#常数插补
  constant=-999,affect_columns=selector_name("glucose")
))
new_task=po$train(list(task))[[1]]
new_task$missings()

po=po("imputemean")
new_task=po$train(list(task))[[1]]#均值插补
new_task$missings()

po=po("imputemedian")
new_task=po$train(list(task))[[1]]#中位数插补
new_task$missings()

po=po("imputemode")
new_task=po$train(list(task))[[1]]#众数插补
new_task$missings()

#2.随机抽样插补
po=po("imputesample")
new_task=po$train(list(task))[[1]]
new_task$missings()

#3.直方图插补
po=po("imputehist")
new_task=po$train(list(task))[[1]]
new_task$missings()

#4.学习器插补
po=po("imputelearner",lrn("regr.rpart"))#决策树插补
new_task=po$train(list(task))[[1]]
new_task$missings()

po=po("imputelearner",po("imputehist") %>>% lrn("regr.kknn"))#KNN插补
new_task=po$train(list(task))[[1]]
new_task$missings()

#5.超出范围插补(尤其适合树模型)
set.seed(2409)
data=tsk("pima")$data()
data$y=factor(c(NA,sample(letters,size=766,replace=T),NA))
data$z=ordered(c(NA,sample(1:10,size=767,replace=T)))
task=as_task_classif(data,target="diabetes")
task$missings()
po=po("imputeoor")
new_task=po$train(list(task))[[1]]
new_task$missings()

#特征工程
#1.特征缩放
library(tidyverse)
df=as_tibble(iris) %>% 
  set_names(str_c("x",1:4),"Species")
task=as_task_classif(df,target="Species")

pos=po("scale")#标准化
pos$train(list(task))[[1]]$data()

pos=po("scale",scale=F)#中心化
pos$train(list(task))[[1]]$data()

pop=po("scalerange",param_vals=list(lower=0,upper=1))#归一化,线性缩放到(0,1)
pop$train(list(task))[[1]]$data()

pop=po("spatialsign")
pop$train(list(task))[[1]]$data()#行规范化,所有行处于一个球面

#2.特征变换
#非线性特征
pop=po("modelmatrix",formula=~.^2+I(x1^2)+log(x2))#用高阶项逼近因变量
pop$train(list(task))[[1]]$data()

pop=po("modelmatrix",formula=~splines::ns(x1,5))#基于自然样条的样条特征
pop$train(list(task))[[1]]$data()

#计算新特征
pom=po("mutate",mutation=list(
  x1_p=~x1+1,
  Area1=~x1*x2,
  Area2=~x3*x4,
  Area=~Area1+Area2))
pom$train(list(task))[[1]]$data()

po_ftextract=po("mutate",mutation=list(#利用正则表达式提取信息
  fare_per_persion=~fare/(parch+sib_sp+1),
  deck=~factor(str_sub(cabin,1,1)),
  title=~factor(str_extract(name,"(?<=, ).*(?=\\.)")),
  surname=~factor(str_extract(name,"^.*(?=,)")),
  ticket_prefix=~factor(str_extract(ticket,"^.*(?= )"))
))
po_ftextract$train(list(tsk("titanic")))[[1]]$data()

pom=po("mutate",mutation=list(
  x1_s=~slider::slide_dbl(x1,mean,.before=2,.after=2)))#滑动平均
dat=pom$train(list(task))[[1]]$data()
library(patchwork)
p1=ggplot(dat,aes(1:150,x1))+
  geom_line()
p2=ggplot(dat,aes(1:150,x1_s))+
  geom_line()
p1/p2

poca=po("colapply",applicator=as.character)#应用函数到每一列
pop=po("renamecolumns",param_vals=list(renaming=c("Petal.Length"="PL")))#修改列名

#正态性变换
pop=po("boxcox")#非负非正态变为正态
pop$train(list(task))[[1]]$data()

pop=po("yeojohnson")#包含0或负数采用Yeo-Johnson变换
pop$train(list(task))[[1]]$data()

#连续变量分箱
pop=po("histbin",breaks=4)#等宽分箱
pop$train(list(task))[[1]]$data()

pop=po("quantilebin",numsplits=4)#分位数分箱
pop$train(list(task))[[1]]$data()

#3.特征降维
pop=po("pca",rank.=3)#主成分(PCA)
pop$train(list(task))[[1]]$data()

pop=po("kernelpca",features=3)#核PCA,用于线性不可分的数据集
pop$train(list(task))[[1]]$data()

pop=po("ica",n.comp=3)#独立成分分析(ICA),提取统计意义上的独立成分
pop$train(list(task))[[1]]$data()

pop=po("nmf",rank=3)#非负矩阵分解
pop$train(list(task))[[1]]$data()

library(data.table)
data=data.table(y=runif(10),a=1:10,b=rep(1,10),
                c=rep(1:2,each=5))
task_ex=as_task_regr(data,target="y")
po=po("removeconstants")
po$train(list(task_ex))[[1]]$data()

#4.分类特征
#因子折叠,折叠较少的水平
dat=tibble(color=factor(starwars$skin_color),y=1:87)
dat %>% count(color)
task=as_task_classif(dat,target="y")
poc=po("collapsefactors",target_level_count=5)
poc$train(list(task))[[1]]$data() %>% count(color)

#因子修正
dattrain=data.table(
  a=factor(c("a","b","c",NA),levels=letters),
  b=ordered(c("a","b","c",NA)),target=1:4)
dattest=data.table(
  a=factor(c("a","b","c","d")),
  b=ordered(c("a","b","c","d"),levels=letters[10:1]),
  target=1:4)
tasktrain=as_task_regr(dattrain,target="target")
tasktest=as_task_regr(dattest,target="target")
op=po("fixfactors")
op$train(list(tasktrain))
x=op$predict(list(tasktest))[[1]]$data()

#因子编码
task=tsk("penguins")
poe=po("encode",method="one-hot")#独热编码
poe$train(list(task))[[1]]$data()

poe=po("encode",method="treatment")#虚拟编码
poe$train(list(task))[[1]]$data()

poe=po("encode",method="helmert")#helmert编码
poe$train(list(task))[[1]]$data()

poe=po("encode",method="poly")#多项编码
poe$train(list(task))[[1]]$data()

poe=po("encode",method="sum")#sum编码
poe$train(list(task))[[1]]$data()

#效应编码
poe=po("encodeimpact")
poe$train(list(task))[[1]]$data()

#随机截距编码
poe=po("encodelmer")
poe$train(list(task))[[1]]$data()

#5.日期时间特征
dat=tsk("bike_sharing")$data()
dat[,date:=as.POSIXct(paste0(data," ",hour,":00:00"),tz="GMT",
                      format="%Y-%m-%d %H:%M:%S")]
task=as_task_regr(dat,target="count")
pipeop_date=po("datefeatures",cyclic=T,
               minute=F,second=F)
pipeop_date$train(list(task))[[1]]$data()

#6.文本特征
library(quanteda)
library(readtext)
docs=readtext("datas/job/*.txt",encoding="UTF-8")
task=as_task_regr(docs,target="docvar1")
po_text=po("textvectorizer",param_vals=list(
  stopwords_language="en",
  scheme_df="inverse",
  remove_punct=TRUE,
  remove_symbols=TRUE,
  remove_numbers=TRUE),
  affect_columns=selector_name("text"))
dat=po_text$train(list(task))[[1]]$data()

#7.处理不均衡数据
task=tsk("german_credit")
table(task$truth())
#欠采样
opb_down=po("classbalancing",reference="minor",adjust="major",ratio=2)#默认ratio=1
result=opb_down$train(list(task))[[1]]
table(result$truth())
#过采样
opb_up=po("classbalancing",reference="major",adjust="minor")
result=opb_up$train(list(task))[[1]]
table(result$truth())
#SMOTE法
pop=po("colapply",applicator=as.numeric,#只接受double型特征,先将integer转化为double
       affect_columns=selector_type("integer")) %>>% 
  po("encodeimpact") %>>% 
  po("smote",K=5,dup_size=1)
result=pop$train(task)[[1]]
table(result$truth())

#8.目标变换
task=tsk("mtcars")
learner=lrn("regr.lm")
g_ppl=ppl("targettrafo",graph=learner)
g_ppl$param_set$values$targetmutate.trafo=function(x)log(x)
g_ppl$param_set$values$targetmutate.inverter=function(x)list(response=exp(x$response))
g_ppl$plot()
gl=as_learner(g_ppl)
gl$train(task)
gl$predict(task)

#嵌套重抽样
at=auto_tuner(#内层重抽样
  tuner=tnr("grid_search",resolution=10),
  learner=lrn("classif.rpart",cp=to_tune(lower=0.001,upper=0.1)),
  resampling=rsmp("cv",folds=4),
  measure=msr("classif.acc"),
  term_evals=5)
task=tsk("pima")
outer_resampling=rsmp("cv",folds=3)
rr=resample(task,at,outer_resampling,store_models=T)
extract_inner_tuning_results(rr)#提取内层重抽样结果,查看最优的超参数
rr$score()#查看外层重抽样的每次结果
rr$aggregate()#查看外层重抽样的平均模型性能

#用全部数据自动调参,预测新数据
at$train(task)
dat=task$data()[1:5,-1]
at$predict_newdata(dat)

#采用tune_nested函数,进一步简化
rr=tune_nested(
  tuner=tnr("grid_search",resolution=10),
  task=task,
  learner=lrn("classif.rpart",cp=to_tune(lower=0.001,upper=0.1)),
  inner_resampling=rsmp("cv",folds=4),
  outer_resampling=rsmp("cv",folds=3),
  measure=msr("classif.acc"),
  term_evals=5)

#超参数调参
learner=lrn("classif.svm")
learner$param_set#查看所有超参数

#1.独立调参
#(1)选取任务,划分数据集,将测试集索引设置为留出
task=tsk("iris")
split=partition(task,ratio=0.8)
task$set_row_roles(split$test,"holdout")
#(2)选择学习器,同时指定部分超参数
learner=lrn("classif.svm",type="C-classification",kernel="radial",
            cost=to_tune(0.1,10),
            gamma=to_tune(0,5))#给出参数范围
#(3)用tune对学习器调参
instance=tune(
  tuner=tnr("grid_search"),
  task=task,
  learner=learner,
  resampling=rsmp("cv",folds=3),
  measures=msr("classif.ce"),
  term_evals=5)
#(4)提取超参数结果
instance$result#调参结果
instance$archive#调参档案
autoplot(instance, type = "surface", cols_x = c("cost", "gamma"))

#2.自动调参器
#(1)创建任务,选择学习器
task=tsk("iris")
learner=lrn("classif.svm",type="C-classification",
            kernel="radial")
#(2)用ps()生成搜索,用auto_turner()创建自动调参器
search_space=ps(cost=p_dbl(0.1,10),gamma=p_int(0,5))
at=auto_tuner(
  tuner=tnr("grid_search"),
  learner=learner,
  search_space=search_space,
  resampling=rsmp("cv",folds=5),
  measure=msr("classif.ce"),
  term_evals=5)
#(3)外层采用4折交叉验证重抽样
rr=resample(task,at,rsmp("cv",folds=4),store_models=T)
rr$aggregate()#总模型的平均性能
rr$score()#每次结果
extract_inner_tuning_results(rr)#内层每次的调参结果
extract_inner_tuning_archives(rr)#内层调参档案
#(4)在全部数据上自动调参,预测新数据
at$train(task)
dat=task$data()[1:5,-1]
at$predict_newdata(dat)

#tune_nested()嵌套调参更方便
rr=tune_nested(
  tuner=tnr("grid_search"),
  task=task,
  learner=learner,
  search_space=search_space,
  inner_resampling=rsmp("cv",folds=5),
  outer_resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=5,
  store_models=T)
rr$aggregate(msr("classif.acc"))#平均准确率
rr$score(msr("classif.acc"))#每个重抽样的准确率 

#查看第1折上的结果
rr$resampling$train_set(1)#训练集索引
rr$learners[[1]]$model#拟合模型
rr$predictions()[[1]]#预测结果

#所有预测合并为一个预测对象
rr$prediction()


#定制搜索空间
search_space=ps(cost=p_dbl(0.1,10),
               kernel=p_fct(c("ployniminal","redial")))
#1.变换
tibble(x=1:20,y=exp(seq(log(3),log(50),length.out=20))) %>% 
  ggplot(aes(x,y))+
  geom_point()

search_space=ps(
  cost=p_dbl(log(0.1),log(10),trafo=function(x)exp(x)),#做log变换使得超参数前密后疏
  kernel=p_fct(c("ployniminal","redial")))

params=generate_design_grid(search_space,resolution=5)
params$transpose() %>% rbindlist()
#2.依赖关系(一个超参数在另一个超参数的特定范围内采有意义)
search_space=ps(
  cost=p_dbl(log(0.1),log(10),trafo=function(x)exp(x)),
  kernel=p_fct(c("polynominal","redial")),
  degree=p_int(1,3,depends=kernel=="polynominal"))

#图学习器调参
task=tsk("pima")
#插补和特征工程
prep=gunion(list(
  po("imputehist"),#直方图插补
  po("missind",affect_columns=selector_type(c("numeric","integer"))))) %>>% #增加是否缺失指示列
  po("featureunion") %>>% #结合
  po("encode") %>>% 
  po("removeconstants")
learners=list(
  knn=lrn("classif.kknn",id="kknn",predict_type="prob"),
  svm=lrn("classif.svm",id="svm",type="C-classification",predict_type="prob"),
  rf=lrn("classif.ranger",id="ranger",predict_type="prob"))
graph=ppl("branch",learners)
graph=prep %>>% graph
glearner=as_learner(graph)#转为为图学习器
glearner$param_set
#启动并行计算
future::plan("multicore") #多核
future::plan("multisession") #多线程
#设定搜索区间
search_space=ps(
  branch.selection=p_fct(c("kknn","svm","ranger")),
  kknn.k=p_int(3,50,logscale=T,depends=branch.selection=="kknn"),
  svm.cost=p_dbl(-1,1,trafo=function(x)10^x,depends=branch.selection=="svm"),
  ranger.mtry=p_int(1,8,depends=branch.selection=="ranger"))
rr=tune_nested(
  tuner=tnr("random_search"),
  task=task,
  learner=glearner,
  inner_resampling=rsmp("cv",folds=3),
  outer_resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=10)
rr$aggregate(list(msr("classif.auc"),msr("classif.ce")))
rr$score(list(msr("classif.auc"),msr("classif.ce")))
extract_inner_tuning_results(rr)#内层调参结果
extract_inner_tuning_archives(rr)#内侧调参档案

#特征选择
#过滤法
#1.基于重要指标
task=tsk("pima")
po=po("imputesample")
task$missings()
new_task=po$train(list(task))[[1]]
new_task$missings()
#给每个特征计算一个重要度指标并排序
filter=flt("auc")
as.data.table(filter$calculate(new_task))

#2.基于学习器的变量重要度
task=tsk("iris")
learner=lrn("classif.ranger",importance="impurity")#激活重要性度量指标
filter=flt("importance",learner=learner)
as.data.table(filter$calculate(task))
#直接构建图学习器
task=tsk("spam")
graph=po("filter",filter=flt("auc"),filter.frac=0.5) %>>%
  po("learner",learner=lrn("classif.rpart"),predict_type="prob")
graph$plot()
glearner=as_learner(graph)
rr=resample(task,glearner,rsmp("cv",folds=5))
rr$aggregate(list(msr("classif.auc"),msr("classif.ce")))
rr$score(list(msr("classif.auc"),msr("classif.ce")))

#包装法(随机选取特征拟合,找到最优)
#1.独立特征选择
#(1)选取任务，划分数据集
task=tsk("pima")
split=partition(task,ratio=0.8)
task$set_row_roles(split$test,"holdout")
#(2)选择学习器
learner=lrn("classif.rpart")
#(3)用fselect()对学习器做特征选择
instance=fselect(
  fselector=fs("rfe"),#递归特征消除法
  task=task,
  learner=learner,
  resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=10,
  store_models=T
)
instance$result#查看选择结果
instance$archive#特征选择档案
task$select(instance$result_feature_set)
learner$train(task)

#2.自动特征选择
task=tsk("pima")
afs=auto_fselector(
  fselector=fs("random_search"),
  learner=lrn("classif.rpart"),
  resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=10)
rr=resample(task,afs,rsmp("cv",folds=5),store_models=T)
rr$aggregate()
rr$score()
extract_inner_fselect_results(rr)
extract_inner_fselect_archives(rr)
#嵌套特征选择
rr=fselect_nested(
  fselector=fs("random_search"),
  task=task,
  learner=lrn("classif.rpart"),
  inner_resampling=rsmp("cv",folds=4),
  outer_resampling=rsmp("cv",folds=5),
  measure=msr("classif.ce"),
  term_evals=10)

#模型解释
dat=tsk("penguins")$data() %>% na.omit()
task=as_task_classif(dat,target="species")
learner=lrn("classif.ranger",predict_type="prob")
learner$train(task)
learner$model

library(iml)
#1.特征效应
mod=Predictor$new(learner,data=dat,y="species")#封装到predictor中
effect=FeatureEffects$new(mod)
effect$plot(features=c("bill_length","bill_depth","flipper_length","body_mass","year"))
#2.夏普利值(单个观测的特殊贡献)
mod=Predictor$new(learner,data=dat,y="species")
shapley=Shapley$new(mod,x.interest=dat[1,])
plot(shapley)
#3.特征重要度
mod=Predictor$new(learner,data=dat,y="species")
imp=FeatureImp$new(mod,loss="ce")
imp$plot()

#DALEX包
library(DALEX)
library(DALEXtra)
fifa[,c("nationality","overall","potential","wage_eur")]=NULL
fifa=fifa %>% 
  mutate(across(.fns=as.numeric))
task=as_task_regr(fifa,target="value_eur")
ranger=lrn("regr.ranger",num.trees=250)
ranger$train(task)
ranger$model
#创建解释器
ranger_exp=explain_mlr3(ranger,data=fifa,y=fifa$value_eur,
                        label="Ranger RF",colorize=F)
#1.特征层面的解释
fifa_vi=model_parts(ranger_exp)#基于排列组合的重要度来计算变量重要度
plot(fifa_vi,max_vars=12,show_boxplots=T)
vars=c("age","movement_reactions","skill_ball_control","skill_dribbling")
fifa_pd=model_profile(ranger_exp,variables=vars)
plot(fifa_pd)+
  scale_y_continuous("估计值(欧元)",
                     labels=scales::dollar_format(suffix="€",prefix=""))+
  ggtitle("所选变量的部分依赖剖析图")
#2.观测层面的解释
ronaldo=fifa["Cristiano Ronaldo",]
ronaldo_bd=predict_parts(ranger_exp,new_observation=ronaldo)
plot(ronaldo_bd)
#SHAP法
ronaldo_shap=predict_parts(ranger_exp,new_observation=ronaldo,type="shap")
plot(ronaldo_shap)+
  scale_y_continuous("估计值(欧元)",
                     labels=scales::dollar_format(suffix="€",prefix=""))
#Ceteris Paribus(相当于一个观测的部分依赖关系图)
ronaldo_cp=predict_profile(ranger_exp,ronaldo,varibles=vars)
plot(ronaldo_cp,variables=vars)+
  scale_y_continuous("Ronaldo身价估计值",
                     labels=scales::dollar_format(suffix="€",prefix=""))
