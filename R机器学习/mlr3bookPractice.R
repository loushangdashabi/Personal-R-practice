library(tidyverse)
library(mlr3verse)
#简单建模示例(决策树)
task=tsk("penguins")
split=partition(task)
learner=lrn("classif.rpart")
learner$train(task,row_ids=split$train)
learner$model
prediction=learner$predict(task,row_ids=split$test)
prediction$score(msr("classif.acc"))

#复杂建模
library(mlr3verse)
library(mlr3pipelines)
library(mlr3benchmark)
tasks=tsks(c("breast_cancer","sonar"))
tuned_rf=auto_tuner(
  tnr("grid_search",resolution=5),
  lrn("classif.ranger",num.trees=to_tune(200,500)),
  rsmp("holdout")
)
tuned_rf=pipeline_robustify(NULL,tuned_rf,TRUE) %>>% 
  po("learner",tuned_rf)
stack_lrn=ppl(
  "stacking",
  base_learners=lrns(c("classif.rpart","classif.kknn")),
  lrn("classif.log_reg")
)
stack_lrn=pipeline_robustify(NULL,stack_lrn,TRUE) %>>%
  po("learner",stack_lrn)
learners=c(tuned_rf,stack_lrn)
bm=benchmark(benchmark_grid(tasks,learners,rsmp("holdout")))

bma=bm$aggregate(msr("classif.acc"))[,c("task_id","learner_id","classif.acc")]
bma$learner_id=rep(c("RF","Stack"),2)
bma
as.BenchmarkAggr(bm)$friedman_test()

#data.table示例
library(data.table)
dt=data.table(x=1:6,y=rep(letters[1:3],each=2))
dt[,mean(x),by="y"]#根据y分组统计

as.data.table(mlr_learners)#查看所有学习器
as.data.table(mlr_tasks)

#从已有task中选择一个
task_mtcars=tsk("mtcars")
task_mtcars$help()#查看任务详情
#or
help("mlr_tasks_mtcars")

#将新数据设为task
data("mtcars",package="datasets")
str(mtcars)
mtcars_subset=subset(mtcars,select=c("mpg","cyl","disp"))
task_mtcars=as_task_regr(mtcars_subset,target="mpg",id="cars")
autoplot(task_mtcars,type="pairs")#可视化

#检索维度
c(task_mtcars$nrow,task_mtcars$ncol)
#查看目标或对象名
list(task_mtcars$feature_names,task_mtcars$target_names)
#如果设置了ID参数,则每行具有独立的自然数编码
head(task_mtcars$row_ids)

#查看任务数据集
task_mtcars$data()
#查看若干条目
task_mtcars$data(rows=c(1,5,10),col="mpg")
#提取数据集
as.data.table(task_mtcars)
summary(as.data.table(task_mtcars))
#筛选观测或特征
task_mtcars_small=tsk("mtcars")
task_mtcars_small$select(c("am","carb"))
task_mtcars_small$filter(2:4)
task_mtcars_small$data()
#或者通过row_id筛选
keep=task_mtcars_small$row_ids[2]
task_mtcars_small$filter(keep)
#增加新行或列
task_mtcars_small$rbind(
  data.frame(mpg=23,am=0,carb=3))

#学习器
learner_rpart=lrn("regr.rpart")
learner_rpart$feature_types#查看学习器可以处理的特征类型
learner_rpart$packages#查看依赖的包
learner_rpart$properties#查看学习器具有的能力(如填补缺失,计算重要性等)
learner_rpart$predict_type#查看可能的预测类型
learner_rpart$param_set#查看超参数
learner_rpart

#划分测试集与训练集
splits=partition(task_mtcars)
splits
#训练模型
learner_rpart$train(task_mtcars,splits$train)
learner_rpart$model#查看模型

#超参数
learner_rpart$param_set#查看超参数
learner_rpart$param_set$values#查看目前超参数的值
#简化一下,选取了部分超参数
all_params=learner_rpart$param_set
subset=all_params$subset(c("cp","keep_model","minsplit","xval"))
#部分学习器的超参数还存在parents列,表示该超参数依赖另一个超参数
lrn("classif.svm")$param_set
lrn("classif.svm")$param_set$deps#查看哪些超参数存在依赖
lrn("classif.svm")$param_set$deps$cond
#设置超参数
subset$values$minsplit=10#改单一超参数
subset$values=list(#改所有超参数
  cp=0.05,
  keep_model=T,
  minsplit=10,
  xval=5)
subset$set_values(minsplit=10)#只改给出的超参数(更好)
learner_rpart=lrn("regr.rpart",minsplit=10)#构建学习器的时候也可以修改超参数
learner_rpart$train(task_mtcars,splits$train)
learner_rpart$model

#预测
predictions=learner_rpart$predict(task_mtcars,splits$test)
as.data.table(predictions)
#用新数据预测
mtcars_new=data.table(cyl = c(5,6),
                      disp = c(100,120),
                      hp = c(100,150),
                      drat = c(4,3.9),
                      wt = c(3.8,4.1),
                      qsec = c(18,19.5),
                      vs = c(1,0),
                      am = c(1,1),
                      gear = c(6,4),
                      carb = c(3,5))
predictions=learner_rpart$predict_newdata(mtcars_new)
predictions$response#直接返回结果
#结果可视化
predictions=learner_rpart$predict(task_mtcars,splits$test)
autoplot(predictions)

#改变预测类型
learner_lm=lrn("regr.lm")
learner_lm$predict_type="se"#增加一项标准差
learner_lm$train(task_mtcars,splits$train)
predictions=learner_lm$predict(task_mtcars,splits$test)
predictions

#模型评估
mlr_measures$keys("regr")#查看回归任务的评估方法
measure=msr("regr.rmse")
measures=msrs(c("regr.rmse","regr.sse"))

measure=msr("regr.rmse")
predictions$score(measure)#评估预测精度

measures=msrs(c("regr.rmse","regr.sse"))
predictions$score(measures)#多个指标

#带有超参数的measure
task_mtcars=tsk("mtcars")
splits=partition(task_mtcars)
learner_rpart=lrn("regr.rpart",minsplit=10)
learner_rpart$train(task_mtcars,splits$train)
predictions=learner_rpart$predict(task_mtcars,splits$test)
measure=msr("selected_features")#只能用于支持select_feature properties的learner
predictions$score(measure,task=task_mtcars,learner=learner_rpart)
#设置measure的超参数
measure=msr("selected_features",normalize=T)
predictions$score(measure,task=task_mtcars,learner=learner_rpart)

#分类任务
as.data.table(mlr_tasks)[task_type=="classif"]#查看所有内置分类任务
task_penguins=tsk("penguins")
#查看target的levels
unique(task_penguins$data(cols="species"))
#任务可视化
task_penguins_small=task_penguins$clone()
task_penguins_small$select(head(task_penguins_small$feature_names,1))#筛选特征,防止图太小
autoplot(task_penguins_small,type="pairs")

#建立分类任务
learner_rpart=lrn("classif.rpart")
splits=partition(task_penguins)
learner_rpart$train(task_penguins,splits$train)
learner_rpart$model
predictions=learner_rpart$predict(task_penguins,splits$test)
#可视化
autoplot(predictions)

#改变预测种类
learner_rpart$predict_type="prob"
learner_rpart$train(task_penguins,splits$test)
predictions=learner_rpart$predict(task_penguins,splits$test)

#分类任务评估
mlr_measures$keys("classif")
measure=msr("classif.acc")
measure$predict_type
predictions$score(measure)
predictions$confusion#混淆矩阵

#二分类任务
data("Sonar",package="mlbench")
task_sonar=as_task_classif(Sonar,target="Class",positive="R")
task_sonar$positive="M"#更改阳性水平
splits=partition(task_sonar)
learner=lrn("classif.kknn")
learner$train(task_sonar,splits$train)
predictions=learner$predict(task_sonar,splits$test)

#确定阈值(二分类)
task_credit=tsk("german_credit")
splits=partition(task_credit)
learner=lrn("classif.rpart",predict_type="prob")
learner$train(task_credit)
predictions=learner$predict(task_credit)
predictions$confusion
autoplot(predictions)#由于数据不平衡,learner过度预测了多数类

predictions$set_threshold(0.7)#在不改变超参数的前提下解决这个问题
autoplot(predictions)

#确定阈值(多分类)
task=tsk("zoo")
learner=lrn("classif.rpart",predict_type="prob")
learner$train(task)
preds=learner$predict(task)
autoplot(preds)#amphibian和insect两类比例太小,导致预测中完全没了,所以适当降低阈值

new_tresh=c(0.5,0.5,0.5,0.5,0.4,0.4,0.5)
names(new_tresh)=task$class_names
preds$set_threshold(new_tresh)
autoplot(preds)

#Column Roles
task_mtcars_small=tsk("mtcars")
task_mtcars_small$select(c("am","carb"))
task_mtcars_small$filter(2:4)
task_mtcars_small$col_roles[c("feature","target")]#查看任务的特征列和目标列
names(task_mtcars_small$col_roles)#查看所有支持的Column Roles
#feature target
#name: 设置行名,用于作图
#order: $data()返回的数据,以order列排序
#group: 重抽样时,同一个组的变量会被放入训练集或测试集
#stratum: 分层
#weight: 权重,感兴趣的类是少数类,增加权重可以提高性能
#列也可以没有任何角色,$select()和$filter()就是将列或行忽略,而不是在原数据中删除
task_mtcars_small$backend#查看被忽略的行列
#设置roles
task_mtcars_small$set_col_roles("mpg",roles="target")#设置单一列
task_mtcars_small$col_roles(c(...))#设置所有列
#还原unmodified的task
new_task=task_mtcars_small$clone()

#feature role example
task_mtcars_small$set_col_roles("cyl",roles="feature")#将被筛选掉的cyl列加回来
task_mtcars_small$feature_names
task_mtcars_small$data()
#weights role example
task_cancer=tsk("breast_cancer")
summary(task_cancer$data()$class)
cancer_data=task_cancer$data()
cancer_data$weights=ifelse(cancer_data$class=="malignant",2,1)
task_cancer=as_task_classif(cancer_data,target="class")
task_cancer$set_col_roles("weights",roles="weight")
task_cancer$col_roles[c("feature","target","weight")]
#其他column role
#Clustering：在特征空间中识别相似组的无监督任务
#Survival: target是到事件发生的时间
#Density: 一个无监督的任务，根据观察到的数据(作为一个数值向量或一列矩阵样的对象)估计不可检测的潜在概率分布

#其他学习器
#mlr3包:
#featureless classifier:"classif.featureless",预测训练集中最常出现的标签
#featureless regressor:"regr.featureless",预测训练集中目标值的平均值
#分类树:"classif.rpart"
#回归树:"regr.rpart"

#mlr3learner包:
#线性回归:"regr.lm"
#逻辑回归:"regr.log_reg"
#惩罚广义线性模型:"regr.glmnet""classif.glmnet"
#内置优化的惩罚参数:"regr.cv_glmnet""classif.cv_glmnet"
#核化最近邻回归/分类:"regr.kknn""classif.kknn"
#高斯回归:"regr.km"
#线性/多元判别分析:"classif.lda""classif.qda"
#朴素贝叶斯分析:"classif.naive_bayes"
#支持向量机:"regr.svm""classif.svm"
#梯度提升:"regr.xgboost""classif.xgboost"
#随机森林:"regr.ranger""classif.ranger"

#查看所有学习器
as.data.table(mlr_learners)
as.data.table(mlr_learners)[task_type=="regr"]#查看所有回归任务
as.data.table(mlr_learners)[task_type=="regr"&sapply(predict_types,function(x)"se"%in%x)]#查看支持预测标准差的回归任务
as.data.table(mlr_learners)[task_type=="classif"&sapply(properties,function(x)"missings"%in%x)]#查看支持缺失值的分类任务

#exercise
task=tsk("sonar")
splits=partition(task,ratio=0.8)
learner=lrn("classif.rpart",predict_type="prob")
learner$train(task,row_ids=splits$train)
predictions=learner$predict(task,row_ids=splits$test)
predictions$score(msr("classif.ce"))
predictions$score(c(msr("classif.tpr"),msr("classif.fpr"),
                    msr("classif.tnr"),msr("classif.fnr")))

predictions$set_threshold(0.9)#提高阈值,减少假阳性,增加假阴性
predictions$score(c(msr("classif.tpr"),msr("classif.fpr"),
                    msr("classif.tnr"),msr("classif.fnr")))


#重抽样
task=tsk("penguins")
learner=lrn("classif.rpart",predict_type="prob")
#选择measurement
msr_tbl=as.data.table(mlr_measures)
msr_tbl[1:5,.(key,label,task_type)]
msr_tbl[1:5,.(key,packages,predict_type,task_properties)]
#留出法
resampling=rsmp("holdout")
rr=resample(task,learner,resampling)
rr$aggregate(msr("classif.acc"))
#benchmark
lrns=c(learner,lrn("classif.featureless"))
d=benchmark_grid(task,lrns,resampling)
bmr=benchmark(design=d)
acc=bmr$aggregate(msr("classif.acc"))
acc[,.(task_id,learner_id,classif.acc)]

#查看所有重抽样方法
as.data.table(mlr_resamplings)
#构建重抽样
resampling=rsmp("holdout")
#改变留出法的比例
resampling=rsmp("holdout",ratio=0.8)
resampling$param_set$values=list(ratio=0.5)
#k折交叉验证
resampling=rsmp("cv",folds=10)

#实例化重抽样策略
resampling=rsmp("holdout",ratio=0.8)
resampling$instantiate(task)#实例化
train_ids=resampling$train_set(1)
test_ids=resampling$test_set(1)
str(train_ids)
str(test_ids)

#执行
resampling=rsmp("cv",folds=4)
rr=resample(task,learner,resampling)
print(rr)
as.data.table(rr)
acc=rr$score(msr("classif.acc"))
acc[,.(iteration,classif.acc)]
rr$aggregate(msr("classif.acc"))#默认为macro宏平均
rr$aggregate(msr("classif.acc",average="micro"))#微平均

#检查重抽样结果
rrdt=as.data.table(rr)
rrdt$prediction
all.equal(rrdt$prediction,rr$predictions())#两者结果一致
pred=rr$prediction()
pred$score(msr("classif.acc"))

#保存每次迭代的模型
rr=resample(task,learner,resampling,store_models=T)
rr$learners[[1]]$model
#查看每个变量的重要性在各次迭代中有无区别
lapply(rr$learners,function(x) x$model$variable.importance)
map(rr$learners,~.$model$variable.importance)

#自定义重抽样
resampling=rsmp("custom")
resampling$instantiate(task,
                       train=list(c(1:50,151:333)),
                       test=list(51:150))
str(resampling$train_set(1))
str(resampling$test_set(1))

custom_cv=rsmp("custom_cv")
folds=as.factor(rep(1:4,each=task$nrow/4))
custom_cv$instantiate(task,f=folds)

#分层与分组重抽样
#分组
task_grp=tsk("penguins")
task_grp$col_roles$group="year"
r=rsmp("loo")
r$instantiate(task_grp)

table(task_grp$data(cols="year"))

table(task_grp$data(rows=r$test_set(1),cols="year"))
table(task_grp$data(rows=r$test_set(2),cols="year"))
table(task_grp$data(rows=r$test_set(3),cols="year"))#每个测试集只包含一个年份

#分层(数据严重不平衡时用)
prop.table(table(task$data(cols="species")))

r=rsmp("cv",folds=3)
r$instantiate(task)
prop.table(table(task$data(rows=r$test_set(1),cols="species")))
prop.table(table(task$data(rows=r$test_set(2),cols="species")))
prop.table(table(task$data(rows=r$test_set(3),cols="species")))#三次重抽样中分布不同

task_str=tsk("penguins")
task_str$col_roles$stratum="species"
r=rsmp("cv",folds=3)
r$instantiate(task_str)
prop.table(table(task$data(rows=r$test_set(1),cols="species")))
prop.table(table(task$data(rows=r$test_set(2),cols="species")))
prop.table(table(task$data(rows=r$test_set(3),cols="species")))#分布一致
#用set_col_roles设置
task_str$set_col_roles("species",remove_from="stratum")
task_str$set_col_roles("species",add_to="stratum")
#两个以上分层因素,可显示矩阵
task_str$set_col_roles("year",add_to="stratum")
task_str$strata
table(task$data(cols=c("species","year")))

#可视化重抽样
resampling=rsmp("bootstrap")
rr=resample(task,learner,resampling)
autoplot(rr,measure=msr("classif.acc"),type="boxplot")
autoplot(rr,measure=msr("classif.acc"),type="histogram")

task$select(c("bill_length","flipper_length"))
resampling=rsmp("cv",folds=4)
rr=resample(task,learner,resampling,store_models=T)
autoplot(rr,type="prediction")#只有两个特征,可绘制二维预测曲面

#benchmarking
#构建benchmark
tsks=tsks(c("german_credit","sonar","breast_cancer"))
lrns=lrns(c("classif.ranger","classif.rpart","classif.featureless"),predict_type="prob")
rsmp=rsmp("cv",folds=5)
design=benchmark_grid(tsks,lrns,rsmp)
mlr3misc::ids(design$task)
design[mlr3misc::ids(task)=="sonar",]
bmr=benchmark(design)#运行benchmark
acc=bmr$aggregate(msr("classif.acc"))
acc[,.(task_id,learner_id,classif.acc)]#每个learner,每个task的acc
acc[,list(mean_accuracy=mean(classif.acc)),by="learner_id"]#每个learner的平均acc
#利用mlr3benchmark包排序
library(mlr3benchmark)
bma=as_benchmark_aggr(bmr,measures=msr("classif.acc"))
bma$rank_data(minimize=F)

#查看benchmark对象
bmrdt=as.data.table(bmr)
names(bmrdt)
#排序好的benchmark可以用索引读取
rr1=bmr$resample_result(1)
rr2=bmr$resample_result(2)
#可视化
autoplot(bmr,measure=msr("classif.acc"))

#统计检验(Friedman-Nemenyi test)
bma=as_benchmark_aggr(bmr,measures=msr("classif.acc"))
bma$friedman_posthoc()
autoplot(bma,type="cd")

#ROC分析
task=tsk("german_credit")
learner=lrn("classif.ranger",predict_type="prob")
splits=partition(task,ratio=0.8)
learner$train(task,splits$train)
pred=learner$predict(task,splits$test)
pred$confusion
mlr3measures::confusion_matrix(truth=pred$truth,
                               response=pred$response,positive=task$positive)
#改变阈值,阈值越高,真阳性率越高,但假阴性率也越高
pred$set_threshold(0.99)
mlr3measures::confusion_matrix(truth=pred$truth,
                               response=pred$response,positive=task$positive)
pred$set_threshold(0.01)
mlr3measures::confusion_matrix(truth=pred$truth,
                               response=pred$response,positive=task$positive)
#绘制ROC曲线
thresholds=sort(pred$prob[,1])
rocvals=data.table::rbindlist(lapply(thresholds,function(t){
  pred$set_threshold(t)
  data.frame(
    threshold=t,
    FPR=pred$score(msr("classif.fpr")),
    TPR=pred$score(msr("classif.tpr"))
  )
}))
#手动画
ggplot(rocvals,aes(FPR,TPR))+
  geom_point()+
  geom_path(color="darkred")+
  geom_abline(linetype="dashed")+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  labs(
    title="Manually constructed ROC curve",
    x="1-Specificity (FPR)",
    y="Sensitivity (TPR)"
  )+
  theme_bw()
#自动画
autoplot(pred,type="roc")
autoplot(pred,type="prc")#对于不均衡的数据,prc好于roc
autoplot(pred,type="threshold",measure=msr("classif.fpr"))
autoplot(pred,type="threshold",measure=msr("classif.acc"))
#重抽样后可视化ROC
rr=resample(tsk("german_credit"),lrn("classif.ranger",predict_type="prob"),
            rsmp("cv",folds=5))
autoplot(rr,type="roc")
autoplot(rr,type="prc")
#benchmark可视化ROC
design=benchmark_grid(
  tsk("german_credit"),
  lrns(c("classif.rpart","classif.ranger","classif.kknn","classif.featureless"),predict_type="prob"),
  resamplings=rsmp("bootstrap")
)
bmr=benchmark(design)
autoplot(bmr,type="roc")
autoplot(bmr,type="prc")

#exercise
task=tsk("spam")
learners=lrns(c("classif.ranger","classif.log_reg","classif.xgboost"),predict_type="prob")
resampling=rsmp("cv",folds=5)
design=benchmark_grid(task,learners,resampling)
bmr=benchmark(design)
bmr$aggregate(msr("classif.acc"))
autoplot(bmr,type="roc")
autoplot(bmr,type="prc")

task=tsk("penguins_simple")
