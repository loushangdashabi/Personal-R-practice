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
autoplot(preds)
