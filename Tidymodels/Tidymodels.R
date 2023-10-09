library(tidymodels)

#The Ames Housing Data
library(modeldata)
data(ames)
#or
data(ames,package = "modeldata")

#查看冲突的包
tidymodels_prefer()

#可视化ames的目标变量:成交房价(直方图显示右偏)
ggplot(ames,aes(Sale_Price))+
  geom_histogram(bins=50,col="white")+
  theme_bw()

#log变换,趋近正态(建模更好,但会损失解释性)
ggplot(ames,aes(Sale_Price))+
  geom_histogram(bins=50,col="white")+
  scale_x_log10()+
  theme_bw()

ames = ames %>% 
  mutate(Sale_Price=log10(Sale_Price))

#数据集划分(简单随机抽样)
set.seed(501)
ames_split=initial_split(ames,prop=0.8)#80%用于训练集
ames_train=training(ames_split)
ames_test=testing(ames_split)

#如果数据是不均衡的,随机抽样不好,可以使用分层抽样
#本数据是右偏的,右侧的(即极端贵的房子)可能在训练集中得不到体现
set.seed(501)
ames_split=initial_split(ames,prop=0.8,strata=Sale_Price)
ames_train=training(ames_split)
ames_test=testing(ames_split)
#通常情况下分层不会有缺点,但只支持一个分层变量

#在时间序列数据中,随机抽样不好,仅表示开头的prop作为训练集(需提前排列好)
initial_time_split()

#划分验证集(通常是训练集的一个子集,减少过拟合风险)
set.seed(501)
ames_val_split=initial_validation_split(ames,prop = c(0.6,0.2))
ames_train=training(ames_val_split)
ames_test=testing(ames_val_split)
ames_val=validation(ames_val_split)

#多级数据(如随访调查),划分数据集时应当将一个受试者的所有观测整体划分

#建模时只能使用训练集数据,否则会造成泄露


#parsnip(接口包)
linear_reg() %>% 
  set_engine("lm")#使用stat包的lm
linear_reg() %>% 
  set_engine("glmnet")#使用glmnet包
linear_reg() %>% 
  set_engine("stan")#使用stan包

#查看parsnip包如何将参数传入模型
linear_reg() %>% 
  set_engine("lm") %>% translate()
linear_reg(penalty=1) %>% 
  set_engine("glmnet") %>% translate()#penalty参数是必须的
linear_reg() %>% 
  set_engine("stan") %>% translate()

#构建模型
lm_model=linear_reg() %>% 
  set_engine("lm")
#用formula
lm_form_fit=lm_model %>% 
  fit(Sale_Price~Longitude+Latitude,data=ames_train)
#直接引入参数
lm_xy_fit=lm_model %>% 
  fit_xy(
    x=ames_train %>% select(Longitude,Latitude),
    y=ames_train %>% pull(Sale_Price)
  )

#查看parsnip的参数名称如何对应
rand_forest(trees=1000,min_n=5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

show_engines('rand_forest')#查看支持随机森林的engine

#查看拟合的更多信息
rand_forest(trees=1000,min_n=5) %>% 
  set_engine("ranger",verbose=T) %>% 
  set_mode("regression")

#使用模型结果
lm_form_fit %>% extract_fit_engine()
#结果还可以用于其他普通方法
lm_form_fit %>% extract_fit_engine() %>% vcov()

#查看模型结果,但不tidy
model_res=lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()
param_set=coef(model_res)
#使用broom包,查看整洁结果
tidy(lm_form_fit)

#做出预测
predict(lm_form_fit,new_data = ames_test)

ames_test %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit,new_data = ames_test)) %>% 
  bind_cols(predict(lm_form_fit,new_data = ames_test,type="pred_int"))

#parsnip包的统一接口
tree_model=
  decision_tree(min_n=2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")
tree_fit=
  tree_model %>% 
  fit(Sale_Price~Longitude+Latitude,data=ames_train)
ames_test %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit,ames_test))

#workflow
lm_model =
  linear_reg() %>% 
  set_engine("lm")
lm_wflow=
  workflow() %>% 
  add_model(lm_model)#创建workflow,model必须是parsnip model

#模型简单时可以用标准formula作为预处理器
lm_wflow =
  lm_wflow %>% 
  add_formula(Sale_Price~Longitude+Latitude)

#fit()来拟合模型
lm_fit=fit(lm_wflow,ames_train)

#predict()预测结果
ames_test %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_fit,ames_test))

#workflow中的模型和预处理器都可以删除或更新
lm_fit %>% 
  update_formula(Sale_Price~Latitude)#拟合结果会被删除,因为formula不一致

#添加原始变量(类似于dplyr的操作)
lm_wflow=
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price,predictors = c(Longitude,Latitude))

#支持tidyselect
lm_wflow=
  lm_wflow %>% 
  remove_variables() %>% 
  add_variables(outcome = Sale_Price,predictors = c(ends_with("tude")))
#选择所有特征
lm_wflow=
  lm_wflow %>% 
  remove_variables() %>% 
  add_variables(outcome = Sale_Price,predictors = everything())

fit(lm_wflow,ames_train)

#特殊包的formula(如lme4的随机效应模型)
library(lme4)
data(Orthodont,package="nlme")
lmer(distance~Sex+(age|Subject),data=Orthodont)
model.matrix(distance ~ Sex + (age | Subject), data = Orthodont)#标准formula无法处理age|Subject

#workfolw可以同时接受variable和formula
library(multilevelmod)
multilevel_spec=
  linear_reg() %>% set_engine("lmer")
multievel_wflow=
  workflow() %>% 
  add_variables(outcomes=distance,predictors=c(Sex,age,Subject)) %>% 
  add_model(multilevel_spec,
            formula = distance ~ Sex + (age | Subject))#将formula作为model的一部分传入,而非add_formula
multilevel_fit=fit(multievel_wflow,Orthodont)

#也可以做生存分析
library(censored)
paramtric_spec =survival_reg()
paramtric_wflow=
  workflow() %>% 
  add_variables(outcomes = c(fustat,futime),predictors = c(age,rx)) %>% 
  add_model(paramtric_spec,
            formula = Surv(futime,fustat)~age+strata(rx))
paramtric_fit=fit(paramtric_wflow,ovarian)
paramtric_fit %>% tidy()

#一次创建多个工作流
location=list(
  longitude=Sale_Price~Longitude,
  latitude=Sale_Price~Latitude,
  coords=Sale_Price~Longitude+Latitude,
  neiborhood=Sale_Price~Neighborhood
)
library(workflowsets)
location_models=workflow_set(preproc=location,models=list(lm=lm_model))#可以多个formula和多个model交叉
extract_workflow(location_models,id="longitude_lm")#提取其中一个workflow 
location_models=
  location_models %>% 
  mutate(fit=map(info,~fit(.x$workflow[[1]],ames_train)))

#评估测试集
final_lm_res=last_fit(lm_wflow,ames_split)#注意用split对象,initial_split()
fitted_lm_wflow=extract_workflow(final_lm_res)#可以从结果中取用workflow
collect_metrics(final_lm_res)#提取性能指标
collect_predictions(final_lm_res)


#特征工程recipe包
lm(Sale_Price~Neighborhood+log10(Gr_Liv_Area)+Year_Built+Bldg_Type,data=ames)
#recipe包做类似的事,但不立即执行
simple_ames=
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% #通过step_*()来操作数据
  step_dummy(all_nominal_predictors())#将所有分类数据转化为虚拟变量

#recipe对象可以添加到workflow中
lm_wflow %>% 
  add_recipe(simple_ames)
#要求workflow中尚无预处理器,若有则需要先删除
lm_wflow =
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)
lm_fit=fit(lm_wflow,ames_train)
predict(lm_fit,ames_test)
#拟合后获取recipe或model
lm_fit %>% 
  extract_recipe()
lm_fit %>% 
  extract_fit_parsnip() %>% #返回parsnip对象,即model
  tidy()

#recipe能实现的功能
#1.用数字格式编码定性数据
simple_ames=
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data=ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neiborghood,threshold = 0.01) %>% #将Neiborghood中排序最后1%的类别合并为other
  step_dummy(all_nominal_predictors())#将因子的第一个水平值作为ref
#2.交互项
ggplot(ames_train,aes(x=Gr_Liv_Area,y=10^Sale_Price))+
  geom_point(alpha=0.2)+
  facet_wrap(~Bldg_Type)+
  geom_smooth(method=lm,formula=y~x,se=F,color="lightblue")+
  scale_x_log10()+
  scale_y_log10()+
  labs(x="Gross Living Area",y="Sale Price (USD)")#x,y的关系与Bldg_type相关(交互项)
simple_ames=
  recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type,
         data=ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neighborhood,threshold=0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  #必须先创建好虚拟变量再做交互,否则baseR会先生成虚拟变量,可能产生意想不到的错误
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_"))

#样条函数
library(patchwork)
library(splines)
plot_smoother=function(deg_free){
  ggplot(ames_train,aes(Latitude,10^Sale_Price))+
    geom_point(alpha=.2)+
    scale_y_log10()+
    geom_smooth(
      method="lm",
      formula=y~ns(x,df=deg_free),
      color="lightblue",
      se=TRUE
    )+
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}
(plot_smoother(2)+plot_smoother(5))/(plot_smoother(20)+plot_smoother(100))

#recipe实现自然样条
recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type+Latitude,
       data=ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neighborhood,threshold=0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_")) %>% 
  step_ns(Latitude,deg_free=20)#增加一定的非线性，有助于优化


#特征提取
recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type+Latitude,
       data=ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neighborhood,threshold=0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_")) %>% 
  step_ns(Latitude,deg_free=20) %>% 
  step_pca(matches("(SF$)|(Gr_Liv)"))#将SF结尾的,和包含Gr_Liv的特征做主成分回归

#行采样,对不平衡数据进行二次采样,上采样下采样
recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type+Latitude,
       data=ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neighborhood,threshold=0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_")) %>% 
  step_ns(Latitude,deg_free=20) %>% 
  step_pca(matches("(SF$)|(Gr_Liv)")) %>% 
  step_downsample(Sale_Price)#基于themis包

#一般变换,强烈建议直接在数据内操作而不要在recipe中操作
step_mutate()

#整理
ames_rec=
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neighborhood,threshold=0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_")) %>% 
  step_ns(Latitude,Longitude,deg_free=20)
#调用tidy(),会给出配方步骤摘要
tidy(ames_rec)

#设置step的id,使其在tidy()中容易识读,特别是多次用到同一个方法时
ames_rec=
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_other(Neighborhood,threshold=0.01,id="Other_Neighbor") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_")) %>% 
  step_ns(Latitude,Longitude,deg_free=20)
tidy(ames_rec)
#fit the workflow
lm_wflow=
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)
lm_fit=fit(lm_wflow,ames_train)

#可以再次调用tidy()方法和我们指定的id标识符，以获得应用step_other()的结果
estimated_recipe=
  lm_fit %>% 
  extract_recipe(estimated=TRUE)
tidy(estimated_recipe,id="Other_Neighbor")#可以看到step_other步中哪些没有被合并
tidy(estimated_recipe,number=2)#知道是第几步也可以直接选取


#Column Roles,给那些非特征或结局的列设置Role
ames_rec %>% 
  update_role(address,new_role="street address")
