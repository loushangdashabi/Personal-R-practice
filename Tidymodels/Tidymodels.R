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
