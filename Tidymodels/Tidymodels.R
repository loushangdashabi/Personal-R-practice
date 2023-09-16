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
