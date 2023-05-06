library(tidyverse)
#概率分布
tibble(
  x=seq(-4,4,length.out=100),
  `μ=0,σ=0.5`=dnorm(x,0,0.5),
  `μ=0,σ=1`=dnorm(x,0,1),
  `μ=0,σ=2`=dnorm(x,0,2),
  `μ=-2,σ=1`=dnorm(x,-2,1)
) %>% 
  pivot_longer(-x,names_to="参数",values_to="p(x)") %>% 
  ggplot(aes(x,`p(x)`,color=参数))+ 
  geom_line()

#抽样方法
#简单抽样
(X=str_c(1:6,"点"))
sample(X,1)
sample(X,10,replace=T)
sample(c("正","反"),10,replace=T)
sample(c("正","反"),10,replace=T,prob=c(0.9,0.1))
df=as_tibble(iris)
df %>% 
  slice_sample(n=5)
#分层抽样
df %>% 
  group_by(Species) %>% 
  slice_sample(n=5)
df %>% 
  group_by(Species) %>% 
  slice_sample(prop=0.1)
#整群抽样
df %>% 
  filter(Species %in% sample(levels(Species),2))
#多阶段抽样
df %>% 
  filter(Species %in% sample(levels(Species),2)) %>% 
  group_by(Species) %>% 
  slice_sample(n=3)

#随机试验
#概率计算
prod(3:7)#连乘
factorial(5)#阶乘
choose(10,6)#组合数
combn(LETTERS[1:4],2)#取出所有可能组合


