library(tidyverse)
df=tibble(
  ID=1:10,
  真实类别=c("Pos","Pos","Pos","Neg","Pos","Neg","Neg","Neg","Pos","Neg"),
  预测概率=c(0.95,0.86,0.69,0.65,0.59,0.52,0.39,0.28,0.15,0.06)
)
knitr::kable(df)
c=0.85
df1 = df %>%
  mutate( 
    预测类别=ifelse(预测概率>=c,"Pos","Neg"),
    预测类别=factor(预测类别,levels = c("Pos","Neg")),
    真实类别=factor(真实类别,levels = c("Pos","Neg"))
  )
cm=table(df1$预测类别,df1$真实类别)
t=cm["Pos",]/colSums(cm)
#参数1是横标目，参数2是纵标目

#泛函式编程
cal_ROC=function(df,c){
  df=df%>%
    mutate( 
      预测类别=ifelse(预测概率>=c,"Pos","Neg"),
      预测类别=factor(预测类别,levels = c("Pos","Neg")),
      真实类别=factor(真实类别,levels = c("Pos","Neg"))
    )
  cm=table(df$预测类别,df$真实类别)
  t=cm["Pos",]/colSums(cm)
  list(TPR=t[[1]],FPR=t[[2]])
}

c=seq(1,0,-0.02)
rocs=map_dfr(c,cal_ROC,df=df)
head(rocs)

rocs%>%
  ggplot(aes(FPR,TPR))+
  geom_line(size=2,color="steelblue")+
  geom_point(shape="diamond",size=4,color="red")+
  theme_bw()
