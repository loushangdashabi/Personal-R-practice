library(tidyverse)
library(haven)
load("b11.rda")
load("b13.rda")
load("b15.rda")
b11c=b11 %>%
 filter(qa003!=993 & qa007!=993 & qa011!=993)%>%
  mutate(SBP=(qa003+qa007+qa011)/3,
         DBP=(qa004+qa008+qa012)/3)%>%
  filter(SBP>=0 & DBP>=0)
b11c[b11c$SBP<b11c$DBP,]=b11c[b11c$SBP<b11c$DBP,]%>%
  mutate(SBP=DBP-SBP,DBP=DBP-SBP,SBP=DBP+SBP)
b11c=b11c%>%
  mutate(hypertension=ifelse(SBP>=140|DBP>=90,"Hypertension",
                             ifelse(SBP<120&DBP<80,"Normal","Prehypertension")),
         hypertension_treat=if_else(da011s1==1|da011s2==2,1,0,0))

b11c=within(b11c, {hypertension[da007_1_==1|hypertension_treat==1] <- "Hypertension"})%>%
  mutate(stage=ifelse(SBP>=160|DBP>100,"Stage 2 hypertension",
                      ifelse(SBP>=140|DBP>90,"Stage 1 hypertension",
                             ifelse(SBP<120&DBP<80,"Normal","Prehypertension"))),
         phenotype=ifelse(SBP>=140&DBP<90,"ISH",
                          ifelse(SBP<140&DBP>=90,"IDH",
                            ifelse(SBP<120&DBP<80,"Normal","Prehypertension"))),
         age=2011-ba002_1,gender=rgender,weight=ql002,height=qi002,
         education=ifelse(bd001<=3,1,
                          ifelse(bd001==4,2,
                                 ifelse(bd001==5,3,4))))%>%
  filter(age>=45&gender>0&weight>0&height>0,
         ge004<=10,
         !is.na(ge006),!is.na(ge007),!is.na(ge008),
         ge009_1!=-9999&ge009_1!=9999&!is.na(ge009_1),
         ge009_2!=-9999&ge009_2!=9999&!is.na(ge009_2),
         ge009_3!=-9999&ge009_3!=9999&ge009_3!=-999&!is.na(ge009_3),
         ge009_4!=-9999&ge009_4!=9999&ge009_4!=-999&!is.na(ge009_4),
         ge009_5!=-9999&ge009_5!=9999&ge009_5!=-999&!is.na(ge009_5),
         ge009_6!=-9999&ge009_6!=9999&ge009_6!=-999&!is.na(ge009_6),
         ge009_7!=-9999&ge009_7!=9999&ge009_7!=-999&!is.na(ge009_7),
         ge010_1!=-9999&ge010_1!=-9999&ge010_1!=-999&ge010_1!=9999&!is.na(ge010_1),
         ge010_2!=-9999&ge010_2!=-999&ge010_2!=9999&!is.na(ge010_2),
         ge010_3!=-9999&ge010_3!=-999&ge010_3!=9999&!is.na(ge010_3),
         ge010_4!=-9999&ge010_4!=-999&ge010_4!=9999&!is.na(ge010_4),
         ge010_5!=-9999&ge010_5!=-999&ge010_5!=9999&!is.na(ge010_5),
         ge010_6!=-9999&ge010_6!=-999&ge010_6!=9999&!is.na(ge010_6),
         ge010_7!=-9999&ge010_7!=-999&ge010_7!=9999&!is.na(ge010_7),
         ge010_8!=-9999&ge010_8!=-999&ge010_8!=9999&!is.na(ge010_8),
         ge010_9!=-9999&ge010_9!=-999&ge010_9!=9999&!is.na(ge010_9),
         ge010_10!=-9999&ge010_10!=-999&ge010_10!=9999&!is.na(ge010_10),
         ge010_11!=-9999&ge010_11!=-999&ge010_11!=9999&!is.na(ge010_11),
         ge010_12!=-9999&ge010_12!=-999&ge010_12!=9999&!is.na(ge010_12),
         ge010_13!=-9999&ge010_13!=-999&ge010_13!=9999&!is.na(ge010_13),
         ge010_14!=-9999&ge010_14!=-999&ge010_14!=9999&!is.na(ge010_14),
         )
pce_w=rowSums(b11c[paste0("ge00",6:8)])
pce_m=rowSums(b11c[paste0("ge009_",1:7)])
pce_a=rowSums(b11c[paste0("ge010_",1:14)])
b11c=b11c%>%
  mutate(pce_w,pce_m,pce_a,
         pce=(pce_w*52+pce_m*12+pce_a)/ge004,
         Ln_PCE=log(pce),
         Ln_PCE_t=ifelse(Ln_PCE>0&Ln_PCE<8.2300443101,1,
                         ifelse(Ln_PCE<8.9092352792,2,3)))%>%
  filter(!is.na(da059)&!is.na(da067))%>%
  mutate(smoke=ifelse(da059==1,1,0),
         drink=ifelse(da067==1,1,0),
         wc=qm002)%>%
  filter(!is.na(wc)&wc!=999,
         !(gender=1&wc<40)&!(gender=2&wc<35))%>%
  mutate(wc_=ifelse(gender==1&wc>90,1,
                    ifelse(gender==2&wc>85,1,
                           ifelse(gender==1&wc<=90&wc>=0,0,0))),
         CO=ifelse(gender==2&wc>80,1,
                   ifelse(gender==1&wc>85,1,0)))
b11c[b11c$height<10,]=b11c[b11c$height<10,]%>%
  mutate(height=height*100)
b11c=b11c%>%
  mutate(BMI=weight/(height/100)^2,
         ob=ifelse(BMI>=0&BMI<18,1,
                           ifelse(BMI<24,1,
                                  ifelse(BMI<28,2,3))),
         bodyweight=ifelse(BMI>=0&BMI<24,"NW",
                           ifelse(BMI<28,"OverW","Obesity")),
         status=ifelse(CO==0&bodyweight=="NW","NWNCO",
                       ifelse(CO==0&(bodyweight=="OverW"|bodyweight=="Obesity"),"AWNCO",
                              ifelse(CO==1&bodyweight=="NW","NWCO","AWCO"))))

b11c_sub=b11c%>%
  filter(hypertension_treat!=1)
b11c_stage_pre=b11c_sub%>%
  filter(stage=="Normal"|stage=="Prehypertension")

anal=b11c_sub%>%
  select(id,SBP,DBP,hypertension,stage,phenotype,age,gender,weight,height,
         education,Ln_PCE_t,smoke,drink,wc_,CO,weight,height,BMI,ob,bodyweight,status)%>%
  mutate(stage=factor(stage,levels=c("Normal","Prehypertension","Stage 1 hypertension","Stage 2 hypertension"),ordered=TRUE),
         status=factor(status,levels = c("NWNCO","AWNCO","NWCO","AWCO")))


library(skimr)
skim(anal)#数据概况
contrasts(anal$status)
table(anal$status)

#构建模型
log_reg=glm(stage~status+age+gender+education+Ln_PCE_t+smoke+drink,data=anal,
            family=binomial())


library(broom)
glance(log_reg)
tidy(log_reg)
exp(cbind(OR=coef(log_reg),confint(log_reg)))

library(tableone)
ShowRegTable(log_reg)#*****
write.csv(x,file = "123.csv")

predprob=predict(log_reg,newdata = anal,type = "response")
library(pROC)
rocs=roc(response=anal$status,predictor=predprob)
plot(rocs,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=TRUE,
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue",
     print.thres=TRUE,
     legacy.axes=TRUE)