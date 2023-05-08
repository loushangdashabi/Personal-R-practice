#NRIcalculate Fuction
NRIcalculate=function(m1="dia1",m2="dia2",gold="gold"){
  datanri=datanri[complete.cases(datanri),]
  for (i in 1:length(names(datanri))){
    if (names(datanri)[i]==m1)nm1=as.numeric(i)
    if (names(datanri)[i]==m2)nm2=as.numeric(i)
    if(names(datanri)[i]==gold)ngold=as.numeric(i)
  }
  if(names(table(datanri[,nm1]))[1]!="0" ||
     names(table(datanri[,nm1]))[2]!="1")stop("指标1诊断值不是0和1")
  if(names(table(datanri[,nm2]))[1]!="0" ||
     names(table(datanri[,nm2]))[2]!="1")stop("指标2诊断值不是0和1")
  if(names(table(datanri[,ngold]))[1]!="0" ||
     names(table(datanri[,ngold]))[2]!="1")stop("金标准诊断值不是0和1")
  datanri1=datanri[datanri[,ngold]==1,]
  table1=table(datanri1[,nm1],datanri1[,nm2])
  datanri2=datanri[datanri[,ngold]==0,]
  table2=table(datanri2[,nm1],datanri2[,nm2])
  p1=as.numeric(table1[2,1]/table(datanri[,ngold])[2])
  p2=as.numeric(table1[1,2]/table(datanri[,ngold])[2])
  p3=as.numeric(table2[2,1]/table(datanri[,ngold])[1])
  p4=as.numeric(table2[1,2]/table(datanri[,ngold])[1])
  NRI=round(p1-p2-p3+p4,3)
  z=NRI/sqrt((p1+p2)/table(datanri[,ngold])[2]+(p3+p4)/table(datanri[,ngold])[1])
  z=round(as.numeric(z),3)
  pvalue=round((1-pnorm(abs(z)))*2,3)
  if(pvalue<0.001)pvalue="<0.001"
  result=paste("NRI=",NRI,",z=",z,",p=",pvalue,sep= "")
  return(result)
}

#以上程序不需要更改
#library(foreign)
datanri=as.data.frame(read.csv("dignosisdata.csv")) #请更改数据存在路径
NRIcalculate(m1="v1",m2="v2",gold="gold")#m1为第一诊断变量名，m2第二诊断变量名，gold为金标准

# IDIcalculate Fuction
IDIcalculate=function(m1="v1",m2="v2",gold="gold"){
  dataidi= dataidi [complete.cases(dataidi),]
  for (i in 1:length(names(dataidi))){
    if(names(dataidi)[i]==m1)nm1=as.numeric(i)
    if(names(dataidi)[i]==m2)nm2=as.numeric(i)
    if(names(dataidi)[i]==gold)ngold=as.numeric(i)
  }
  if(names(table(dataidi[,ngold]))[1]!="0" ||
     names(table(dataidi[,ngold]))[2]!="1")stop("金标准诊断值不是0和1")
  logit1=glm(dataidi[,ngold]~dataidi[,nm1],family=binomial(link='logit'),data=dataidi)
  dataidi$pre1=logit1$fitted.values
  logit2=glm(dataidi[,ngold]~dataidi[,nm2],family=binomial(link='logit'),data=dataidi)
  dataidi$pre2=logit2$fitted.values
  dataidi$predif=dataidi$pre1-dataidi$pre2
  dataidi1=dataidi[dataidi[,ngold]==1,]
  dataidi2=dataidi[dataidi[,ngold]==0,]
  p1=mean(dataidi1$pre1)
  p2=mean(dataidi1$pre2)
  p3=mean(dataidi2$pre1)
  p4=mean(dataidi2$pre2)
  IDI=round(p1-p2-p3+p4,3)
  z=IDI/sqrt(sd(dataidi1$predif)/length(dataidi1$predif)+sd(dataidi2$predif)/length(dataidi2$predif))
  z=round(as.numeric(z),3)
  pvalue=round((1-pnorm(abs(z)))*2,3)
  if(pvalue<0.001)pvalue="<0.001"
  result=paste("IDI=",IDI,",z=",z,",p=",pvalue,sep= "")
  return(result)
}

#以上程序不需要更改
#library(foreign)
dataidi=as.data.frame(read.csv("dignosisdata.csv"))#请更改数据存在路径
IDIcalculate(m1="v1",m2="v2",gold="gold")#m1为第一诊断变量名，m2第二诊断变量名，gold为金标准
