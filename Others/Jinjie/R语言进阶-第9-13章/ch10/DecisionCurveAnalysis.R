#install.packages("devtools")
#library(devtools) 
#install_github("mdbrown/DecisionCurve")

#library(rmda)
#library(DecisionCurve)

## 载入包 载入数据
#library(DecisionCurve)
#install.packages("rmda")
library(rmda)
Data<- read.csv('2.20.Framingham.csv',sep = ',')

## DCA运算
simple<- decision_curve(chdfate~scl,data = Data, family = binomial(link ='logit'),
         thresholds= seq(0,1, by = 0.01),
         confidence.intervals = 0.95,study.design = 'case-control',
         population.prevalence = 0.3) 
#decision_curve()函数中，family =binomial(link = ‘logit’)是使用logistic回归来拟合模型。
#threshold设置横坐标阈概率的范围，一般是0 ~ 1；但如果有某种具体情况，大家一致认为Pt达到某个值以上，
#比如40%，则必须采取干预措施，那么0.4以后的研究就没什么意义了，可以设为0 ~ 0.4。by是指每隔多少距离计算一个数据点。
#Study.design可设置研究类型，是cohort还是case-control，当研究类型为case-control时，还应加上患病率population.prevalance参数。

complex<- decision_curve(chdfate~scl+sbp+dbp+age+bmi+sex,data = Data,
          family = binomial(link ='logit'), thresholds = seq(0,1, by = 0.01),
          confidence.intervals= 0.95,study.design = 'case-control',
          population.prevalence= 0.3)
#基本和simple相同，就是那几个联合应用的变量之间用个+号连接起来。

List<- list(simple,complex)
#把刚才计算的simple和complex两个对象合成一个list，命名为List。

##DCA曲线绘制
plot_decision_curve(List,curve.names= c('simple','complex'),
                   cost.benefit.axis =FALSE,col = c('red','blue'),
                   confidence.intervals =FALSE,standardize = FALSE)
#plot_decision_curve()函数的对象就是刚才的List，如果只画一根曲线，就不需要合成的那步，直接把List替换成simple或complex就好了。
#curve.names是出图时，图例上每条曲线的名字，书写顺序要跟上面合成list时一致。cost.benefit.axis是另外附加的一条横坐标轴，损失收益比，默认值是TRUE，所在不需要时要记得设为FALSE。
#col就是颜色。confidence.intervals设置是否画出曲线的置信区间，standardize设置是否对净受益率（NB）使用患病率进行校正。

summary(complex,measure= 'sNB')
#summary(complex,measure= 'NB')

##绘制临床影响曲线（Clinical Impact Curve）
plot_clinical_impact(simple,population.size = 1000,cost.benefit.axis = T,
                     n.cost.benefits= 8,col = c('red','blue'),
                     confidence.intervals= T,ylim=c(0,1000),
                     legend.position= "topright")
plot_clinical_impact(complex,population.size = 1000,cost.benefit.axis = T,
                     n.cost.benefits= 8,col = c('red','blue'),
                     confidence.intervals= T,ylim=c(0,1000),
                     legend.position= "topright")
#使用simple模型预测1000人的风险分层，显示“损失:受益”坐标轴，赋以8个刻度，显示置信区间，得到图
#红色曲线（Numberhigh risk）表示，在各个阈概率下，被simple或complex模型划分为阳性（高风险）的人数；
#蓝色曲线（Number high risk with outcome）为各个阈概率下真阳性的人数。意义一目了然。

#DCA算法的设计原理
#相当于在回归预测分析的基础上，引入了损失函数。先简单定义几个概念：
#P：给真阳性患者施加干预的受益值（比如用某生化指标预测某患者有癌症，实际也有，予活检，达到了确诊的目的）；
#L：给假阳性患者施加干预的损失值（比如预测有癌症，给做了活检，原来只是个增生，白白受了一刀）；
#Pi：患者i有癌症的概率，当Pi > Pt时为阳性，给予干预。
#所以较为合理的干预的时机是，当且仅当Pi × P >(1 – Pi) × L，即预期的受益高于预期的损失。推导一下可得，Pi > L / ( P + L )即为合理的干预时机，于是把L / ( P + L )定义为Pi的阈值，即Pt。
#但对二元的预测指标来说，如果结果是阳性，则强制Pi=1，阴性则Pi = 0。这样，二元和其他类型的指标就有了可比性。
#然后我们还可用这些参数来定义真阳性（A）、假阳性（B）、假阴性（C）、真阴性（D），即：
#A：Pi ≥ Pt，实际患病；
#B：Pi ≥ Pt，实际不患病；
#C：Pi < Pt，实际患病；
#D：Pi < Pt，实际不患病。
#我们有一个随机抽样的样本，A、B、C、D分别为这四类个体在样本中的比例，则A+B+C+D = 1。那么，患病率（π）就是A + C了。
#在这个样本中，如果所有Pi ≥ Pt 的人我们都给做了活检，那么就会有人确诊，有人白白被拉了一刀，那么净受益率NB = A × P – B × L。
#但Vickers认为，知道P和L的确切值并没有什么实际意义，人们可能更关心L/P的比值，所以将上面的公式强行除以P，变成NB = A – B × L/P。根据Pt定义公式可推导出：NB = A – B × Pt / ( 1 – Pt )。以Pt为横坐标，U为纵坐标，画出来的曲线就是决策曲线。
#若使用患病率进行校正，则U = A × π – B ×(1 –π) × Pt / ( 1 – Pt )。
#那么两个极端情况的曲线也很好推导了。当所有样本都是阴性（Pi < Pt），所有人都没干预，那么A = B = 0，所以NB = 0。当所有样本都是阳性，所有人都接受干预，那么C = D = 0，A = π，B = 1 –π，NB = π– ( 1 –π )Pt / ( 1 – Pt )，所以它斜率为负值。

#参考资料：
#1.Decision curve analysis: anovel method for evaluating prediction models
#2.Decision curve analysisrevisited: overall net benefit, relationships to ROC curve analysis, andapplication to case-control studies
#3.Assessing the Clinical Impactof Risk Prediction Models With Decision Curves: Guidance for CorrectInterpretation and Appropriate Use