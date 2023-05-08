#install.packages("forestplot")
library(forestplot)
rs_forest <- read.csv('abiraterone.csv',header = FALSE)
# 读入数据的时候大家一定要把header设置成FALSE，保证第一行不被当作列名称。
# tiff('Figure 1.tiff',height = 6000,width = 7000,res= 600)
forestplot(labeltext = as.matrix(rs_forest[,1:4]),
           #设置用于文本展示的列，此处我们用数据的前四列作为文本，在图中展示
           mean = rs_forest$V5, #设置均值
           lower = rs_forest$V6, #设置均值的lowlimits限
           upper = rs_forest$V7, #设置均值的uplimits限
           is.summary = c(T,T,T,F,F,T,F,F,T,F,F,T,F,F,F,T,F,F,T,F,F,T,F,F,T,F,F),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box='#458B00', summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           xlab="The estimates",#设置x轴标签
           lwd.xaxis=2,#设置X轴线的粗细
           lty.ci = "solid",
           graph.pos = 4)#设置森林图的位置，此处设置为4，则出现在第四列
