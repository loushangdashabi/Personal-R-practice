sigmoid <- function(x) {
   1 / ( 1 + exp(-x) )
   }

x <- seq(-5, 5, .1)

plot(sigmoid(x))

library(ggplot2)
s <- sigmoid(x)
t <- tanh(x)
z <- data.frame(cbind(x, s, t))
ggplot(z, aes(x)) + 
  geom_line(aes(y = s, color = "sigmoid")) + 
  geom_line(aes(y = t, color = "tanh")) +
  labs(x = "Input",y = "Output") 

####神经网络案例1

library(caret)
library(MASS)
library(neuralnet)
library(vcd)

data(shuttle)
str(shuttle)
table(shuttle$use)
table1 <- structable(wind + magn ~ use, shuttle)
table1
mosaic(table1, shade = T)
mosaic(use ~ error + vis, shuttle)
table(shuttle$use, shuttle$stability)
prop.table(table(shuttle$use, shuttle$stability))
chisq.test(shuttle$use, shuttle$stability)

dummies <- dummyVars(use ~. ,shuttle, fullRank = T)
dummies
shuttle.2 <- data.frame(predict(dummies, newdata = shuttle))
names(shuttle.2)
head(shuttle.2)
shuttle.2$use <- ifelse(shuttle$use == "auto", 1, 0)
table(shuttle.2$use)
set.seed(123)
trainIndex <- createDataPartition(shuttle.2$use, p = .7,
                                  list = F)
# head(trainIndex)
shuttleTrain <- shuttle.2[ trainIndex, ]
shuttleTest  <- shuttle.2[-trainIndex, ]

#### 模型构建与评价
n <- names(shuttleTrain)
form <- as.formula(paste("use ~", paste(n[!n %in% "use"], collapse = " + ")))
form

set.seed(1)
fit <- neuralnet(form, data = shuttleTrain, hidden = c(2, 1), err.fct = "ce", 
                 linear.output = F)
fit$result.matrix
head(fit$generalized.weights[[1]])
plot(fit)
par(mfrow=c(1,2))
gwplot(fit, selected.covariate = "vis.yes")
gwplot(fit, selected.covariate = "wind.tail")

resultsTrain <- compute(fit, shuttleTrain[, 1:10])
predTrain <- resultsTrain$net.result

predTrain <- ifelse(predTrain >= 0.5, 1, 0)
table(predTrain, shuttleTrain$use)

resultsTest <- compute(fit, shuttleTest[,1:10])
predTest <- resultsTest$net.result
predTest <- ifelse(predTest >= 0.5, 1, 0)
table(predTest, shuttleTest$use)

which(predTest == 1 & shuttleTest$use == 0)
shuttleTest[62,]

#####神经网络案例2

library(mlbench)
library(neuralnet)

data(BreastCancer)
summary(BreastCancer)

mvindex = unique (unlist (lapply (BreastCancer, function (x) which (is.na (x)))))
mvindex
data_cleaned <- na.omit(BreastCancer) 
summary(data_cleaned)

boxplot(data_cleaned[,2:10])
hist(as.numeric(data_cleaned$Mitoses))

par(mfrow=c(3, 3))
hist(as.numeric(data_cleaned$Cl.thickness))
hist(as.numeric(data_cleaned$Cell.size))
hist(as.numeric(data_cleaned$Cell.shape))
hist(as.numeric(data_cleaned$Marg.adhesion))
hist(as.numeric(data_cleaned$Epith.c.size))
hist(as.numeric(data_cleaned$Bare.nuclei))
hist(as.numeric(data_cleaned$Bl.cromatin))
hist(as.numeric(data_cleaned$Normal.nucleoli))
hist(as.numeric(data_cleaned$Mitoses))

str(data_cleaned)
input<-data_cleaned[,2:10]
indx <- sapply(input, is.factor)
input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))

max_data <- apply(input, 2, max)
min_data <- apply(input, 2, min)
input_scaled <- as.data.frame(scale(input,center = min_data, scale = max_data - min_data))
View(input_scaled)

Cancer<-data_cleaned$Class
Cancer<-as.data.frame(Cancer)
Cancer<-with(Cancer, data.frame(model.matrix(~Cancer+0)))

final_data<-as.data.frame(cbind(input_scaled,Cancer))

index = sample(1:nrow(final_data),round(0.70*nrow(final_data)))
train_data <- as.data.frame(final_data[index,])
test_data <- as.data.frame(final_data[-index,])

n = names(final_data[1:9])
f = as.formula(paste("Cancerbenign + Cancermalignant ~", paste(n, collapse = " + ")))

net = neuralnet(f,data=train_data,hidden=5,linear.output=FALSE)
plot(net)

predict_net_test <- compute(net,test_data[,1:9])
predict_result<-round(predict_net_test$net.result, digits = 0)
net.prediction = c("benign", "malignant")[apply(predict_result, 1, which.max)]
predict.table = table(data_cleaned$Class[-index], net.prediction)
predict.table

library(gmodels)
CrossTable(x = data_cleaned$Class[-index], y = net.prediction,
           prop.chisq=FALSE)
###########################################################################

#####  Deep Learning

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tverberg/5/R")))
library(h2o)

path <- "C:/h2o/bank_DL.csv" # C盘新建h2o文件夹，把bank_DL.csv提前拷贝进去。
localH2O = h2o.init(nthreads = -1)

bank <- h2o.uploadFile(path=path)
class(bank)
str(bank)
head(bank)
summary(bank)
h2o.table(bank$y)

rand <- h2o.runif(bank, seed = 123)

train <- bank[rand <= 0.7, ]
train <- h2o.assign(train, key = "train")
test <- bank[rand  > 0.7, ]
test <- h2o.assign(test, key = "test")

h2o.table(train[, 64])
h2o.table(test[, 64])

hyper_params <- list(
  activation = c("Tanh", "TanhWithDropout"),
  hidden = list(c(20,20),c(40, 40),c(30, 30, 30)),
  input_dropout_ratio = c(0, 0.05),
  rate = c(0.01, 0.25)
)

search_criteria = list(
  strategy = "RandomDiscrete", max_runtime_secs = 420,     
  max_models = 100, seed = 123, stopping_rounds = 5, 
  stopping_tolerance = 0.01
)

randomSearch <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "randomSearch",
  training_frame = train,
  validation_frame = test, 
  x = 1:63, 
  y = 64,
  epochs = 1,
  stopping_metric = "misclassification",
  hyper_params = hyper_params,
  search_criteria = search_criteria
)                        

grid <- h2o.getGrid("randomSearch", sort_by = "auc", decreasing = T)
grid

best_model <- h2o.getModel(grid@model_ids[[1]])
h2o.confusionMatrix(best_model, valid = T)

dlmodel <- h2o.deeplearning(
  x = 1:63,
  y = 64, 
  training_frame = train,
  hidden = c(30, 30, 30),
  epochs = 3,
  nfolds = 5,
  fold_assignment="Stratified",
  balance_classes = T,
  activation = "TanhWithDropout",
  seed = 123,
  adaptive_rate = F, 
  input_dropout_ratio = 0.05,
  stopping_metric = "misclassification",
  variable_importances = T
)

dlmodel

perf <- h2o.performance(dlmodel, test)
perf

dlmodel@model$variable_importances
