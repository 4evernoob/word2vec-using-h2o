library(h2o)
#local cluster
h2o.init()
#remote cluster
#h2o.init(ip = "192.168.1.200", port = 54321)
#for reproducibility
set.seed(666)
# import wine dataset: from https://archive.ics.uci.edu/ml/datasets/Wine
wines <-h2o.importFile("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
#we don't want regression
wines[,1]<-as.factor(wines[,1])

View(as.data.frame(wines))
# split into train and validation
iris_splits <- h2o.splitFrame(data = wines, ratios = c(.8,.1), seed = 6)
train <- iris_splits[[1]]
class(as.data.frame(train))
valid <- iris_splits[[2]]
perf <- iris_splits[[3]]
#this time we use  ANN
mrf<-h2o.deeplearning(model_id ='leclassifier' ,seed = 666,x=2:13,y=1,training_frame=train,validation_frame = valid,activation = "RectifierWithDropout",hidden = c(20),epochs = 20)
## get results on our performance frame
h2o.performance(mrf,perf)
res<-h2o.predict(mrf,perf, id)
summary(res,exact_quantiles=TRUE)


