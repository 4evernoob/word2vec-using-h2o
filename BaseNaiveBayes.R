library(h2o)
#local cluster
h2o.init()
#my remote cluster
#h2o.init(ip = "192.168.1.200", port = 54321)
set.seed(666)
# import the iris dataset:
iris <-h2o.importFile("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data")
# view its contents
View(as.data.frame(iris))
# set the predictor names
predictors <-colnames(iris)[-length(iris)]


# split into train 80% and validation 10% and performance 10%
iris_splits <- h2o.splitFrame(data = iris, ratios = c(.8,.1), seed = 6)
#Number of rows in frames
train <- iris_splits[[1]]
nrow(train)
valid <- iris_splits[[2]]
nrow(valid)
perf <- iris_splits[[3]]
nrow(perf)
# train frame properties
summary(train,exact_quantiles=TRUE)
View(as.data.frame(train))
##select columns to use declare the data frames to use
mrf<-h2o.naiveBayes(model_id ='leclassifier' ,seed = 666,x=1:4,y=5,training_frame=train,validation_frame = valid)
#how well was our model using perf
h2o.performance(mrf,perf)

#what do we got on our perf dataset
res<-h2o.predict(mrf,perf)
summary(res,exact_quantiles=TRUE)


