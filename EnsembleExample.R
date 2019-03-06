library(h2o)
#local cluster
h2o.init()
#remote cluster
#h2o.init(ip = "192.168.1.200", port = 54321)
#for reproducibility
set.seed(666)
# import ecoli dataset:https://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/
ecolis <-h2o.importFile("https://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/ecoli.data")
#we don't want regression
ecolis[,c(1,9)]<-as.factor(ecolis[,c(1,9)])

sapply(as.data.frame(ecolis),class)
# split into train and validation
frame_splits <- h2o.splitFrame(data = ecolis, ratios = c(.8), seed = 6)
train <- frame_splits[[1]]
valid <- frame_splits[[2]]
nfolds<-7
#this time we use  ANN, GBM and random forest we drop first column because itis unnecessary
ensembl<-h2o.deeplearning(model_id ="leclassifier" ,
                          seed = 666,
                          x=2:8,
                          y=9,
                          training_frame=train,
                          activation = "RectifierWithDropout",
                          fold_assignment = "Modulo",
                          hidden = c(26),
                          keep_cross_validation_predictions = TRUE,
                          nfolds = nfolds,
                          epochs = 20)
ensembl1<-h2o.gbm(model_id ='leclassifier2' ,
                  seed = 666,
                  x=2:8,
                  y=9,
                  ntrees = 8,
                  max_depth = 2,
                  training_frame=train,
                  fold_assignment = "Modulo",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE
                 )
ensembl2<-h2o.randomForest(model_id ='leclassifier3' ,
                         seed = 666,
                         x=2:8,
                         y=9,
                         training_frame=train,
                         fold_assignment = "Modulo",
                         nfolds=nfolds,
                         keep_cross_validation_predictions = TRUE
                         )
#declare my ensemble
endEnsemble<-h2o.stackedEnsemble(x=2:8,y=9,training_frame = train,

                                 model_id = "myEnsemble",
                                 base_models = list(ensembl,ensembl1,ensembl2))
# get results on our performance frame
h2o.performance(ensembl,newdata=valid)
h2o.performance(ensembl1,newdata=valid)
h2o.performance(ensembl2,newdata=valid)
# we test our ensemble
h2o.performance(endEnsemble,newdata=valid)


#h2o.shutdown()
