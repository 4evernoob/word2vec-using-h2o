library(h2o)
h2o.init()
email <-h2o.uploadFile("C:/Users/l440/Documents/testset.csv", destination_frame = "basemails",col.names = c("Asunto","Cuerpo","De"),col.types =c("Enum","String","Enum"),header=TRUE )
# English an dSpanish words were used because I receive Mails in both Languages
#To improve this i should include more common spanish and english words
STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
               "there","all","we","one","the","a","an","of","or","in","for","by","on",
               "but","is","in","a","not","with","as","was","if","they","are","this","and","it","have",
               "from","at","my","be","by","not","that","to","from","com","org","like","likes","so",
               "de","para","obtiene","que","en","mas","para","como")

tokenize <- function(sentences, stop.words = STOP_WORDS) {
  #split sentences removing t,n,r
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  # convert to lower case
  tokenized.lower <- h2o.tolower(tokenized)
  # remove short words (less than 2 characters)
  tokenized.lengths <- h2o.nchar(tokenized.lower)
  tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  # remove words that contain numbers
  tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
  # remove stop words
  tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

predict <- function(job.title, w2v, gbm) {
  ##convert query to tokens
  words <- tokenize(as.character(as.h2o(job.title)))
  # calculate vector for these words
  job.title.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  #predict using  model and vector
  h2o.predict(gbm, job.title.vec)
}

print("Break job titles into sequence of words")
words <- tokenize(email$Cuerpo)

print("Build word2vec model")
w2v.model <- h2o.word2vec(words, sent_sample_rate = 2e-5, epochs = 25)

print("Sanity check - find synonyms for the word 'help' and list the 12 first positions")
print(h2o.findSynonyms(w2v.model, "help", count = 12))

print("Calculate a vector for each job title")
email.title.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")

print("Prepare training&validation data (keep only job titles made of known words)")
valid.email <- ! is.na(email.title.vecs$C2)
##bind columns
data <- h2o.cbind(email[valid.email, "De"], email.title.vecs[valid.email, ])
#split frame 80 for train,20 for validatio
data.split <- h2o.splitFrame(data, ratios = 0.8)

print("Build a basic GBM model")
##De is our email sender
gbm.model <- h2o.gbm(x = names(email.title.vecs), y = "De",
                     training_frame = data.split[[1]], validation_frame = data.split[[2]])

#test queries to our model
print(predict("developer java ", w2v.model, gbm.model))
print(predict("universidad tecnologica de la mixteca",w2v.model,gbm.model))
print(predict("Foro del tejedor", w2v.model, gbm.model))