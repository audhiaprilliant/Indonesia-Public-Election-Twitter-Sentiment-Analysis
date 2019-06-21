# ===== TWITTER DATA ANALYSIS WITH PACKAGE twitteR =====

library(caret)
library(tm)

# ===== LOAD THE RAW DATA
data.raw = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Data Prabowo Subianto.csv',
                    header = TRUE,
                    sep = ',')
data.raw.label = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Sentiment Data Prabowo Subianto.csv',
                          header = TRUE,
                          sep = ',')
data.raw = data.frame(text = data.raw$text,
                      class = data.raw.label$class)
data.raw$class = as.character(data.raw$class)
data.raw = data.raw[which(data.raw$class != 'Neutral'),]
data.raw$class = as.factor(data.raw$class)

# ===== LOAD THE CORPUS DATA
data.corpus = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Sentiment Data Prabowo Subianto.csv',
                       header = TRUE,
                       sep = ',')
data.corpus.clean = Corpus(VectorSource(data.corpus$text))
View(data.corpus.clean)
str(data.corpus.clean)

# ===== CONVERT Corpus Clean as DTM
data.dtm = DocumentTermMatrix(data.corpus.clean)

# ===== DATA PARTITIONING
set.seed(100)
index = createDataPartition(data.raw$class, p = 0.7, list = FALSE)
# Data Raw
data.raw.train = data.raw[index,]
data.raw.test = data.raw[-index,]
# Data Corpus Clean
data.corpus.clean.train = data.corpus.clean[index]
data.corpus.clean.test = data.corpus.clean[-index]
# Data DTM
data.dtm.train = data.dtm[index,]
data.dtm.test = data.dtm[-index,]

# ===== PICK TERMS THAT APPEAR AT LEAST 5 TIMES IN DTM TRAIN
data.freq.words = findFreqTerms(data.dtm.train,
                                lowfreq = 5)
data.dtm.freq.train = data.dtm.train[,data.freq.words]
data.dtm.freq.test = data.dtm.test[,data.freq.words]
# Function
convert_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x,
             levels = c(0, 1),
             labels = c('Absent', 'Present'))
}
# Make Data Train and Data Test
data.train = apply(data.dtm.freq.train,
                   MARGIN = 2,
                   FUN =  convert_counts)
data.test = apply(data.dtm.freq.test,
                  MARGIN = 2,
                  FUN =  convert_counts)

# ===== MODELLING WITH NAIVE BAYES WITHOUT USING LAPLACE CORRECTION
ctrl = trainControl(method = 'cv',
                    number = 10)
set.seed(1234)
nb.model = train(y = data.raw.train$class, 
                 x = data.train,
                 method = 'nb',
                 metric = 'accuracy',
                 trControl = ctrl)

# ===== MODELLING WITH NAIVE BAYES WITH USING LAPLACE CORRECTION
set.seed(1234)
nb.model = train(y = data.raw.train$class, 
                 x = data.train,
                 method = 'nb',
                 tuneGrid = data.frame(.fL = 1,
                                       .usekernel = FALSE),
                 trControl = ctrl)

nb.model
plot(nb.model)

# MAKING PREDICTION
valid_pred = predict(nb.model, data.train)
#STORING MODEL PERFORMANCE SCORES
confusionMatrix(valid_pred, data.raw.train$class, positive = 'Positive')
