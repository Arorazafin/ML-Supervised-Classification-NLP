library(openxlsx)

path_xlsx = "C:/Users/Utilisateur/Desktop/NLP/test-classification/4.2/data_tweets.xlsx"

tweets = data.frame()
for(i in 1:3){
  temp_df = read.xlsx(path_xlsx, sheet = i)
  tweets = rbind(tweets, temp_df)
  rm(temp_df)
}

Global_results <- data.frame ()



################################################## TMP ####################################################

# Distribution des classes
table(tweets$tonalite)

max_spc = min(table(tweets$tonalite)) # Max sample per class

# Suppression des classes en surnombre
tweets_cl1 <- tweets[which(tweets$tonalite=='1'),][sample(1:nrow(tweets[which(tweets$tonalite=='1'),]), max_spc, replace=FALSE),]
tweets_cl2 <- tweets[which(tweets$tonalite=='2'),][sample(1:nrow(tweets[which(tweets$tonalite=='2'),]), max_spc, replace=FALSE),]
tweets_cl3 <- tweets[which(tweets$tonalite=='3'),][sample(1:nrow(tweets[which(tweets$tonalite=='3'),]), max_spc, replace=FALSE),]
tweets_ss <- rbind(tweets_cl1, tweets_cl2, tweets_cl3)

### Shuffle ###
set.seed(123)
tweets_ss = tweets_ss[sample(1:nrow(tweets_ss), nrow(tweets_ss)),]

# Distribution des classes sur le Shuffle/Sample
prop.table(table(tweets_ss$tonalite, useNA = "ifany"))

rm("tweets_cl1", "tweets_cl2", "tweets_cl3")

sizeData <- dim(tweets_ss)[1]
tweets_shuffled <- tweets_ss


## RETRAITEMENT ASCII
unwanted_array = list('À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 
                      'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 
                      'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c', 'è'='e', 'é'='e', 
                      'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 
                      'ô'='o', 'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ü'='u', 'ý'='y', 'ý'='y', 
                      'þ'='b', 'ÿ'='y')

library(gsubfn)
tweets_shuffled$tweet <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array, tweets_shuffled$tweet)


#CORPUS 



##

library(tm)

tweets_shuffled_corpus <- Corpus(VectorSource(as.vector(tweets_shuffled$tweet)))

#Transformation du text : Remplacer “/”, “@” et “|” avec un espace
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, toSpace, "/")
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, toSpace, "@")
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, toSpace, "\\|")

# Nettoyage du texte
# Convertir le texte en minuscule
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, content_transformer(tolower))
# Supprimer les nombres
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, removeNumbers)
# Supprimer les mots
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, removeWords, stopwords("french"))
# Supprimer votre propre liste de mots non désirés
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, removeWords, c("être", "afin")) 
# Supprimer les ponctuations
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, removePunctuation)
# Supprimer les espaces vides supplémentaires
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, stripWhitespace)
# Text stemming
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, stemDocument)


#"'"
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, removePunctuation, ucp=T)

#
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, function(x)removeWords(x,c("easyjet", "sncf", "plus", "aussi", "donc","ratp","https")))

#
tweets_shuffled_corpus <- tm_map(tweets_shuffled_corpus, removeWords, stopwords("french"))


# Construire la matrice des mots : table contenant la fréquence des mots
dtm <- TermDocumentMatrix(tweets_shuffled_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Générer le nuage de mots
library("wordcloud")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Explorer les mots fréquents ainsi que leurs associations
#findFreqTerms(dtm, lowfreq = 5)
#findAssocs(dtm, terms = "SNCF", corlimit = 0.3)
#Dessiner la fréquence des mots
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")



tweets_shuffled$tweet <- data.frame(text= get("content", tweets_shuffled_corpus))


### Model ###

set.seed(123)
library(RTextTools)
matrix <- create_matrix(tweets_shuffled$tweet, language="french", removeNumbers=FALSE, stemWords=FALSE, removePunctuation=FALSE)

# CREATE A container THAT IS SPLIT INTO A TRAINING SET AND A TESTING SET
container <- create_container(matrix,tweets_shuffled$tonalite, trainSize = 1:round(sizeData*0.75,0), testSize = (round(sizeData*0.75,0)+1):sizeData, virgin=FALSE)
testTest <- round(sizeData*0.25, digits = 0)

# THERE ARE TWO METHODS OF TRAINING AND CLASSIFYING DATA.
# ONE WAY IS TO DO THEM AS A BATCH (SEVERAL ALGORITHMS AT ONCE)
#models <- train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
models <- train_models(container, algorithms=c("MAXENT","SVM"))
results <- classify_models(container, models)

# VIEW THE RESULTS BY CREATING ANALYTICS
analytics <- create_analytics(container, results)
analytics@ensemble_summary


#### CONFUSION MATRIX - OTHER METHODE
labels_out = data.frame(
  correct_label = tweets_shuffled$tonalite[(round(sizeData*0.75,0)+1):sizeData], 
  maxent = as.character(results[,1]),
  svm = as.character(results[,3]),
  stringsAsFactors = FALSE)

ConfMatrix_maxent <- table(actual = labels_out[,1], predicted = labels_out[,2])
ConfMatrix_svm <- table(actual = labels_out[,1], predicted = labels_out[,3])



#coefficient 
coef <- rbind(c(1, 0.25, 0), c(0.25, 1, 0.25),c(0, 0.25, 1))

#Score
score_maxent <- sum(ConfMatrix_maxent * coef) /testTest
score_svm <- sum(ConfMatrix_svm * coef) /testTest



#Attribution of results
df_Global_results <-  data.frame (
  model = c("tweets-Equi-ASCII-tmLib"),
  score_maxent = c(score_maxent), 
  score_svm = c(score_svm)
)
Global_results <- rbind(Global_results,df_Global_results)
Global_results


##################################################  ####################################################


