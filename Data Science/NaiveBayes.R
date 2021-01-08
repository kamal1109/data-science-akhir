#install package
#install.packages("caret")
#install.packages("e1071") #for naivebayes

# Load library
library(tidyverse)
library(tm)
library(e1071)
library(caret)
library(dplyr)

# Load dataset
my_Dataset <- read.csv("Restaurant_Reviews.csv", stringsAsFactors = FALSE)

glimpse(my_Dataset)

# Ambil kolom reviewnya dan Liked (classnya)
restaurant_review <- my_Dataset %>%
  select(text = Review, class = Liked)

restaurant_review$class <- as.factor(restaurant_review$class)

glimpse(restaurant_review)

# Ambil 1000 baris per class data untuk sample
like_review <- restaurant_review %>%
  filter(class == "1") 


dislike_review <- restaurant_review %>%
  filter(class == "0") 



restaurant_review <- rbind(like_review, dislike_review)

restaurant_review %>% count(class)

view(restaurant_review)

# Acak data set biar ga beurutan
set.seed(10)
restaurant_review <- restaurant_review[sample(nrow(restaurant_review)), ]

# CLEANING DATASET

# Mengubah data reviewnya ke bentuk corpus
corpus <- Corpus(VectorSource(restaurant_review$text))

# Cleaning
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

corpus[[15]]$content
corpus_clean[[15]]$content

# Mengubah corpus jadi dtm
dtm <- DocumentTermMatrix(corpus_clean)

# Partisi 3:1 data untuk test dan training
restaurant_review_train <- restaurant_review[1:700,]
restaurant_review_test <- restaurant_review[701:1000,]

corpus_clean_train <- corpus_clean[1:700]
corpus_clean_test <- corpus_clean[701:1000]

dtm_train <- dtm[1:701,]
dtm_test <- dtm[701:1000,]

dim(dtm_train)
dim(dtm_test)


# Feature Selection, ambil kata yang muncul minimal 5 kali
fiveFreq <- findFreqTerms(dtm_train, 5)

length(fiveFreq)

# set directory tempat simpan feature yg digunakan
# save featurenya
saveRDS(fiveFreq, "features.rds")


# Sesuaikan fitur pada data train dan test dengan fitur yang sudah diseleksi sebelumnya
dtm_train_nb <- corpus_clean_train %>%
  DocumentTermMatrix(control=list(dictionary = fiveFreq))

dtm_test_nb <- corpus_clean_test %>%
  DocumentTermMatrix(control=list(dictionary = fiveFreq))

dim(dtm_train_nb)
dim(dtm_test_nb)

# Funsi untuk convert jumlah kemunculan kata jadi yes (ada) dan no (ga ada)
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm_train_nb, 2, convert_count)
testNB <- apply(dtm_test_nb, 2, convert_count)

view(testNB)

# Membuat model naive bayes dari data training
classifier <- naiveBayes(trainNB, restaurant_review_train$class, laplace = 1)

# set directory tempat simpan model naivebayes nya
# save model untuk di gunakan pada aplikasi
save(classifier , file = 'NaiveBayesClassifier.rda')


# test model naivebayes nya
pred <- predict(classifier, newdata=testNB)

# Buat table hasil prediksi
table("Predictions"= pred,  "Actual" = restaurant_review_test$class)

# Confusion Matrix
conf_mat <- confusionMatrix(pred, restaurant_review_test$class)
conf_mat$overall['Accuracy']

