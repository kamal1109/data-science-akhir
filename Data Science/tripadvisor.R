library(rvest)
library(purrr)
library(textclean)
library(tokenizers)
library(wordcloud)
library(corpus)
library(dplyr)
library(tm)

baseUrl <- "https://www.tripadvisor.com"
restaurantUrl <- "/Restaurants-g294265-Singapore.html"
url <- paste(baseUrl, restaurantUrl, sep = "")
webpage <- read_html(url)

restaurantName <- webpage %>% html_nodes('[class="_15_ydu6b"]') %>% html_text()
restaurantReviewURL <- webpage %>% html_nodes('[class="_15_ydu6b"]') %>% html_attr('href')

dfrestaurant <- data.frame(name = restaurantName, link = restaurantReviewURL, stringsAsFactors = FALSE)

View(dfrestaurant)

# set direktori untuk simpan data
setwd("C:/ALIVI/KULIAH/SEMESTER 6/TUGAS/Data Science/proyek")
# simpan data
saveRDS(dfrestaurant, "singapore.rds")
write.csv(dfrestaurant,"singapore.csv", row.names = FALSE)
dok<-read.csv("singapore.csv" , stringsAsFactors = TRUE)


corpusdok <- Corpus(VectorSource(dok$name))
inspect(corpusdok[1:10])
#Cleaning Hashtag
remove.hashtag <- function(x) gsub("#\\S+", "", x)
dok_hashtag <- tm_map(corpusdok, remove.hashtag)
inspect(dok_hashtag[1:10])
#Cleaning Punctuation
dok_punctuation<-tm_map(dok_hashtag,content_transformer(removePunctuation))
inspect(dok_punctuation[1:10])
#Cleaning Number
dok_nonumber<-tm_map(dok_punctuation, content_transformer(removeNumbers))
inspect(dok_nonumber[1:10])


df_restaurant=data.frame(name=unlist(sapply(dok_nonumber, `[`)),link = restaurantReviewURL, stringsAsFactors=F) 
saveRDS(df_restaurant, "restaurant.rds")

View(df_restaurant)

# Cara ngambil semua review dari hotel pertama (diterapkan di shinynya)
dfrestaurant$name[1]
reviewUrl <- paste(baseUrl, dfrestaurant$link[1], sep = "")
reviewPage <- read_html(reviewUrl)

review <- reviewPage %>%
  html_nodes('.partial_entry') %>%
  html_text()

reviewer <- reviewPage %>%
  html_nodes('.info_text.pointer_cursor') %>%
  html_text()

reviews <- character()
reviewers <- character()
reviews <- c(reviews, review)
reviewers <- c(reviewers, reviewer)

nextPage <- reviewPage %>%
  html_nodes('.next') %>%
  html_attr('href')

while (!is.na(nextPage)) {
  reviewUrl <- paste(baseUrl, nextPage, sep = "")
  reviewPage <- read_html(reviewUrl)
  
  review <- reviewPage %>%
    html_nodes('.prw_rup.prw_reviews_text_summary_hsx') %>%
    html_text()
  
  reviewer <- reviewPage %>%
    html_nodes('.info_text.pointer_cursor') %>%
    html_text()
  
  reviews <- c(reviews, review)
  reviewers <- c(reviewers, reviewer)
  
  nextPage <- reviewPage %>%
    html_nodes('.next') %>%
    html_attr('href')
}
p <- length(reviewers)
reviews <- reviews[1:p]
datareview <- data.frame(reviews, reviewers, stringsAsFactors = FALSE)

length(reviews)
View(datareview)

