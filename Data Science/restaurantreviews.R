library(rvest)

baseUrl <- "https://www.tripadvisor.com"
get_restaurant_reviews <- function(restaurantUrl) {
  withProgress(message = 'Scrape Tripadvisor', value = 0, {
    
    reviewPage <- read_html(paste(baseUrl, restaurantUrl, sep = ""))
    
    review <- reviewPage %>%
      html_nodes('.prw_rup.prw_reviews_text_summary_hsx') %>%
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
    
    while (!is.na(nextPage) & length(reviews) < 100) {
      incProgress(1/100, detail = paste("jumlah review : ", length(reviews)))
      
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
    data.frame(name =reviewers, Review = reviews, stringsAsFactors = FALSE)
  })
}

