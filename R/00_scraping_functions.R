#Author:Dominic Behrens
#Project: General Use
#Purpose: Scrape property sales and their details from domain.com.au
#Date: August 2025

pacman::p_load(
  tidyverse,
  httr,
  rvest,
  targets,
  magrittr,
  janitor,
  zoo,
  chromote
)

#helper functions
#function to paste bedroom,bathroom,parking strings into one string. 
paste_every_third <- function(x, sep = " ") {
  # Create grouping indices
  groups <- rep(1:ceiling(length(x)/3), each = 3, length.out = length(x))
  
  # Split and paste
  split(x, groups) %>%
    map_chr(paste, collapse = sep)
}
#function to convert the HTML from a sold listings page on domain into a clean dataframe
html_to_frame <- function(html_content) {
  # Read the page
  page <- read_html(html_content)
  
  # Extract all elements
  dates<- page%>%
    html_nodes('[data-testid="listing-card-tag"]') %>% 
    html_text(trim = TRUE)
  
  address1 <- page %>% 
    html_nodes('[data-testid="address-line1"]') %>% 
    html_text(trim = TRUE)
  
  address2 <- page %>% 
    html_nodes('[data-testid="address-line2"]') %>% 
    html_text(trim = TRUE)
  
  prices <- page %>% 
    html_nodes('[data-testid="listing-card-price"]') %>% 
    html_text(trim = TRUE)
  
  features <- page %>% 
    html_nodes('[data-testid="property-features-text-container"]') %>% 
    html_text(trim = TRUE)
  
  features_nosize<-features[!str_detect(features,'m²')]
  
  features_out<-paste_every_third(features_nosize)
  
  property_types <- page %>% 
    html_nodes('span.css-693528') %>% 
    html_text(trim = TRUE)
  
  # Find the maximum length
  max_length <- max(
    length(dates),
    length(address1),
    length(address2),
    length(prices), 
    length(features_out), 
    length(property_types)
  )
  
  # Create data frame, padding shorter vectors with NA
  data.frame(
    position = 1:max_length,
    date= rep(dates, length.out = max_length)[1:max_length],
    address1 = rep(address1, length.out = max_length)[1:max_length],
    address2 = rep(address2, length.out = max_length)[1:max_length],
    price = rep(prices, length.out = max_length)[1:max_length],
    features = rep(features_out, length.out = max_length)[1:max_length],
    property_type = rep(property_types, length.out = max_length)[1:max_length],
    stringsAsFactors = FALSE
  )
}
#function that wraps around html_to_frame(), sets up a chromote session and extracts details
scrape_page<-function(url){
  #establish chromote session, navigate to url, wait 2 secs then extract HTML
  sesh<-ChromoteSession$new()
  on.exit(sesh$close(), add = TRUE)
  sesh$Page$navigate(url)
  Sys.sleep(2)
  page_html<-sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  #convert HTML into a useful dataframe
  sales_frame<-html_to_frame(page_html)
  return(sales_frame)
}
#function to check if there are further pages to scrape- returns 1 if there are more to scrape, 0 otherwise
check_for_more_pages<-function(url){
  check_sesh<-ChromoteSession$new()
  on.exit(check_sesh$close(), add = TRUE)
  check_sesh$Page$navigate(url)
  Sys.sleep(2)
  count_disabled <- check_sesh$Runtime$evaluate('
   document.querySelectorAll("[data-testid=\\"paginator-navigation-button\\"]")
     .length - 
   document.querySelectorAll("[data-testid=\\"paginator-navigation-button\\"]:not([disabled])")
     .length
 ')$result$value
  if(count_disabled==0){
    return(1)}else{
  page_link<-check_sesh$Runtime$evaluate("
  document.querySelector('a[data-testid=\"paginator-navigation-button\"]').getAttribute('href')
")$result$value
  next_page_num<-as.numeric(sub(".*=","",page_link))
  current_page_num<-as.numeric(sub(".*=","",url))
  if(next_page_num>current_page_num){
    return(1)
  }else{
    return(0)
  }}
}

#Function to scrape given a url
url_base<-"https://www.domain.com.au/sold-listings/rushcutters-bay-nsw-2011/?excludepricewithheld=1&page="
i<-1
more_pages<-T
out_frame<-data.frame()
while(more_pages==T){
  cat(paste0('Scraping page ',i,'\n'))
  #build url
  url_full<-paste0(url_base,i)
  #scrape first page
  temp_frame<-scrape_page(url_full)
  out_frame%<>%bind_rows(temp_frame)
  #check for next and end if no more, otherwise update index and repeat
  if(check_for_more_pages(url_full)==0){
    more_pages<-F
  } else{
   i<-i+1
  }
}
