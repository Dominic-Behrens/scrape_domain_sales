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


#function to check strings are formatted appropriately
check_string_format <- function(input_string) {
  # Check if input is a character string
  if (!is.character(input_string) || length(input_string) != 1) {
    return(FALSE)
  }
  
  # Count the number of hyphens in the string
  hyphen_count <- nchar(input_string) - nchar(gsub("-", "", input_string))
  
  # Check if string ends with exactly 4 digits
  ends_with_4_digits <- grepl("\\d{4}$", input_string)
  
  #check if string contains a state string
  contains_state<-str_detect(input_string,"nsw|act|qld|wa|vic|sa|tas")

  # Return TRUE if both conditions are met
  return(hyphen_count >= 2 && ends_with_4_digits && contains_state)
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
  #set up session
  check_sesh<-ChromoteSession$new()
  on.exit(check_sesh$close(), add = TRUE)
  check_sesh$Page$navigate(url)
  Sys.sleep(2)
  #check how many buttons are 'disabled'
  count_disabled <- check_sesh$Runtime$evaluate('
   document.querySelectorAll("[data-testid=\\"paginator-navigation-button\\"]")
     .length - 
   document.querySelectorAll("[data-testid=\\"paginator-navigation-button\\"]:not([disabled])")
     .length
 ')$result$value
  if(count_disabled==0){
    return(1)}else{
      if(count_disabled==2){
        return(0)
      }else{
  page_link<-check_sesh$Runtime$evaluate("
  document.querySelector('a[data-testid=\"paginator-navigation-button\"]').getAttribute('href')
")$result$value
  next_page_num<-as.numeric(sub(".*=","",page_link))
  current_page_num<-as.numeric(sub(".*=","",url))
  if(next_page_num>current_page_num){
    return(1)
  }else{
    return(0)
  }}}
}

#function to check there are actually sales on the page. Returns 1 if sales, 0 if none
check_for_sales<-function(url){
  check_sesh<-ChromoteSession$new()
  on.exit(check_sesh$close(), add = TRUE)
  check_sesh$Page$navigate(url)
  Sys.sleep(1)
  no_matches_present <- check_sesh$Runtime$evaluate('
  Array.from(document.querySelectorAll("h3.css-1c8ubmt"))
    .some(el => el.textContent.trim() === "No exact matches")
')$result$value
 if(no_matches_present==T){
   return(0)}else{
     return(1)
   } 
}

#Function to scrape given a suburb-state-postcode string
scrape_suburb<-function(suburb_string,exclude_withheld=T,dwelling_type=c('House','Apartment','Townhouse','All')){
  #high level checks for validity of string
  #needs to have at least 2 dashes and end in a 4 digit number, as well as contain one of the state strings
  check_string<-check_string_format(suburb_string)
  if(check_string==F){
    cat('Invalid Suburb, check formatting\n')
    return()
  }
  #build url
  #toggle if you include price witheld or not
  if(exclude_withheld==T){
    url_suffix<-'/?excludepricewithheld=1&ssubs=0&page='
  }else{
    url_suffix<-'&ssubs=0&page='
  }
  #add in dwelling type if specified
  url_suffix<-case_when(
    dwelling_type=='All'~url_suffix,
    dwelling_type=='House'~paste0('/house',url_suffix),
    dwelling_type=='Apartment'~paste0('/apartment',url_suffix),
    dwelling_type=='Townhouse'~paste0('/town-house',url_suffix))
  #pull it all together
  url_base<-paste0('https://www.domain.com.au/sold-listings/',suburb_string,url_suffix)
  #check that the URL has sales
  sales_check<-check_for_sales(paste0(url_base,1))
  if(sales_check==1){
  i<-1
  more_pages<-T
  out_frame<-data.frame()
  while(more_pages==T){
    #build url
    url_full<-paste0(url_base,i)
    cat(paste0('Scraping page ',i,', url: ',url_full,'\n'))
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
  last_date<-out_frame%>%
    slice_tail(n=1)%>%
    pull(date)
  cat(paste0('Reached limit for suburb, final sale: ',last_date,'\n'))
  return(out_frame)
  }else{
  return()
}
}

