#Author: Dominic Behrens
#Project: Domain Webscraping- Utilities
#Purpose: Clean up scraped dataset into a useable format with bedroom,bathroom and parking columns
#Notes:

pacman::p_load(
  tidyverse,
  sf,
  tmap,
  httr,
  scales,
  janitor,
  shiny,
  magrittr,
  systemfonts
)

#Basic Setup and Useful things
rm(list=ls())
options(scipen=1000)
#CIE Palette and ggplot theme----
cie_palette<-c("#006B79","#F61D12","#C5BEAB","#9B917D","#6E6652","#9AC2C4","#FAA39F","#FCD1CF")
scale_fill_cie<-function(...){
  discrete_scale("fill","cie_palette",manual_pal(values=cie_palette),...)
}
scale_colour_cie<-function(...){
  discrete_scale("colour","cie_palette",manual_pal(values=cie_palette),...)
}
theme_cie<-function(){
  theme_minimal(base_size=15)+
    theme(panel.background=element_rect(fill="#e9e8e5",colour=NA),
          plot.background=element_rect(fill="#e9e8e5",colour=NA),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_line(colour='black'),
          panel.grid.minor.y=element_blank())
}

#read in data----
nsw_sales<-read_csv('G:/Large Data Projects/Domain Sales Webscraping/scrape_domain_sales/Outputs/domain_all_sales.csv')
#filter down to only the typologies we care about
included_types<-c('Apartment / Unit / Flat','Duplex','House','New apartments / off the plan',
                  'Penthouse','Semi-detached','Studio','Terrace','Townhouse','Villa')

nsw_sales%<>%filter(property_type%in%included_types)

#functions---- 
#split property characteristics into three columns
make_feature_columns<-function(dataset,drop_non_standard=T){
  if(drop_non_standard==T){
    temp<-dataset%>%
      filter(str_detect(features,'Beds')|str_detect(features,'Bed'),
             str_detect(features,'Baths')|str_detect(features,'Bath'),
             str_detect(features,'Parking'),
             !str_detect(features,' Parking '))#remove where order has been messed up by the scraping. 
  }else{
    temp<-dataset
  }
  temp%<>%mutate(
      beds = as.numeric(str_extract(!!sym("features"), "^(\\d+)")),
      baths = as.numeric(str_extract(!!sym("features"), "(?<=Beds? )\\d+")),
      parking = as.numeric(str_extract(!!sym("features"), "(?<=Baths? )\\d+"))
    )
  return(temp)
}

cleaned_data<-make_feature_columns(nsw_sales)

#filter out places with zero bathrooms- many parking spaces etc
#and replace NA beds and parking with 0s

cleaned_data%<>%filter(!is.na(baths))%>%
  mutate(parking=replace_na(parking,0),
         beds=replace_na(beds,0))
