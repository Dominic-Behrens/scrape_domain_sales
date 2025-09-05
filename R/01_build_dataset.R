#Author: Dominic Behrens
#Project: General Use
#Purpose: Build database using scraped domain.com.au sales
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
  systemfonts,
  cli
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

#source functions
source('./R/00_scraping_functions.R')


#read in suburb shapefile
suburbs_shp<-st_read('./Inputs/SAL_2021_AUST_GDA2020.shp')%>%
  clean_names()%>%
  select(c('sal_name21','ste_name21'))

#read in postcode shapefile
poa_shp<-st_read('./Inputs/POA_2021_AUST_GDA94.shp')%>%
  st_transform(7844)%>%
  clean_names()%>%
  select(c('poa_name21'))

#get postcodes by centroids of suburbs
suburbs_postcodes<-suburbs_shp%>%
  st_centroid()%>%
  st_join(poa_shp)


#get list of suburbs in format for domain (suburb-state-postcode)
suburbs_postcodes%<>%mutate(
  state_name_short=case_when(
    ste_name21=='New South Wales'~'nsw',
    ste_name21=='Victoria'~'vic',
    ste_name21=='Queensland'~'qld',
    ste_name21=='South Australia'~'sa',
    ste_name21=='Western Australia'~'wa',
    ste_name21=='Tasmania'~'tas',
    ste_name21=='Northern Territory'~'nt',
    .default='Other'
  ),
  suburb_name_clean=str_trim(str_remove_all(str_to_lower(sal_name21), "\\(.*?\\)")),
  suburb_name_clean=str_replace_all(suburb_name_clean," ","-"),
  domain_suburb=paste(suburb_name_clean,state_name_short,poa_name21,sep='-')
)

#loop over NSW suburbs and scrape into a big dataset
nsw_suburbs<-suburbs_postcodes%>%
  filter(ste_name21=='New South Wales')

big_domain_dataset<-data.frame()

for(suburb in nsw_suburbs$domain_suburb){
  cat(paste0('Scraping sales from suburb: ',suburb,'\n'))
  temp_frame<-scrape_suburb(suburb,dwelling_type='All')
  big_domain_dataset%<>%bind_rows(temp_frame)
}



