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
  systemfonts,
  quantreg,
  biglm
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

#cleaning---- 
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
         beds=replace_na(beds,0),
         postcode=substr(address2,nchar(address2)-3,nchar(address2)))

#now fix up dates.
date_prefixes<-c('Sold by private treaty','Sold at auction','Sold prior to auction','Sold')
  
cleaned_data%<>%mutate(
  date=str_remove(date,paste(date_prefixes,collapse='|')),
  date=str_trim(date),
  year=substr(date,nchar(date)-3,nchar(date)))
#Convert to LGA level
#Read in Postcode-LGA concordance and tidy up to make 1 to 1
poa_lga<-read_csv('./Inputs/CG_POSTCODE_2021_LGA_2022.csv')%>%
  clean_names()%>%
  group_by(postcode)%>%
  filter(ratio_from_to==max(ratio_from_to))%>%
  ungroup()%>%
  select(c('postcode','lga_name_2022'))
#join to data
cleaned_data%<>%left_join(poa_lga)

#Make higher level typology column
apartment_types<-c("Apartment / Unit / Flat","Penthouse","New apartments / off the plan","Studio")
townhouse_types<-c("Duplex",'Semi-detached','Terrace','Townhouse','Villa')

cleaned_data%<>%mutate(
  prop_type_clean=case_when(
    property_type%in%apartment_types~"Apartment",
    property_type%in%townhouse_types~"townhouse",
    .default = "House")
)

#finally, tidy up prices into numeric
cleaned_data%<>%
  mutate(price=str_remove_all(price,"\\$"),
         price=str_remove_all(price,","),
         price=as.numeric(price))

#analysis----
#Get year-by-type stats by LGA
lga_averages<-cleaned_data%>%
  group_by(prop_type_clean,lga_name_2022,year)%>%
  summarise(
    med_price=median(price),
    ave_price=mean(price)
  )

#estimate simple hedonic model
#Beforehand, trim top and bottom 5 percent of sales within each lga 
#and remove those with crazy numbers of beds, baths or parking (7,7 and 5)
#also drop years pre 2020 as this reduces dimensionality
model_data<-cleaned_data%>%
  group_by(lga_name_2022)%>%
  filter(price<quantile(price,0.95),
         price>quantile(price,0.05))%>%
  ungroup()%>%
  filter(beds<=7,
         baths<=7,
         parking<=5,
         as.numeric(year)>=2020)

#run model
hedonic_model<-lm(model_data,
                    formula=log(price)~lga_name_2022+beds+baths+parking+prop_type_clean+year+year*lga_name_2022)

#make predictions
exp(predict(hedonic_model,newdata = data.frame(lga_name_2022='Blacktown',
                                           year="2024",
                                           beds=3,
                                           baths=2,
                                           parking=1,
                                           prop_type_clean='House')))

#predict price of a 3-bedroom, 2 bath townhouse in each LGA in 2024
