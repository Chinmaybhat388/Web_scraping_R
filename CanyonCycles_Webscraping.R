install.packages('xopen')
library(xopen)
library(lubridate)
library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library("rstudioapi")
library(stringr)
library(stringi)
library(rvest)
library(purrr)

url_home <- "https://www.canyon.com/en-de"

html_home <- read_html(url_home)

#Get the family ID
pdt_family_id <- html_home %>% 
  html_nodes("div.js-navigationDrawer__list--secondary") %>%
  html_attr('id') %>% 
  discard(.p = str_detect(.,"(GEAR|OUTLET|WMN|WOMAN|WOMEN)")) %>%
  enframe(., name="Rank", value="Product_Family") %>%
  mutate(family_id = str_glue('#{Product_Family}'))

#Get the URL for product category
  
fam_id_css <- pdt_family_id$family_id %>% 
  str_c(collapse = ", ")  #Not really necessary

#Get the URLs for different categories of bikes
bike_category_tbl <- read_html(url_home) %>% 
  html_nodes(".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr("href") %>% 
  enframe(name = 'Order',value = 'Subdirectory') %>% 
  mutate(url = str_glue('https://www.canyon.com{Subdirectory}')) %>% 
  select(Order,url) %>%
  distinct(url)

#Get the URLs for different bikes in one of the categories from the above table.Hence use ,$url[1]
bike_url_tbl <- read_html(bike_category_tbl$url[1]) %>% 
  html_nodes("div.productTile__contentWrapper > a") %>%
  html_attr("href") %>% 
  str_remove(.,"(?<=html).*") %>% 
  enframe(name = 'Order',value='Cycle_Url')  


#Misunderstood the task and went and pulled the actual long description inside individual cycle page.Ignore this
read_html(bike_url_tbl$Cycle_Url[[1]]) %>% 
  html_nodes("p.js-showMore > span") %>% 
  html_text() %>% .[2] %>%
  str_extract("(?<= ).*(?=\\\n)") %>% 
  str_trim(.,side = c("both","left","right"))

#Get Names of all cycles under the first category
bike_names <- read_html(bike_category_tbl$url[1]) %>%  
  html_nodes("div.productTile__productName") %>%
  html_text() %>% 
  str_extract("(?<= ).*(?=\\\n)") %>% 
  str_trim(.,side = "both") %>% 
  enframe(name = 'Order',value = 'Bike_name')


#Get Descriptions of all cycles under the first category
bike_descriptions <- read_html(bike_category_tbl$url[1]) %>%  
  html_nodes("div.productTile__highlights") %>% 
  html_text() %>%
  str_extract("(?<= ).*(?=\\\n)") %>% 
  str_trim(.,side = "both")

#Get the price of all cycles under the first category
bike_prices <- read_html(bike_category_tbl$url[1]) %>%  
  html_nodes("div.productTile__priceSale") %>% 
  html_text() %>%
  str_extract("(?<= ).*(?=€)") %>% 
  str_trim(.,side = "both") %>% 
  str_replace_all(.,"[.]","") %>%
  str_replace_all(.,"[,]",".") %>% 
  as.numeric() %>% 
  enframe(name = "Order",value = "Price")

###########################################################################################
#The above code was for 1 category/cycle. Let's create functions to iterate over all 
#categories and cycles.

#Create function to fetch urls of all the individual cycles in a category.
bike_url <- function(page_url){
  test_df <- read_html(page_url) %>%
    html_nodes("div.productTile__contentWrapper > a") %>%
    html_attr("href") %>% 
    str_remove(.,"(?<=html).*") %>% 
    enframe(name = 'Order',value='Cycle_Url')
}

#Pass this function to purrr to collect all individual cycles in all categories
try_df <- purrr::map(bike_category_tbl$url,bike_url) %>%
  bind_rows(., .id = "column_label")
###################

#FULL FUNCTION TO GET DESCRIPTION,NAME and PRICE OF CYCLE

get_bike_data <- function(page_url2){
  
  #Get Names of all cycles under the first category
  bike_names <- read_html(page_url2) %>%  
    html_nodes("div.productTile__productName") %>%
    html_text() %>% 
    str_extract("(?<= ).*(?=\\\n)") %>% 
    str_trim(.,side = "both") %>% 
    enframe(name = 'Position',value = 'Bike_name')
  
  #Get Descriptions of all cycles under the first category
  bike_descriptions <- read_html(page_url2) %>%  
    html_nodes("div.productTile__highlights") %>% 
    html_text() %>%
    str_extract("(?<= ).*(?=\\\n)") %>% 
    str_trim(.,side = "both") %>%
    enframe(name = 'Position',value = 'Description')
  
  #Get the price of all cycles under the first category
  bike_prices <- read_html(page_url2) %>%  
    html_nodes("div.productTile__priceSale") %>% 
    html_text() %>%
    str_extract("(?<= ).*(?=€)") %>% 
    str_trim(.,side = "both") %>% 
    str_replace_all(.,"[.]","") %>%
    str_replace_all(.,"[,]",".") %>% 
    as.numeric() %>% 
    enframe(name = "Position",value = "Price") %>%
    left_join(bike_descriptions) %>%
    left_join(bike_names)
}



#Collecting bike data for all cycles under all categories and creating a new dataframe.
All_cycle_data <- purrr::map(bike_category_tbl$url,get_bike_data) %>% 
  bind_rows(., .id = "column_label")

