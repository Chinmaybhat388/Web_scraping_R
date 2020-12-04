install.packages("DBI")
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library("rstudioapi")
library(stringr)
library(purrr)

#Connect to the Chinook database

conn <- DBI::dbConnect(drv = SQLite(),
                       dbname = 'D:/Personal DataProjects/R_assignment/00_data/02_chinook/Chinook_Sqlite.sqlite')

dbListTables(conn)[1] #ALBUM table

str(collect(tbl(conn,dbListTables(conn)[1]))) #str of album table

dbDisconnect(conn)

#######################################################################

#Getting data from APIs using HTTR library

#swapi 

httr::GET('https://swapi.dev/api/people/?page=3')

fromJSON(rawToChar(httr::GET('https://swapi.dev/api/people/?page=3')$content))

#Define a function for star wars api

sw_api <- function(path){
  url = modify_url(url = 'https://swapi.dev/',path = glue('/api/{path}'))
  resp <- GET(url)
  resp
}

resp <- sw_api('people/1')

resp$content

rawToChar(resp$content)

fromJSON(rawToChar(resp$content)) #Displays the content fetched from api in list format

content(resp) #Displays the content fetched from api in list format

#######################################################################
#Working with another API for stock prices

resp2 <- GET("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey=3O4AHLHSJ829XIO1")
resp2$content

fromJSON(rawToChar(resp2$content))

content(resp2)

Sys.getenv('API_KEY')

install.packages("rstudioapi")

alphavantage_api_url <- 'https://www.alphavantage.co/'
ticker <- 'WDI.DE'

#Using library rstudioapi to insert the API key in the prompt.
library("rstudioapi")
GET(alphavantage_api_url,query = list('function' = "GLOBAL_QUOTE",symbol= ticker,apikey= askForPassword("token")))

########################################################################

#WEB SCRAPING

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)

sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()

sp_500

#Below code is similar to the previous.
as_tibble(html_table(html_nodes(read_html(url),css="#constituents"))[[1]])

#Example 2 - IMDB web page


url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

#Get the ranking of the movie
ranking <- read_html(url) %>% 
  html_nodes(css = "td.titleColumn") %>% 
  html_text() %>%
  stringr::str_extract("(?<=)[0-9]*(?=\\.\\n)") %>% 
  as.numeric() 

#Same as above.
as.numeric(str_extract(html_text(html_nodes(read_html(url),css = ".titleColumn")),"(?<= )[0-9]+(?=\\.\\n)"))

#Get the title
title <- read_html(url) %>% html_nodes("td.titleColumn > a") %>% html_text()

#Get the year 
year <- read_html(url) %>% 
  html_nodes("td.titleColumn > span") %>%
  html_text() %>% 
  str_extract("(?<=\\()[0-9]+(?=\\))") %>% 
  as.numeric()
  
#Get the actors
actors <- read_html(url) %>% 
  html_nodes("td.titleColumn > a") %>% 
  html_attr("title")

#Get the rating
rating <- read_html(url) %>% 
  html_nodes("td.imdbRating > strong") %>%
  html_text() %>% 
  as.numeric()

#Get the number of ratings
Number_of_ratings <- read_html(url) %>% 
  html_nodes("td.imdbRating > strong") %>%
  html_attr("title") %>% 
  str_extract("(?<= )[0-9,]+(?= )") %>%
  str_replace_all(',','') %>% 
  as.numeric()

#Final movies dataset
top_movies <- tibble(ranking,title,year,actors,rating,Number_of_ratings)

