library(rvest)
library(httr)
library(tidyverse)
library(data.table)
library(lubridate)
options(scipen=999999,stringsAsFactors = F)
past_data <- fread("boxoffice_scores.csv")
numbersClean <- function(x){
  #x <- movie_list$movie[i]
  x <- tolower(x)
  if(grepl("^the",x)){
    x <- gsub("^the ","",x)
    x <- paste0(x," the")
  }
  if(grepl("^a ",x)){
    x <- gsub("^a ","",x)
    x <- paste0(x," a")
  }
  if(grepl("^an ",x)){
    x <- gsub("^an ","",x)
    x <- paste0(x," an")
  }
  x <- gsub(" ","-",x)
  x <- gsub("'","",x)
  x <- gsub("’","",x)
  x <- gsub("&","and",x)
}




number_movie_scrapR <- function(movie_list)  {
  movie_meta <- data.frame()
  numbers <-"https://www.the-numbers.com/movie/%s#tab=summary"
i <- 1
while(i <= nrow(movie_list)) {
  movie <- numbersClean(movie_list$movie[i])
  movie_year <- year(Sys.Date())
  if(grepl("…",movie,fixed=T)==F){
    url <- sprintf(numbers,movie)
    thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T))
    
    if(class(thing1) != "try-error"){
      rownames(thing1) <- gsub("\\s","_",rownames(thing1))
      release_date <- str_extract(thing1['Domestic_Releases:',],'^[A-z]+ [0-9]+[a-z]{2}, [0-9]{4}')
      release_date <- gsub("[a-z]{2}, ", " ",release_date)
      release_date <- as.Date(release_date,format="%B %d %Y")
      number_year <- year(release_date)
      movie_year <- year(Sys.Date())
      if(is.na(number_year)){
        movie <- numbersClean(movie_list$movie[i])
        movie <- paste0(movie,"-(",movie_year,")")
        url <- sprintf(numbers,movie)
        thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T)) 
      } else if(number_year != movie_year){
        movie <- numbersClean(movie_list$movie[i])
        movie <- paste0(movie,"-(",movie_year,")")
        url <- sprintf(numbers,movie)
        thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T)) 
        if(class(thing1) == "try-error"){
          movie <- numbersClean(movie_list$movie[i])
          movie <- paste0(movie,"-(",(movie_year-1),")")
          url <- sprintf(numbers,movie)
          thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T)) 
        }
        if(class(thing1) == "try-error"){
          movie <- numbersClean(movie_list$movie[i])
          movie <- paste0(movie,"-(Remake)")
          url <- sprintf(numbers,movie)
          thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T)) 
        }
      }
    }
    
    if(grepl("*row.names' should specify one of the variables*",thing1)) {
      movie <- numbersClean(movie_list$movie[i])
      movie <- paste0(movie,"-(",movie_year,")")
      url <- sprintf(numbers,movie)
      thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T))
      
    }
    if(grepl("*row.names' should specify one of the variables*",thing1)) {
      movie <- numbersClean(movie_list$movie[i])
      movie <- paste0(movie,"-(Remake)")
      url <- sprintf(numbers,movie)
      thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T))
      
    }
    if(grepl("*row.names' should specify one of the variables*",thing1)) {
      movie <- paste0(movie,"-(UK)")
      url <- sprintf(numbers,movie)
      thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T))
    }
    if(grepl("*curl::curl*",thing1)) {
      movie <- numbersClean(movie_list$movie[i])
      movie <- paste0(movie,"-(",movie_year,")")
      url <- sprintf(numbers,movie)
      thing1 <- try(GET(url) %>% read_html() %>% html_nodes("h2+ table") %>% html_table() %>% data.frame(row.names=T))
    }
    if(class(thing1) != "try-error") {
      rownames(thing1) <- gsub("\\s","_",rownames(thing1))
      franchise <- thing1['Franchise:',]
      creative_type <- thing1['Creative_Type:',]
      budget <- gsub("\\$","",thing1['Production_Budget:',])
      budget <- as.numeric(gsub(",","",budget))
      distro <- gsub(",.*","",gsub(".* by ","",thing1['Domestic_Releases:',]))
      release_date <- str_extract(thing1['Domestic_Releases:',],'^[A-z]+ [0-9]+[a-z]{2}, [0-9]{4}')
      release_date <- gsub("[a-z]{2}, ", " ",release_date)
      release_date <- as.Date(release_date,format="%B %d %Y")
      genre <- thing1['Genre:',]
      source <- thing1['Source:',]
      method <- thing1['Production_Method:',]
      keywords <- thing1['Keywords:',]
      rating <- gsub(" ", "",str_extract(thing1['MPAA_Rating',],"^[A-Z]+ "))
      temp_df <- data.frame(movie = movie_list$movie[i], release_date, distro,franchise,creative_type,budget,genre,source,method,rating,keywords)
      movie_meta <- rbind(movie_meta,temp_df)
    }
  }
  
    i <- i+1

}
return(movie_meta)
}
#write.csv(movie_meta,"Data/numbers_data.csv",row.names = F)
