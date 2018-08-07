library(rvest)
get_boxoffice <- function(dates) {
  movies <- data.frame()
  # get the URL 
  url_start <- "https://www.the-numbers.com/box-office-chart/daily/"
  # get date list
  url_dates <- gsub("-", "/", dates)
  i <- 1
  while (i < length(url_dates)) {
    thing1 <- GET(paste0(url_start,url_dates[i])) %>% read_html() %>% html_nodes("#page_filling_chart center:nth-child(2) > table") %>% html_table
    thing1 <- thing1[[1]]
    thing1[1:2]<-NULL
    names(thing1) <- c("movie","distributor","gross","percent_change","theaters", "per_theater","total_gross","days")
    j <- 1
    for (j in 3:ncol(thing1)) {
      thing1[,j] <- as.integer(gsub("[[:punct:]]","",thing1[,j]))
    }
    
    thing2 <- GET(paste0(url_start,url_dates[i])) %>% read_html() %>% html_nodes("#page_filling_chart td:nth-child(3) a") %>% html_attr('href')
    thing2 <- gsub("/movie/","",thing2)
    thing2 <- gsub("#.*","",thing2)
    thing2 <- gsub("-"," ",thing2)
    thing2 <- gsub("(\\([0-9]+\\))$","",thing2)
    thing2 <- gsub("(\\([A-z ]+\\))$","",thing2)
    
    k <- 1
    for (k in 1:length(thing2)){
      if(grepl(" The$",thing2[k])){
        thing2[k] <- gsub(" The$", "",thing2[k])
        thing2[k] <- paste0("The ",thing2[k])
      }
      if(grepl(" A $",thing2[k])){
        thing2[k] <- gsub(" A $", "",thing2[k])
        thing2[k] <- paste0("A ",thing2[k])
      }
    }
    
    t2 <- data.frame(thing2)
    temp <- cbind(t2$thing2,thing1[2:8],date=dates[i])
    names(temp)[1]<- "movie"
    movies <- rbind(movies,temp)
    i <- i+1
  }
  return(movies)
}

