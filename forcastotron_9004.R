library (rvest)
library(httr)
library(stringr)
library(boxoffice)
library(data.table)
library(prophet)
library(lubridate)
library(textclean)
library(stringdist)
#source("RFiles/wiki_movie_scrapR.R")
source("number_movie_scrapR.R")
source("new_movie_Scraper.R")
library(tidyverse)
options(scipen=999999,stringsAsFactors = F)

predict_opening <- function(temp_df) {
  has_pred = F
  # if it has budget and theaters
  if(!is.na(temp_df$budget) & !is.na(temp_df$theaters) & has_pred==F) {
    model <- lm(gross ~budget + theaters+is_franchise+is_kids+is_animated, data=to_pred)
    pred <- predict(model, temp_df)
    has_pred = T
    # if it has theaters
  } else if(!is.na(temp_df$theaters) & has_pred==F) {
    model <- lm(gross ~theaters+is_franchise+is_kids+is_animated, data=to_pred)
    pred <- predict(model, temp_df)
    has_pred = T
    # if it has budget
  } else if(!is.na(temp_df$budget) & has_pred==F) {
    model <- lm(gross ~budget +is_franchise+is_kids+is_animated, data=to_pred)
    pred <- predict(model, temp_df)
    has_pred = T
  } else {
    pred <- round(mean(to_pred$gross))
  }
  return(pred)
}
get_weekend <- function(x) {
  wk <- seq.Date(x,x+6,by="days")
  wk_df <- data.frame(date=wk,day=weekdays(wk))
  wk_df <- filter(wk_df,day=="Friday"|day=="Saturday"|day=="Sunday")
  date_list <- wk_df$date
  return(date_list)
}
clean_title <- function(x) {
  x <-gsub("-"," ",x)
  x <-gsub("[[:punct:]]","",x)
  x <- gsub(" $","",x)
  Encoding(x) <- "ASCII"
  x <- tolower(x)
  
}


# LOAD DATA
studios <- fread("Data/distro_list.csv")
#past_data <- fread("boxoffice_scores.csv")
past_data <- fread("corrected_movie_data.csv")
wiki_data <- fread("Data/wiki_data.csv")
numbers_data <- fread("Data/numbers_meta_data.csv")
dim_genre <- fread("Data/dim_genre.csv")
# CLEAN DATA
#past_data$filter <- NULL
past_data$date <- as.Date(past_data$date)

# GET WEEKEND TO FORECAST
weekend_dates  <- get_weekend(Sys.Date())


# BOX OFFICE SCRAPER ####
last_date <- max(past_data$date)
 end_date <- Sys.Date()-1
 start_date <- as.Date(last_date)+1
#start_date <- as.Date("2018-07-20")
#end_date <- as.Date("2018-07-26")
if(as.Date(last_date) < (Sys.Date()-1)) {
  date_list <- seq.Date(start_date,end_date,by="days")
  
  box_office <-  get_boxoffice(date_list)
  updated_boxoffice <- rbind(past_data,box_office)
  write.csv(updated_boxoffice, "boxoffice_scores.csv",row.names = F)
  
} else {
  updated_boxoffice <- past_data
}
updated_boxoffice <- fread("boxoffice_scores.csv")
updated_boxoffice$movie <- tolower(updated_boxoffice$movie)
updated_boxoffice$movie <-gsub("-"," ",updated_boxoffice$movie)
updated_boxoffice$movie <-gsub("[[:punct:]]","",updated_boxoffice$movie)
updated_boxoffice$movie <- gsub(" $","",updated_boxoffice$movie)

# GET MOVIE BUX ####
fml <- "https://fantasymovieleague.com/researchvault?section=box-office"
fml_table <- GET(fml) %>% read_html() %>% html_nodes(".movie-title--with-cost-and-image a") %>% html_text() %>% data.frame()
fml_table2 <- GET(fml) %>% read_html() %>% html_nodes(".movie-title--with-cost-and-image+ td") %>% html_text() %>% data.frame()
fml_table <- cbind(fml_table,fml_table2)
colnames(fml_table)[1] <- "name"
fml_table$bux <- gsub("^.*[FB$]","",fml_table$name)
fml_table$movie <- gsub("FB\\$\\d*$","",fml_table$name)
fml_table$movie <- gsub("-"," ",fml_table$movie)
fml_table$movie <- gsub("[[:punct:]]","",fml_table$movie)
fml_table$movie <- gsub("– ","",fml_table$movie)
fml_table$name <- NULL
fml_table$movie <- tolower(fml_table$movie)
fml_table$. <- ifelse(fml_table$. == "-","new","old")

# GET MOVIES IN THEATERS' DATA

# get list of movies 

updated_boxoffice$movie <- gsub("’","'",updated_boxoffice$movie)
fml_movies <- fml_table %>% mutate(is_split=grepl("(^FRI )|(^SAT )|(^SUN )",movie),movie = gsub("(^FRI )|(^SAT )|(^SUN )","",movie)) %>% mutate(movie=gsub("’","'",movie)) %>% distinct(movie, is_split)

i <- 1
updated_boxoffice$movie_friendly <- updated_boxoffice$movie
while (i <= nrow(fml_movies)) {
  movie_id <- fml_movies$movie[i]
  updated_boxoffice$qdist <- stringdistmatrix(movie_id, updated_boxoffice$movie, method='cosine',q=2)[1,]
  min_q <- min(updated_boxoffice$qdist)
  updated_boxoffice$movie_friendly <- ifelse(updated_boxoffice$qdist==min_q,movie_id,updated_boxoffice$movie_friendly)
  i <- i+1
}
filter_boxoffice <- filter(updated_boxoffice, !is.na(updated_boxoffice$movie_friendly))


now_playing <- right_join(updated_boxoffice, fml_movies, by=c("movie_friendly"="movie"))
now_playing$movie <- now_playing$movie_friendly


# get number of theaters
theaters_url <- paste0("http://www.boxofficemojo.com/counts/chart/?yr=",year(Sys.Date()),"&wk=",(week(Sys.Date())),"&p=.htm")
# theaters_url <- paste0("http://www.boxofficemojo.com/counts/chart/?yr=",year(Sys.Date()),"&wk=",week(Sys.Date()-1),"&p=.htm")
theaters_get <- GET(theaters_url) %>% read_html() %>% html_nodes("table") %>% html_table(fill=T)
theaters_df <- data.frame(theaters_get[[4]])
names(theaters_df) <- theaters_df[1,]
names(theaters_df) <- gsub("\\s","",names(theaters_df))
theaters_df <- theaters_df[c("Title","TheaterCount")]
theaters_df$TheaterCount <- gsub(",","",theaters_df$TheaterCount)
theaters_df$TheaterCount <- as.numeric(theaters_df$TheaterCount)
names(theaters_df)[1] <- "movie"
theaters_df$movie <-gsub("-"," ",theaters_df$movie)
theaters_df$movie <-gsub("[[:punct:]]","",theaters_df$movie)
theaters_df$movie <- gsub(" $","",theaters_df$movie)
Encoding(theaters_df$movie) <- "ASCII"
theaters_df$movie <- tolower(theaters_df$movie)


joining_studios <- studios %>% transmute(distributor=wiki_name, numbers_name=coalesce(numbers_name,wiki_name)) %>% filter(!is.na(distributor))

meta_data <- numbers_data %>% transmute(movie=movie,budget=budget, distributor=distro,release_date=as.Date(release_date),genre=genre,
                                        is_franchise=ifelse(!is.na(franchise),1,0), is_animated = ifelse(method!="Live Action",1,0),is_superhero=ifelse(creative_type=="Superhero",1,0),
                                        is_remake=ifelse(source=="Remake",1,0),is_kids=ifelse(creative_type=="Kids Fiction",1,0),rating=rating,top_dist = ifelse(distro %in% c("Walt Disney","Warner Bros.","Sony"),1,0)) %>%
                                        left_join(dim_genre,by="genre")
meta_data$movie <- clean_title(meta_data$movie)

# BEGIN MOVIE FORECASTING ####
movies_with_data <- now_playing %>% group_by(movie) %>% summarize(min_date = min(date)) %>%  filter(min_date<=(weekend_dates[1]-days(7)))

movies_with_data <- now_playing %>% group_by(movie) %>% summarize(min_date = min(date)) %>%  filter(min_date<=(weekend_dates[1]-days(7)) & year(min_date)==year(Sys.Date()))
movies_with_data <- movies_with_data$movie
movies_to_forecast <- subset(now_playing, movie %in% movies_with_data) 

i<-1
forecasted <- data.frame()
for(i in 1:length(movies_with_data)) {
  movie <- subset(movies_to_forecast, movie==movies_with_data[i]) 
  unique(movie$movie)
  #n <- .5
  #n <- -15
   
 # n <- 0
  df <- movie %>% transmute(ds = date, y = log(gross), y_og = gross, theaters=theaters)
  
  #floor <- 0
  cap_df <- df %>% filter(ds > max(as.Date(ds))-7)
  #cap <- max(cap_df$y)
  cap <- 0
  floor <- 0
  if(nrow(movie < 7)) {
    n <- .2 # wk2 movies?
    floor <- min(df$y)+n
    cap <- max(df$y)
    df$floor <- floor
    df$cap <- max(df$y)
    m <- prophet(weekly.seasonality = T, changepoint.prior.scale = .05, growth = "logistic")
  } else {
    m <- prophet(weekly.seasonality = T, changepoint.prior.scale = .05, growth = "linear")
  }
  
 # m <- add_regressor(m, name="theaters")
  m <- fit.prophet(m, df)
  #m <- prophet(df, weekly.seasonality = T, changepoint.prior.scale = .05, growth = "logistic")
  #m <- prophet(df, weekly.seasonality = T, changepoint.prior.scale = .7, growth="linear")
  future <- make_future_dataframe(m, 7)
  #theaters <- theaters_df[theaters_df$movie==unique(movie$movie),] 
  #future$theaters <- theaters$TheaterCount 
  future$cap <- cap
  future$floor <- floor
  forecast <- predict(m,future)
  plot(m,forecast)
  weekend_gross <- forecast %>% transmute(expected = exp(yhat), date = ds, movies=movies_with_data[i]) %>% filter(as.Date(date) %in% weekend_dates)
  if(movie$is_split[1]==T) {
    fri <- forecast %>% transmute( movies=paste0("FRI: ",movies_with_data[i]), expected_gross = exp(yhat), date = ds) %>% filter(as.Date(date) == weekend_dates[1])
    sat <- forecast %>% transmute( movies=paste0("SAT: ",movies_with_data[i]), expected_gross = exp(yhat), date = ds) %>% filter(as.Date(date) == weekend_dates[2])
    sun <- forecast %>% transmute( movies=paste0("SUN: ",movies_with_data[i]), expected_gross = exp(yhat), date = ds) %>% filter(as.Date(date) == weekend_dates[3])
    
     total_gross <- rbind(fri,sat,sun)
     total_gross$date <- NULL
  } else {
    total_gross <- weekend_gross %>% group_by(movies) %>% summarise(expected_gross=sum(expected))
  }
  total_gross
  forecasted <- rbind(forecasted,total_gross)
}

# PREDICT MOVIES WITH NO DATA ####



movies_without_data <- now_playing %>% mutate(is_daily = ifelse(grepl("^SAT",movie),1,0)) %>% mutate(movie = gsub("(^FRI: )|(^SAT: )|(^SUN: )","",movie)) %>% 
                      group_by(movie) %>% summarize(min_date = min(date), is_daily = max(is_daily)) %>%  filter(min_date>(weekend_dates[1]-days(7)) | is.na(min_date))# %>%
                      #left_join(theaters_df,by="movie") 
i <- 1
theaters_df$movie_friendly <- NA
while (i <= nrow(movies_without_data)) {
  movie_id <- movies_without_data$movie[i]
  theaters_df$qdist <- stringdistmatrix(movie_id, theaters_df$movie, method='cosine',q=2)[1,]
  min_q <- min(theaters_df$qdist)
  theaters_df$movie_friendly <- ifelse(theaters_df$qdist==min_q,movie_id,theaters_df$movie_friendly)
  i <- i+1
}

movies_without_data <- left_join(movies_without_data,theaters_df,by=c("movie"="movie_friendly"))


i <- 1
new_movies <- data.frame()
while (i <= nrow(movies_without_data)){
movie <- movies_without_data$movie[i]
  if(!(movie %like% "BotR *")){
    num_df <- number_movie_scrapR(movies_without_data[i,])
    if(nrow(num_df)>0){
      meta_movie <- num_df %>% transmute(movie=movie,budget=budget, distributor=distro,release_date=as.Date(release_date),genre=genre,
                                              is_franchise=ifelse(!is.na(franchise),1,0), is_animated = ifelse(method!="Live Action",1,0),is_superhero=ifelse(creative_type=="Superhero",1,0),
                                              is_remake=ifelse(source=="Remake",1,0),is_kids=ifelse(creative_type=="Kids Fiction",1,0),rating=rating,top_dist = ifelse(distro %in% c("Walt Disney","Warner Bros.","Sony"),1,0)) %>%
                                                left_join(dim_genre,by="genre")
    
      
      
      # cosine similarity above
      meta_movie$theaters <- movies_without_data$TheaterCount[i]
      
      distro <- meta_movie$distributor
      release_date_id <- meta_movie$release_date
      release_month <- month(release_date_id)
      movie_genre <- meta_movie$genre_group
      
      comp_list <- meta_data %>% filter((genre_group == movie_genre | month(release_date)==release_month))
      
      if(movies_without_data$is_daily[i]==1) {
        # FRIDAY
          to_pred <- updated_boxoffice[updated_boxoffice$days==1]  %>% group_by(movie) %>% summarise(gross=sum(gross),theaters=mean(theaters)) %>% inner_join(comp_list,by="movie")
          pred <- predict_opening(meta_movie)
          temp_friday <- data.frame(movies = paste0("FRI: ",movie), expected_gross = pred)
        # SATURDAY
          to_pred <- updated_boxoffice[updated_boxoffice$days==2]  %>% group_by(movie) %>% summarise(gross=sum(gross),theaters=mean(theaters)) %>% inner_join(comp_list,by="movie")
          pred <- predict_opening(meta_movie)
          temp_saturday <- data.frame(movies = paste0("SAT: ",movie), expected_gross = pred)
        # SUNDAY
          to_pred <- updated_boxoffice[updated_boxoffice$days==3]  %>% group_by(movie) %>% summarise(gross=sum(gross),theaters=mean(theaters)) %>% inner_join(comp_list,by="movie")
          pred <- predict_opening(meta_movie)
          temp_sunday <- data.frame(movies = paste0("SUN: ",movie), expected_gross = pred)
          
           temp_df_2 <- rbind(temp_friday,temp_saturday,temp_sunday)
      } else{
        to_pred <- updated_boxoffice[updated_boxoffice$days<=3]  %>% group_by(movie) %>% summarise(gross=sum(gross),theaters=mean(theaters)) %>% inner_join(comp_list,by="movie")
        pred <- predict_opening(meta_movie)
        temp_df_2 <- data.frame(movies=movie,expected_gross = pred)
        }
      new_movies <- rbind(new_movies, temp_df_2)
      }
  }
i <- i+1
}

total_movies <- rbind(forecasted,new_movies)
fml_pred <- merge(total_movies,fml_table, by.x="movies",by.y="movie")
names(fml_pred) <- c("movie","forecast","bux")
fml_pred$bux <- as.numeric(fml_pred$bux)
fml_pred$per_bux <- fml_pred$forecast/fml_pred$bux
fml_pred$forecast[fml_pred$per_bux==max(fml_pred$per_bux)] <- fml_pred$forecast[fml_pred$per_bux==max(fml_pred$per_bux)] + 2000000
write.csv(fml_pred,paste0("predictions_",weekend_dates[2],".csv"))
# LET'S DO THIS
set.seed(18963)
iter <- 0
screens=data.frame()
while(iter<=500000){
  s1 <- sample(1:nrow(fml_pred),8,replace = T)
  if(sum(fml_pred[s1,]$bux)<1001&sum(fml_pred[s1,]$bux)>980&length(unique(fml_pred[s1,]$movie))<5)
  {
    s1 <- sort(s1)
    lineup <- paste(fml_pred[s1,]$movie,collapse=", ")
    pred_total <- sum(fml_pred[s1,]$forecast)
    total_bux <- sum(fml_pred[s1,]$bux)
    screens <- rbind(screens,data.frame(lineup,pred_total,total_bux))
  } 
  iter <- iter+1
}
print(paste0("Meatbag! The winning lineup is: ",unique(screens$lineup[screens$pred_total==max(screens$pred_total)]),". It will make you: $",
             unique(screens$pred_total[screens$pred_total==max(screens$pred_total)]),
             ". Use it or not. I don't care. I'm a fucking robot."))


