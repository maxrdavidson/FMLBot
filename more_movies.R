library(boxoffice)
library(tidyverse)

thing1 <- boxoffice(as.Date("2018-07-21"),site="mojo")
thing2 <- boxoffice(as.Date("2018-07-21"),site="numbers")
thing2$movie <- gsub("’","'",thing2$movie)
start_date <- as.Date("2016-03-04")
end_date <- as.Date("2018-07-20")

date_list <- seq.Date(start_date,end_date,by="days")
numbers <- boxoffice(date_list,site="numbers")
mojo <-boxoffice(date_list,site="mojo")
numbers$movie <-  gsub("’","'",numbers$movie)

mojo$id <- paste(substring(mojo$movie,1,10),mojo$theaters,mojo$days,mojo$date,sep="_")
numbers$id <- paste(substring(numbers$movie,1,10),numbers$theaters,numbers$days,numbers$date,sep="_")
numbers <- numbers[,c("id","distributor")]
all_movies <- merge(mojo,numbers,by="id")

past_data$movie <- gsub("’","'",past_data$movie)
past_data$id <- paste(substring(past_data$movie,1,10),past_data$theaters,past_data$days,past_data$date,sep="_")

n_m <- read.csv("corrected_movies.csv")
n_m$id <- paste(substring(n_m$movie,1,10),n_m$theaters,n_m$days,n_m$date,sep="_")
dist <- merge(n_m,past_data,by="id")




numbers$id <- paste(substring(numbers$movie,1,10),numbers$theaters,numbers$days,numbers$date,sep="_")
mojo$id <- paste(substring(mojo$movie,1,10),mojo$theaters,mojo$days,mojo$date,sep="_")
numbers <- numbers[,c("id","distributor")]
all_movies <- merge(mojo,numbers,by="id")
names(all_movies)[11] <- "distributor"
all_movies$distributor.x<- NULL
write.csv(all_movies, "corrected_movies.csv")
new_movies[3:7] <- NULL
new_movies[4] <- NULL
names(new_movies) <- c("movie","distributor","days","id","gross","percent_change","theaters","per_theater","total_gross","date", "distributor_Y")
new_movies <- new_movies[,c(1,2,5,6,7,8,9,3,10)]
write.csv(new_movies, "corrected_movies.csv")
new_movies <- inner_join(past_data,all_movies,by=c("movie","days"))


past_data