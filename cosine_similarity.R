theaters_url <- paste0("http://www.boxofficemojo.com/counts/chart/?yr=",year(Sys.Date()),"&wk=",(week(Sys.Date())),"&p=.htm")
# theaters_url <- paste0("http://www.boxofficemojo.com/counts/chart/?yr=",year(Sys.Date()),"&wk=",week(Sys.Date()),"&p=.htm")
theaters_get <- GET(theaters_url) %>% read_html() %>% html_nodes("table") %>% html_table(fill=T)
theaters_df <- data.frame(theaters_get[[4]])
names(theaters_df) <- theaters_df[1,]
names(theaters_df) <- gsub("\\s","",names(theaters_df))
theaters_df <- theaters_df[c("Title","TheaterCount","Distributor")]
theaters_df$TheaterCount <- gsub(",","",theaters_df$TheaterCount)
theaters_df$TheaterCount <- as.numeric(theaters_df$TheaterCount)
names(theaters_df)[1] <- "movie"
names(theaters_df)[3] <- "distributor"
theaters_df$movie <-gsub("-"," ",theaters_df$movie)
theaters_df$movie <-gsub("[[:punct:]]","",theaters_df$movie)
theaters_df$movie <- gsub(" $","",theaters_df$movie)
Encoding(theaters_df$movie) <- "ASCII"
theaters_df$movie <- tolower(theaters_df$movie)

theaters_studio <- studios %>% transmute(numbers_name = coalesce(numbers_name,wiki_name), distributor=mojo_name) %>% filter(!is.na(distributor) & distributor!= "")

joining_studios <- studios %>% transmute(distributor=wiki_name, numbers_name=coalesce(numbers_name,wiki_name)) %>% filter(!is.na(distributor))

theaters_joined <- inner_join(theaters_df,theaters_studio, by="distributor") %>% group_by(movie, TheaterCount,distributor,numbers_name) %>% summarize()
theaters_joined$id <- paste0(theaters_joined$movie,theaters_joined$numbers_name)



movies_without_data <- now_playing %>% mutate(is_daily = ifelse(grepl("^SAT",movie),1,0)) %>% mutate(movie = gsub("(^FRI: )|(^SAT: )|(^SUN: )","",movie)) %>% 
  group_by(movie) %>% summarize(min_date = min(date), is_daily = max(is_daily)) %>%  filter(min_date>(weekend_dates[1]-days(7)) | is.na(min_date)) %>%
  left_join(theaters_df,by="movie") 
meta_data <- numbers_data %>% transmute(movie=movie,budget=budget, distributor=distro,release_date=as.Date(release_date),genre=genre,
                                        is_franchise=ifelse(!is.na(franchise),1,0), is_animated = ifelse(method!="Live Action",1,0),is_superhero=ifelse(creative_type=="Superhero",1,0),
                                        is_remake=ifelse(source=="Remake",1,0),is_kids=ifelse(creative_type=="Kids Fiction",1,0),rating=rating,top_dist = ifelse(distro %in% c("Walt Disney","Warner Bros.","Sony"),1,0)) %>%
                                        left_join(dim_genre,by="genre")
meta_data$movie <- clean_title(meta_data$movie)
  
i <- 1
num_df <- number_movie_scrapR(movies_without_data[i,])
if(nrow(num_df)>0){
  meta_movie <- num_df %>% transmute(movie=movie,budget=budget, distributor=distro,release_date=as.Date(release_date),genre=genre,
                                     is_franchise=ifelse(!is.na(franchise),1,0), is_animated = ifelse(method!="Live Action",1,0),is_superhero=ifelse(creative_type=="Superhero",1,0),
                                     is_remake=ifelse(source=="Remake",1,0),is_kids=ifelse(creative_type=="Kids Fiction",1,0),rating=rating,top_dist = ifelse(distro %in% c("Walt Disney","Warner Bros.","Sony"),1,0)) %>%
                                      left_join(dim_genre,by="genre")
  
  
  ## ADD IN YOUR LOGIC FOR JOINING ON COSINE SIMILARITY HERE #####
    meta_movie$id <- paste0(meta_movie$movie,meta_movie$distributor)
    theaters_joined$qdist <- stringdistmatrix(meta_movie$id, theaters_joined$id, method='cosine',q=2)[1,]
    theaters_joined$movie <- ifelse(theaters_joined$qdist<= .1,meta_movie$movie,theaters_joined$movie)
    
  
  meta_movie$theaters <- movies_without_data$TheaterCount[i]
}
    

id1 <- "christopher robin"
id3 <- "disneys christopher robin"
id2 <- 'disneys christopher'

stringdist(id1,id2,method="qgram",q=2)



stringdist(id2,id1,method='cosine', q=1)
stringdist(id2,id1,method='cosine', q=2)
stringdist(id2,id3,method='cosine', q=2)
now_playing <- right_join(updated_boxoffice, fml_movies, by="movie")

meta_data$id <- paste0(meta_data$movie,meta_data$distributor)
meta_movies <- meta_data %>% group_by(id, movie) %>% summarize() 
i <- 1
while (i < nrow(now_movies)) {
  movie_id <- now_movies$id[i]
  movie_name <- now_movies$movie[i]
  theaters_joined$qdist <- stringdistmatrix(movie_id, theaters_joined$id, method='cosine',q=2)[1,]
  theaters_joined$movie <- ifelse(theaters_joined$qdist<= .1,movie_id,theaters_joined$movie)
  i <- i+1
}




theaters_joined$id[5]
