## server.R

# load functions
#source('functions/cf_algorithm.R') # collaborative filtering
#source('functions/similarity_measures.R') # similarity measures
#source('functions/genre_algorithm.R') # genre_based recommendation

#define functions to get ratings from online data

get_user_genre = function(value_list) {
  return(value_list$selected_genre)
}

# define functions to get input of a new user rating
get_user_ratings = function(value_list) {
  #print(value_list)
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}


############################ read in data ##############################################

#------------Movie Data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))

movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# #---------------User Data
users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
#######################finish read in data###########################


readin_data = function() {
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  ratings$Timestamp = NULL
  return(ratings)
}
ratings = readin_data()

predict_genre_helper = function(user_genre) {
  print("user picked genre: ")
  print(user_genre)
  n_display = 10
  l.ratings = readin_data()
  movies.genre = movies %>%
    filter(grepl(user_genre,movies$Genres))
  tmp.12 = l.ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig = 3)) %>%
    inner_join(movies.genre, by = 'MovieID') %>%    
    filter(ratings_per_movie > 1000) %>%
    top_n(n_display, ratings_per_movie) %>%
    arrange(desc = -1 * ave_ratings)
  print("line 84")
  print(tmp.12)
  return(tmp.12)
}

genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical",
               "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")

genre_inilize = function() {
  l_tmp = hash()
  for (genre.item in genre_list) {
    l_tmp[[genre.item]] = predict_genre_helper(readin_data(), movies, genre.item)
  }

predict_genre = function(user_genre) {
     return(l_tmp[[user_genre]])
  }
}


gen_Rmat = function() {  
  i = paste0(ratings$UserID)
  j = paste0(ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  return(Rmat)
}

initialize_UBCF = function() {
  Rmat = gen_Rmat()
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                         parameter = list(normalize = 'Z-score', 
                                          method = 'Cosine', 
                                          nn = 25)) 
  return(rec_UBCF)
}
#rec_UBCF = initialize_UBCF();

predict_UBCF = function(new.Rmat) {
  rec_UBCF = initialize_UBCF();  
  recom = predict(rec_UBCF , new.Rmat, type = 'ratings')  
  
  movie.tmp = as(recom, "matrix")[1,]
  ids = movie.tmp[order(movie.tmp, decreasing = TRUE)[1:10]]
  tmp = data.frame(MovieID = as.integer(names(ids)), pred_ratings = ids, row.names = NULL)
  
  movies.tmp = movies %>%
    inner_join(tmp, by = 'MovieID') %>%
    arrange(desc = -1* pred_ratings)     
  
  print("line: 154")
  print(movies.tmp)
  return(movies.tmp)
}

gen_newRmat = function(user_ratings) {
  print(class(user_ratings))
  Rmat = gen_Rmat()
  n.item = ncol(Rmat)
  new.ratings = rep(NA, n.item)
  print(class(new.ratings))
  #new.ratings = vector(length = n.item)
  
  #new.ratings = matrix(NA, nrow= 1, ncol = n.item)
  #new.ratings[1:4] = 4

  names(new.ratings) = colnames(Rmat)
  # 
  for (i in 1:NROW(user_ratings)) {
       
        print(i)
      print(user_ratings[i,])
       
      #new.ratings[which(colnames(Rmat) == paste0(user_ratings[i,1]))]
      new.ratings[paste0(user_ratings[i,1])] = user_ratings[i,2]
       #new.ratings[user_ratings[i,1]] = user_ratings[i,2]
       print(new.ratings[[paste0(user_ratings[i,1])]])
       #print(new.ratings)
  }
  #print(new.ratings[1:10,])
  
  #print(new.ratings)
  new.user = matrix(new.ratings,
                    nrow=1, ncol=n.item,
                    dimnames = list(
                      user=paste('feng'),
                      item=colnames(Rmat)
                    ))
  print(sum(!is.na(new.user)))
  #print(new.user)
  
  new.Rmat = as(new.user, 'realRatingMatrix')     

  return(new.Rmat)
}


shinyServer(function(input, output, session) {
  
  ######################################System 1#############################################
  # Calculate recommendations when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's input genre
      value_list <- reactiveValuesToList(input)
      user_genre <- get_user_genre(value_list)
      print(user_genre)
     
      tmp = predict_genre_helper (user_genre)
      #tmp = predict_genre (user_genre)
      #print(tmp)
      
      user_results = tmp$ave_ratings#(1:10)/10
      user_predicted_ids = tmp$MovieID #rep(1,10)
      recom_result <- data.table(Rank = 1:10, 
                                  MovieID = tmp$MovieID, # movies$MovieID[user_predicted_ids], 
                                  Title = tmp$Title, #movies$Title[user_predicted_ids], 
                                  Predicted_rating =  tmp$ave_ratings,
                                  ImageUrl = tmp$image_url) #user_results)
      print("line 225")
      print(recom_result)
      return(recom_result)
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations for system1
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    
    recom_result <- df1()
    print("239")
    print(recom_result)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                #a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
                a(img(src = recom_result$ImageUrl[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                #strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
  
  ######################################System 2#############################################
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", 
                     ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      print(user_ratings)
      
      #print("***************user ratings: *******************")
      #print(user_ratings)
      
      # # add user's ratings as first column to rating matrix
      # rmat <- cbind(user_ratings, ratingmat)
      # 
      # # get the indices of which cells in the matrix should be predicted
      # # predict all books the current user has not yet rated
      # items_to_predict <- which(rmat[, 1] == 0)
      # prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      # 
      # # run the ubcf-alogrithm
      # res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
      # 
      # # sort, organize, and return the results
      # user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
      # user_predicted_ids <- as.numeric(names(user_results))
      # recom_results <- data.table(Rank = 1:20, 
      #                             Book_id = user_predicted_ids, 
      #                             Author = books$authors[user_predicted_ids], 
      #                             Title = books$title[user_predicted_ids], 
      #                             Predicted_rating =  user_results)
      
      newRmat = gen_newRmat(user_ratings)
      #print(as(newRmat, "matrix"))
      movies.tmp = predict_UBCF(newRmat)
      #user_results = (1:10)/10
      #user_predicted_ids = 1:10
      user_results = movies.tmp$pred_ratings  #(1:10)/10
      user_predicted_ids = movies.tmp$MovieID #1:10
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function