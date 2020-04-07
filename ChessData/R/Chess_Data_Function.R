#' @description Takes in a chess.com username, year, month, and number of months
#' as the 4 parameters and returns chess.com archived data about games within the time range
#' The chess.com game archive API will be downloaded and a table will be
#' produced with information regarding the user's games.
#' @param 4 Values: chess.com username (string), year (YYYY - numeric), month (numeric), number of months (numeric)
#' @return Table of information about the chess games
#' @export
chessdata <- function(username, year, month, nmonths = 1, class = NA, rules = NA,
                       moves = TRUE, position = TRUE) {

  library(tidyverse)
  library(ggplot2)
  library(jsonlite)
  library(bigchess)

  ## Test Values ##

  username = "mastermatthew52"
  year = 2020
  month = 1
  nmonths = 3



  # make a  copy of nmonths for use in loops
  n = nmonths


  # Verify User Exists
  profile_url <- paste0("https://api.chess.com/pub/player/",
                        username,sep = "", collapse = NULL)

  invalid_user <- try(fromJSON(toString(profile_url), flatten = TRUE), silent = TRUE)

  if(class(invalid_user) == "try-error"){
    stop("Invalid username - user does not exist")
  }

  # Verify the information entered is valid


  profile <- fromJSON(toString(profile_url), flatten = TRUE)

  date_joined <- profile$joined %>%
    as.POSIXct(origin="1970-01-01", tz="GMT")

  # The day will be 1 by default since it is the start of the month and  that's  all
  # the data we have.

  day <- 1

  date_given <- paste0(year,"-",month,"-",day, sep = "", collapse = NULL)

  date_given <- as.Date(date_given)

  if(date_given < date_joined){
    stop("No games for that user before the given month.")
  }

  if(nmonths >= 12){
    warning("Date rage of 12 or higher could take awhile to lead")
  }

  chess_PGN_data <- vector(mode = "list", n)
  jsonData <- vector(mode = "list", n)
  JSON_pgn_url <- vector(mode = "list", n)
  JSON_url <- vector(mode = "list", n)


  if (!is.null(nmonths) & nmonths != 1){

    i = 1;
    while (nmonths!=0) {

      if(month == 13) {month = 1; year = year + 1}
      if(month < 10){
        JSON_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/0", month,
                              sep = "", collapse = NULL)
        JSON_pgn_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/0", month,
                                  "/pgn", sep = "", collapse = NULL)

      }else{
        JSON_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/", month,
                              sep = "", collapse = NULL)
        JSON_pgn_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/", month,
                                  "/pgn", sep = "", collapse = NULL)

      } # end else months >= 10

      month <- month + 1;
      nmonths <- nmonths - 1;
      i <- i + 1;
    }

  }else {
    if(month < 10){
      JSON_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/0", month,
                            sep = "", collapse = NULL)
      JSON_pgn_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/0", month,
                                "/pgn", sep = "", collapse = NULL)

    }else{
      JSON_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/", month,
                            sep = "", collapse = NULL)
      JSON_pgn_url[i] <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/", month,
                                "/pgn", sep = "", collapse = NULL)

    } # end else months >= 10
  } # end else

  # Now the function should be able to grab multiple months out of the newly created vector

  # Load JSON data provided via chess.com API
  i <- 1
  while(i <= n) {

    # Start with the PGN data becasue it it turns out to be NULL, the collection of
    # the chesS_data is not needed.

    # Use try block to handle error produced by read.pgn when the data is empty.
    # If there is no link to go to there
    try_error <- try(
      { chess_PGN_data[[i]] <- read.pgn(toString(JSON_pgn_url[i]), add.tags = c("ECO", "Link")) },
      silent = TRUE
    )


    # If the line produced an error the month has no data.
    # Print a warning message
    # If the month does have data, execute the code

    # In the else add a variable that wouldl keep track of "good" months. The below dplyr commands will
    # create errors if there is no data in the tibble

    if(class(try_error) == "try-error") {
      warning("One or more months have no data. Months have been ignnored")
      # Make a dummy table to replace the month with to  avoid screwing up
      # remaining months

      # PGN  Data
      chess_PGN_data[[i]] <- tibble(
        "Event" = NA, "Site" = NA,"Date" = NA,"Round" = NA,
        "White" = NA,"Black" = NA,"Result" = NA,"ECO" = NA,
        "Link" = NA,"Movetext" = NA,"NMoves" = NA,"W1" = NA,
        "B1" = NA,"W2" = NA,"B2" = NA,"W3" = NA,
        "B3" = NA,"W4" = NA,"B4" = NA,"W5" = NA,
        "B5" = NA, "W6" = NA, "B6" = NA,"W7" = NA,
        "B7" = NA,"W8" = NA,"B8" = NA,"W9" = NA,
        "B9" = NA,"W10" = NA,"B10" = NA,"last.move" = NA,
        "complete.movetext" = NA, "W_B_moves" = NA,"W_K_moves" = NA,"W_N_moves" = NA,
        "W_O_moves" = NA,"W_Q_moves" = NA,"W_R_moves" = NA,"B_B_moves" = NA,
        "B_K_moves" = NA,"B_N_moves" = NA,"B_O_moves" = NA,"B_Q_moves" = NA,
        "B_R_moves" = NA,"B_moves" = NA, "K_moves" = NA,"N_moves" = NA,
        "O_moves" = NA,"Q_moves" = NA,"R_moves" = NA)

      jsonData[[i]] <- tibble("url" = NA,"pgn" = NA,"time_control" = NA,"end_time" = NA,
                              "rated" = NA,"fen" = NA,   "time_class" = NA,   "rules" = NA,"start_time" = NA,
                              "tournament"  = NA,"white.rating" = NA,   "white.result" = NA,   "white.@id" = NA,
                              "white.username" = NA,   "black.rating"  = NA, "black.result" = NA,
                              "black.@id" = NA,"black.username"  = NA)

      # Add a variable that will tell us if there is a row of NA values expected

      na_val <- TRUE

    }else{
      chess_PGN_data[[i]] <- read.pgn(toString(JSON_pgn_url[i]),
                                      add.tags = c("ECO", "Link"))

      # flatten will make any nested data "flat" or it's own colum
      jsonData[i] <- fromJSON(toString(JSON_url[i]), flatten = TRUE)

      na_val <- FALSE
    }
    i <- i + 1
  }

  i <- 1

  while(i <= n){
    # Added row name argument (problem explained below)
    chess_PGN_data_temp <- as.data.frame(chess_PGN_data[[i]], row.names = NULL)
    chess_data_temp <- as.data.frame(jsonData[[i]], row.names= NULL)


    # check if there is data - if yes, rename  the Link column

    chess_PGN_data_temp  <- chess_PGN_data_temp %>%
      rename(url = Link)


    # I had to take these off because in some months people may not have played
    # certain types of games meaning that column would be dropped.

    chess_data_temp <- chess_data_temp %>%
      mutate(match = NA) %>%
      mutate(tournament = NA) %>%
      mutate(start_time = NA)

    if(i==1){
      chess <- chess_data_temp
      chess_PGN_data_full <- chess_PGN_data_temp
    }else{
      chess <- chess %>%
        rbind(chess_data_temp, make.row.names = FALSE)

      chess_PGN_data_full <- chess_PGN_data_full %>%
        rbind(chess_PGN_data_temp, make.row.names = FALSE)
    }

    i <- i + 1


  }


  # Join the PGN and Chess tables by the unique URL
  chess <- chess %>%
    inner_join(chess_PGN_data_full, by = "url")



  chess <- as_tibble(chess)

  # remove rows that are not needed
  # site is always chess.com because we are using their API
  # pgn was already extracted via BigChess package. This information is redundant.

  chess <- chess %>%
    select(-Site, -pgn)
  # 4 columns were removed


  # Create New, Informative Columns

  # Column for who won the game

  chess <- chess %>%
    mutate(winner = if_else(Result == "1/2-1/2", "Draw", "")) %>%
    mutate(winner = if_else(Result == "1-0", White, winner)) %>%
    mutate(winner = if_else(Result == "0-1", Black, winner))


  # Column for how the game was won (i.e. checkmate, timeout, resign, draw)

  chess <- chess %>%
    mutate(ending = if_else(Result == "1/2-1/2", "Draw", "")) %>%
    mutate(ending = if_else(Result == "1-0", chess$black.result, ending)) %>%
    mutate(ending = if_else(Result == "0-1", chess$white.result, ending))

  # All columns that contain repeated data from  the pgn file can be deleted. This
  # includes most of the white and black data.


 chess <- chess %>%
    mutate(white_rating = chess$white.rating) %>%
    mutate(black_rating = chess$black.rating) %>%
    select(-`white.@id`, -`black.@id`, -white.result, -black.result, -white.username,
           -black.username, -white.rating, -black.rating)


  # Remove any rows that are NA due to the dummy table
  # The URL should always have a value.

  if(na_val){
    na_row_index <- which(is.na(chess$url), arr.ind= FALSE, useNames = TRUE)
    chess <- chess %>%
      slice(-na_row_index)
  }

  # Reorder the columns to make more sense logically
  # (i.e. putting the ratings  by the usernames not at the end.)

  chess <- chess %>%
    select(url, Date, time_control, time_class, rules, White, white_rating, Black,
           black_rating, ECO, Result, winner, ending, fen,
           NMoves, W1, B1, W2, B2, W3, B3, W4, B4, W5, B5, W6, B6, W7, B7, W8, B8, W9,
           B9, W10, B10, last.move, W_B_moves, W_K_moves, W_N_moves, W_O_moves,
           W_Q_moves, W_R_moves, B_B_moves, B_K_moves, B_N_moves, B_O_moves,
           B_Q_moves, B_R_moves, B_moves, K_moves, N_moves, O_moves,
           Q_moves, R_moves, Movetext)


  # Specific Data - Time Class

  if(!is.na(class)){
    class_index <- which(chess$time_class %in% class)
    chess <- chess %>%
      slice(class_index)
  }

  # Specific Data - Variation Rules


  if(!is.na(rules)){
    rules_index <- which(chess$rules %in% rules)
    chess <- chess %>%
      slice(rules_index)
  }

  # Move data

  if(!moves) {
    chess <- chess %>%
      select(-Movetext, -NMoves,-W1, -B1, -W2, -B2, -W3, -B3, -W4, -B4, -W5, -B5, -W6,
             -B6, -W7, -B7, -W8, -B8, -W9, -B9, -W10, -B10, -last.move,
             -W_B_moves, -W_K_moves, -W_N_moves, -W_O_moves,-W_Q_moves,-W_R_moves,
             -B_B_moves, -B_K_moves, -B_N_moves, -B_O_moves,-B_Q_moves,-B_R_moves,
             -B_moves, -K_moves, -N_moves, -O_moves,-Q_moves,-R_moves)

  }

  # Position Data

  if(!position) {
    chess <- chess %>%
      select(-fen)
  }

  # return finished tibble
  return(chess)

}
