#' @description Takes in a chess.com username, year (YYYY), and month (MM) as three strings.
#' The chess.com game archive API will be downloaded and a table will be
#' produced with information regarding the user's games.
#' @param 3 Strings: chess.com username, year (YYYY), and month (MM)
#' @return Table of information about the games archived
#' @export
chessdata <- function(username, year, month) {

library(tidyverse)
library(jsonlite)

JSON_url <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/", month,
                   sep = "", collapse = NULL)


# Load JSON data provided via chess.com API
jsonData <- fromJSON(JSON_url)
chess <- as_tibble(jsonData$games)

### Variable to work with: ###

# url: Provides a URL to the online game stored in the user's archive
# pgn: "Portable Game Notation" - A way of recording a game's details and moves
  # so that it can be loaded later on.
# time_control: the time control used in the game (in seconds)
# end_time: timestamp of the end time of the game -- unrelated to time_control
# rated: boolean value stating if the game was rated or not
# fen: "Forsyth-Edwards Notation": Used to describe a particular position on a
  # chess board. This can be used to view a position at any given time throughout
  # a chess game. In this case we  have the ending fen.
# time_class: what class of time control was use: (i.e. game with time_control
  # under 2|1 are considered "bullet" games)
# rules: What type  of chess is being played. There are many variations
# white & black:
  # Username of who is playing what side
  # rating of  each player
  # result of the game: result of who won/lost and how
  # @id: url of player's profile on chess.com
#start_time: timestamp of the game start (only applys to time_class "Daily")
# tournament: url pointing to the tournament the game was part of if applicable
# match: url point to the team match the game was a part of if applicable




########## CLEANING THE DATA ##########


# The pgn is very messy, but it could be useful to have once it is cleaned up
# The Completed Games  (ref: https://www.chess.com/news/view/published-data-api)
# Claims there is an ECO (Encycopedia of Chess Openings) code to  find out
# what openings were played in the game. Part of this analysis is going to
# be looking at different openings and win rates, etc. so it would be nice to
# have a column for them. The ECO code IS in the data, but it is hidden in
# the PGN (A PGN typically has the ECO code for reference)
# I will need to extract that manually.


# I found  a nice library called bigchess: (ref: https://rdrr.io/cran/bigchess/man/read.pgn.html)
# This library can be used to read a PGN file and turn it into a data frame.
# This makes it easy to extract data
# from a pgn file. ECO is an option to extract

library(bigchess)

# Make this more dynamic

JSON_pgn_url <- paste0("https://api.chess.com/pub/player/", username, "/games/", year, "/", month,
                       "/pgn", sep = "", collapse = NULL)


chess_PGN_data <- read.pgn(JSON_pgn_url, add.tags = c("ECO", "Link"))


# Now I need to join this new PGN table to the original chess table.

chess_PGN_data <- as_tibble(chess_PGN_data)

# I need to have a column to join the tables on. I have the unique URL in
# the chess_data table, and I have the Link in the chess_PGN_data which I
# Specified to be added. I can use these 2 columns  to create a unique ID
# to join the PGN data to the game data.
# it is. The tables both have a way to reference eachother

chess_PGN_data <- chess_PGN_data %>%
  rename(url = Link)

chess <- chess %>%
  inner_join(chess_PGN_data, by = "url")

### Remove Variables that are not needed ###

chess <- chess %>%
  filter(time_class != "daily" & time_class != "rapid") %>%
  filter(rules == "chess")

chess <- chess %>%
  select(-pgn, -end_time, -rated, -start_time, -tournament,
         -match, -Event, -Site, -Movetext, -rules, -Round)


# Column for who won the game

chess <- chess %>%
  mutate(winner = if_else(Result == "1/2-1/2", "Draw", "")) %>%
  mutate(winner = if_else(Result == "1-0", White, winner)) %>%
  mutate(winner = if_else(Result == "0-1", Black, winner))

# Column for how the game was won (i.e. checkmate, timeout, resign, draw)

chess <- chess %>%
  mutate(ending = if_else(Result == "1/2-1/2", "Draw", "")) %>%
  mutate(ending = if_else(Result == "1-0", chess$black$result, ending)) %>%
  mutate(ending = if_else(Result == "0-1", chess$white$result, ending))


# Clear up the confusing white and black columns and separate them:

chess <- chess %>%
  mutate(white_rating = chess$white$rating) %>%
  mutate(black_rating = chess$black$rating) %>%
  select(-white, -black)


# Reorder the table to make more sense


chess <- chess %>%
  select(url, Date, time_control, time_class, White, white_rating,
         Black, black_rating, ECO, Result, winner, ending, fen, NMoves,
         W1, B1, W2, B2, W3, B3, W4, B4, W5, B5, W6, B6, W7, B7, W8, B8, W9,
         B9, W10, B10, last.move, W_B_moves, W_K_moves, W_N_moves, W_O_moves,
         W_Q_moves, W_R_moves, B_B_moves, B_K_moves, B_N_moves, B_O_moves,
         B_Q_moves, B_R_moves, B_moves, K_moves, N_moves, O_moves,
         Q_moves, R_moves)



return(chess)

}




