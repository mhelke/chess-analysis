\name{chessdata}
\alias{chessdata}
\title{Gathers A Chess.com User's Game Data}
\description{Takes chess.com username and list of months to download game data from. The chess.com game archive API will be downloaded and a table will be produced with information regarding the user's games.}
\usage{chessdata(username, year, month, nmonths = 1, class = NA, rules = NA, moves = TRUE, position = TRUE)}
\arguments{
  \item{username}{chess.com username as a string}
  \item{year}{year of data to be downloaded as a numeric value (YYYY format)}
  \item{month}{month of data to be downloaded as a numeric value}
  \item{nmonths}{Number of months to be downloaded. Default is one month}
  \item{class}{Specifies if only a certain type of time control should be downloaded. Default is all time controls.}
  \item{rules}{What variant of chess should be downloaded. Default is all variations.}
  \item{moves}{Should move data be downloaded. Default is TRUE}
  \item{position}{Should ending position data be downloaded. Default is TRUE}
}
\details{
}
\value{
    Dataframe with the following variables:

    \item{url}{Link to the game that was played on chess.com}
    \item{Date}{Date  of game played on chess.com}
    \item{time_control}{Time control of the game that was played}
    \item{fen}{Final position of the game}
    \item{time_class}{what kind of time control class was used. Values could be: Bullet, Blitz, Rapid, or Daily}
    \item{rules}{What variation of chess was played. Values could be: chess, bughouse, crazyhouse, threecheck, kingofthehill, or chess960}
    \item{white}{Who played the white pieces}
    \item{white_rating}{Rating of the player playing white}
    \item{Black}{Who played  the black pieces}
    \item{black_rating}{Rating of the player playing black}
    \item{ECO}{ECO Code of the opening played in the game}
    \item{Result}{Which side won the game. Values could be: 1-0, 0-1, 1/2-1/2}
    \item{Winner}{Username of the winner of the game}
    \item{ending}{How did the game end}
    \item{fen}{Final position in FEN format}
    \item{NMoves}{Number of moves in the game}
    \item{W1-W10}{First 10 moves made by white}
    \item{B1-B10}{First 10 moves made by black}
    \item{last.move}{Last move made in the game}
    \item{W_B_moves/ B_B_moves}{Number of Bishop moves by the corresponding side}
    \item{W_K_moves/ B_K_moves}{Number of King moves by the corresponding side}
    \item{W_N_moves/ B_K_moves}{Number of Knight moves by the corresponding side}
    \item{W_O_moves/ B_O_moves}{Number of pawn moves by the corresponding side}
    \item{W_Q_moves/ B_Q_moves}{Number of Queen moves by the corresponding side}
    \item{W_R_moves/ B_R_moves}{Number of Rook moves by the corresponding side}
    \item{B_moves}{Number of total Bishop moves in the game}
    \item{K_moves}{Number of total King mves in the game}
    \item{N_moves}{Number of total Kinght moves in the game}
    \item{O_moves}{Number of total Pawn moves in the game}
    \item{Q_moves}{Number of total Queen moves in the game}
    \item{R_moves}{Number of total Rook moves in the game}
    \item{movetext}{Full list of moves made in the game as a string}

}
\references{
\code{\link{https://www.chess.com/news/view/published-data-api}
}
\author{Matthew Helke <matthewhelke52@gmail.com>}
\note{
Terms:

FEN: (Forsyth-Edwards Notation): Used to describe a particular position on a chess board. This can be used to view a position at any given time throughout a chess game. In this case, the ending position.

ECO Code: Encyclopedia of Chess Openings code. Each opening in chess is given a particular code which can be referenced at anytime here \code{\link{https://www.365chess.com/eco.php}}

Algebraic Chess Notation: A way of recording a chess game. Each piece is given a letter, and the  board is numbered algebraically. This is how moves are recorded during a game and it allows a player to play back the game later on.
}

\examples{
    d <- chessdata("MasterMatthew52", "2020", "02")

}

\keyword{chess}
