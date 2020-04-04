\name{chessdata}
\alias{chessdata}
\title{Gathers Chess.com Archive Data}
\description{Takes in a chess.com username, year (YYYY), and month (MM) as three strings. The chess.com game archive API will be downloaded and a table will be produced with information regarding the user's games.}
\usage{chessdata(username, year, month)}
\arguments{
  \item{username}{chess.com username string}
  \item{year}{year as a string (YYYY format)}
  \item{month}{month as a string (MM format)}
}
\details{
}
\value{
    Dataframe with the following variables:

    url: Link to the game that was played on chess.com - String

    time_control: time control of the game that was played - String

    fen: final position of the game - String

    time_class: what kind of time control class was used? Options include: Bullet, Blitz, Rapid, or Daily - String

    Date: What date the game was played - String

    White: Who played the white pieces - String

    Black: who played the black pieces - String

    Result: What was the result of the game. Options include: "1-0"", "1/2-1/2"", "0-1""- String

    ECO: ECO Code for the opening that was played - String

    NMoves: Numbr of moves in the game - Numeric

    W1-10: White's moves in order from 1 to 10 using algebraic chess notation- String

    B1-10: Black's moves in order from 1 to 10 using algebraic chess notation- String

    last.move: Final move in the game using algebraic chess notation - String

    W_B_Moves: Number of white bishop moves - Numeric

    W_K_Moves: Number of white king moves - Numeric

    W_N_Moves: Number of white knight moves - Numeric

    W_O_Moves: Number of white pawn moves - Numeric

    W_Q_Moves: Number of white queen moves - Numeric

    W_R_Moves: Number of white rook moves - Numeric

    B_B_Moves: Number of black bishop moves - Numeric

    B_K_Moves: Number of black king moves - Numeric

    B_N_Moves: Number of black knight moves - Numeric

    B_O_Moves: Number of black pawn moves - Numeric

    B_Q_Moves: Number of black queen moves - Numeric

    B_R_Moves: Number of black rook moves - Numeric

    B_Moves: Number of total bishop moves - Numeric

    K_Moves: Number of total king moves - Numeric

    N_Moves: Number of total knight moves - Numeric

    O_Moves: Number of total pawn moves - Numeric

    Q_Moves: Number of total queen moves - Numeric

    R_Moves: Number of total rook moves - Numeric

    winner: Username of the play that won the game - String

    ending: How the game ended. - String

    white_rating: Rating of player playing white - String

    black_rating: rating of player playing black - String
}
\references{
\code{\link{https://www.chess.com/news/view/published-data-api}
}
\author{

    Matthew Helke <matthewhelke52@gmail.com>
}
\note{
Terms:

FEN    (Forsyth-Edwards Notation): Used to describe a particular position on a chess board. This can be used to view a position at any given time throughout a chess game. In this case, the ending position.

ECO Code:    Encyclopedia of Chess Openings code. Each opening in chess is given a particular code which can be referenced at anytime here \code{\link{https://www.365chess.com/eco.php}}

Algebraic Chess Notation:    A way of recording a chess game. Each piece is given a letter, and the  board is numbered algebraically. This is how moves are recorded during a game and it allows a player to play back the game later on.
}

\examples{


    d <- chessdata("MasterMatthew52", "2020", "02")


}


\keyword{ ~chess }
