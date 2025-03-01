#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Simulate contestant's inital door choice.
#'
#' @description
#' `select_door()` uses sample to select a number between 1-3, 
#' and that number corresponds to one of the three doors.
#' 
#' @details
#' This is the frist step in the game where the contestant chooses one 
#' of three doors, where they have a 1 in 3 chance of choosing the door with the car. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns an integer between 1 and 3.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Simulate host's selection of a door with at goat behind it.
#'
#' @description
#' `open_goat_door()` uses the functions if and sample to have 
#' the host open a door with a goat behind it.
#'
#' @details
#' This function performs the second step in the game where the host 
#' reveals a goat door, leaving the contestant with their choice and one 
#' other door. One of the remaining two doors has a car and the other has a goat. 
#'
#' @param "game" is a character vector.
#'
#' @param "a.pick" is a numeric integer.
#'
#' @return The function returns a numeric vector.
#'
#' @examples
#' open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Simulate contestant staying with original door 
#' or switching doors after goat door is revealed.
#'
#' @description
#' `change_door()` uses the logical statement stay=T and if funtion to determine which
#' door will be the final door depending if the contestant chooses to stay or switch.
#'
#' @details
#' This function performs the third step in the game where the contestant chooses 
#' to stay with their original door or switch doors, now that a goat door has been revealed. 
#'
#' @param stay=T is a logical value meaning stay equals true.
#'
#' @param "opened.door" is a numeric integer.
#'
#' @param "a.pick" is a numeric integer.
#'
#' @return This function returns a numeric vector. 
#'
#' @examples
#' change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Simulate the revealing of contestants final door pick 
#' and determine if they win or lose.
#'
#' @description
#' `determine_winner` is uses if functions to determine if 
#' the door the contestant chooses as their final pick is a 
#' winning or losing door. 
#'
#' @details
#' This funtion performs the fourth and final step in the game, 
#' where the contestants results are determined using if functions 
#' that show win for car and lose for goat.
#'
#' @param "final.pick" is a numeric integer.
#'
#' @param "game" is a character vector.
#'
#' @return This function returns a character vector.  
#' @examples
#' determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play game function to run the game twice, one time the contestant 
#' stays with their original door and the other they swtich doors.  
#'
#' @description
#' 'play_game()' puts together all of the game elements to determine 
#' if staying with the original door or switching doors is more 
#' likely to end in a win.
#'
#' @details
#' The function imports all parts of the game and returns the results 
#' of staying with the original door and the results of switching doors. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return Data frame with the results of staying with original door and switching doors. 
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Game simulation function.
#'
#' @description
#' 'play_n_games()' performs a Monte Carlo style simulation to play through 
#' the game staying and switching the door many times and then produces a results 
#' table that indicates the proportion of wins and losses for each strategy. 
#'
#' @details
#' The function loops through play_game() a set number of times and returns 
#' data frames of the results and the proportion of wins and losses for each strategy. 
#'
#' @param n is an integer for number of simulations. 
#'
#' @return Data frame and proportion table of results. 
#' 
#' @examples
#' play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
