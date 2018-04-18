#' @title Connection Interface
#' @name ConnectionInterface
#' @description Defines a contract of implementation for any connection based class, i.e. a class that extends this
#'              interface. This class cannot be instantiated directly as it is abstract. 
#' @importFrom R6 R6Class
#' @section Contracted Methods:
#' \describe{
#'    \item{\code{filterOutputValue(outputValue)}}{filters the \code{outputValue}, value outputted from the source module of a conntion, to what is expected by the destination module.}
#'    \item{\code{getInputArgument()}}{Gets the argument the output of one module should be used as input into the other module's execution.}
#' }
#' @export
ConnectionInterface <- R6::R6Class( "ConnectionInterface"
    , inherit = UpDraftComponentInterface
    , public = list(
        filterOutputValue = function(outputValue) {
            UpDraftSettings$errorLogger("filterOutputValue not implemented in", class(self)[1]) 
        }
        
        , getInputArgument = function() {
            UpDraftSettings$errorLogger("getInputArgument not implemented in", class(self)[1])
        }
    )
)

#' @title is.Connection
#' @name is.Connection
#' @description Returns a boolean that indicates if \code{connection} is a 
#'              valid connection obj
#' @param connection possible connection obj
#' @return boolean where \code{TRUE} indicates \code{connection} is a valid 
#'         connection object
#' @importFrom R6 is.R6
#' @export
is.Connection <- function(connection) {
    if(R6::is.R6(connection) && "ConnectionInterface" %in% class(connection)) {
        return(TRUE)
    }
    
    return(FALSE)
}