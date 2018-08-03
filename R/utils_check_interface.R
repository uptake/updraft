#' @title CheckInterfaceImplementation
#' @name CheckInterfaceImplementation
#' @description Checks to see if \code{R6Class} has fully implemented
#'              an interface contract.
#' @param R6Class an R6 class in this library or a character string of the class
#'                name in this library. 
#' @return NULL if successful. Raises a fatal error if finds a missing method
#'         expected in \code{R6Class}.
CheckInterfaceImplementation <- function(R6Class) {
    if (is.character(R6Class)) {
        R6Class <- get(R6Class)
    }
        
    if (!R6::is.R6Class(R6Class)) {
        UpDraftSettings$errorLogger("R6Class parameter must be a valid R6 class")
    }
    
    className <- R6Class$name
    classMethods <- character()
    requiredInterfaceMethods <- character()
    while (!is.null(className)) {
        classPointer <- get(as.character(className))
        
        if (grepl("interface", classPointer$classname, ignore.case = TRUE)) {
            requiredInterfaceMethods <- unique(c(requiredInterfaceMethods, names(classPointer$public_methods)))
        } else {
            classMethods <- unique(c(classMethods, names(classPointer$public_methods)))
        }
        
        className <- classPointer$inherit
    }
    
    methodDifferences <- setdiff(requiredInterfaceMethods, classMethods)
    if (length(methodDifferences) > 0) {
        UpDraftSettings$errorLogger(R6Class$classname, ' must implement ', methodDifferences, " method")
    }
    
    return(invisible(NULL))
}