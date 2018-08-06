#' @title Autowire Directed Unpack Connections
#' @name Autowire
#' @description Creates directed unpack connections automatically between
#'              \code{headModules} and \code{tailModules}. For this to work,
#'              the associated function object of a head module must have a
#'              single extractable return statement that returns a named list.
#'              When this returned named list has a name collision with an input
#'              argument name of a tail module, a
#'              \code{DirectedUnpackConnection} is created between the
#'              head module and the tail Module. An error is thrown by this
#'              function when the associated function obj of a head module
#'              is not set up for autowiring. This specifically happens when
#'              the associated function obj has 0 or multiple extractable
#'              return statements in the body of the associated function
#'              object, or the return statement does not return a named list.
#'              Note, the absence of an error message does not imply
#'              connections were created -- just that the \code{headModules}
#'              return statements are compatible with this autowiring function.
#' @param headModules a module object, list of module objects, or character vector of names of modules.
#'                    If multiple module objects are provided and several of them output variables with the same name
#'                    Output of earlier modules in the list will be ignored when constructing a connection.
#' @param tailModules a module object, list of module objects, or character vector of names of modules
#' @param connectionNamePrefix a character string to use as a prefix in the
#'                             name generation of connections created with 
#'                             this function. Default is \code{NULL}, in which case
#'                             The head and tail module names are concatenated
#'                             to be used as the prefix.
#' @param exclusions a character vector of named outputs from
#'                   \code{headModules} to exclude from autowiring.
#' @return list of created connections 
#' @export
Autowire <- function(headModules
                     , tailModules
                     , connectionNamePrefix = NULL
                     , exclusions = c("")
) {
    if (!is.list(headModules)) {
        headModules <- list(headModules)
    }
    if(!is.list(tailModules)) {
        tailModules <- list(tailModules)
    }

    connections <- list()
    connectionIndex <- 1
    # reverse headModules in the loop to ensure that
    # if same variables appear in multiple head modules,
    # only use the variable from the output of the latest one
    for(headModule in rev(headModules)) {
        func <- headModule$getFuncObj()
        returnStatements <- ExtractReturnStatements(func)
        tryCatch({
            if (length(returnStatements) == 0) { stop('no explicit return found') }
            isListReturn <- all(vapply(X = returnStatements
                                       , FUN = function(rts) is.call(rts) && as.character(rts[[1]]) == 'list'
                                       , FUN.VALUE = logical(1)))
            if (!isListReturn) { stop('non-list return found') }
            returnNamesList <- lapply(returnStatements, function(rts) names(rts)[-1])
            returnNames <- returnNamesList[[1]]
            if (length(returnStatements) > 1) {
                isSameReturn <- all(vapply(X = returnNamesList[-1]
                                           , FUN = function(rtn) setequal(returnNames, rtn)
                                           , FUN.VALUE = logical(1)))
                if (!isSameReturn) { stop('heterogeneous returns found') }
            }
        }
        , error = function(err) {
            if(err$message == 'no explicit return found') {
                returnNames <- character(0)
            } else {
                UpDraftSettings$errorLogger("Failed to autowire unpack connections with ", headModule$getName(), " as a head module.")
            }
        })
        namedModuleOutputs <- setdiff(returnNames, exclusions)

        if(length(namedModuleOutputs) == 0) {
            UpDraftSettings$errorLogger("Failed to autowire unpack connections with ", headModule$getName(), " as a head module because it has no named return values.")
        }

        # update exclusions so that variables already output are excluded
        exclusions <- c(exclusions, namedModuleOutputs)

        for(tailModule in tailModules) {
            inputNames <- names(formals(tailModule$getFuncObj()))
            inputsToWire <- intersect(namedModuleOutputs, inputNames)
            if (is.null(connectionNamePrefix)) {
                # reset connectionIndex for this new headModule tailModule combination
                connectionIndex <- 1
                connName <- paste0(headModule$getName(), '_', tailModule$getName())
            } else {
                connName <- connectionNamePrefix
            }

            for(input in inputsToWire) {
                names(input) <- input 
                connection <- DirectedUnpackConnection$new(name = paste0(connName, '_', connectionIndex)
                                                           , headModule = headModule
                                                           , tailModule = tailModule
                                                           , inputArgument = input)
                connectionIndex <- connectionIndex + 1
                connections[[connection$getName()]] <- connection
            }
        }
    }

    return(connections)
}

#' @title Extract Return Statements of a Function
#' @name ExtractReturnStatements
#' @description returns all return calls associated with \code{func}
#' @param func R function object
#' @return a list of return calls
ExtractReturnStatements <- function(func) {
    if(is.function(func)) {
        func <- body(func)
    }
    
    if(is.call(func) && identical(func[[1]], quote(`return`))) {
        return(list(func[[2]]))
    } else if (is.recursive(func)) {
        return(unique(do.call(c, lapply(func, ExtractReturnStatements))))
    }
    
    return(invisible(NULL))
}

#' @title Create Directed Connections
#' @name CreateDirectedConnections
#' @description creates a list of directed connections of \code{connectionType}
#'              type from all \code{headModules} to all \code{tailModules}.
#' @param headModules a list of module objs or names of modules
#' @param tailModules a list of modules objs or names of modules
#' @param connectionNamePrefix a character string to use as a prefix in the
#'                             name generation of connections created with 
#'                             this function. Default is \code{NULL}, in which case
#'                             The head and tail module names are concatenated
#'                             to be used as the prefix.
#' @param inputArgument Name of the argument
#'                      the output of a head module is used for a tail
#'                      module's execution.
#' @param connectionType R6Class that is of \code{DirectedConnection} type,
#'                       any classes that inherit from
#'                       \code{DirectedConnection} will work.
#' @return list of directed connections created
#' @export
CreateDirectedConnections <- function( headModules
                                       , tailModules
                                       , inputArgument
                                       , connectionNamePrefix = NULL
                                       , connectionType = DirectedConnection
) {
    #TODO: Add check to made sure connectionType is some type of DirectedConnection
    if (!is.list(headModules)) {
        headModules <- list(headModules)
    }
    if(!is.list(tailModules)) {
        tailModules <- list(tailModules)
    }

    connections <- list()

    connectionIndex <- 1
    for(headModule in headModules) {
        for(tailModule in tailModules) {
            if (is.null(connectionNamePrefix)) {
                # reset connectionIndex for this new headModule tailModule combination
                connectionIndex <- 1
                connName <- paste0(headModule$getName(), '_', tailModule$getName())
            } else {
                connName <- connectionNamePrefix
            }
            conn <- connectionType$new(name = paste0(connName, '_', connectionIndex)
                                       , headModule = headModule
                                       , tailModule = tailModule
                                       , inputArgument = inputArgument)
            connections[[conn$getName()]] <- conn
            connectionIndex <- connectionIndex + 1
        }
    }
    
    return(connections)
}
