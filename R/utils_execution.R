#' @title Combine Argument Lists
#' @name CombineArgLists
#' @description combines arguments lists \code{args1} and \code{arg2} into one
#'              list. If there is a name collision between \code{args1} and
#'              \code{arg2}, the \code{arg1} value will be in the returned
#'              list.
#' @param args1 named list of function arguments. Has higher precedence over
#'              \code{args2} in the combined list.
#' @param args2 named list of function arguments.
#' @return list
CombineArgLists <- function(args1
                            , args2
) {
    combinedArgs <- args1
    
    for (argName in names(args2)) {
        if(is.null(combinedArgs[[argName]])) {
            combinedArgs[[argName]] <- args2[[argName]]
        }
    }
    
    return(combinedArgs)
}

#' @title Extract Args From Container
#' @name ExtractArgsFromContainer
#' @description extracts values out of \code{container} where the names of
#'              the values intersect with \code{argNames}.
#' @param argNames a character vector of argument names.
#' @param container either a list or environment that contains named values.
#' @return list of named values from \code{container} where \code{argNames}
#'         intersects with \code{container}
ExtractArgsFromContainer <- function(argNames
                                     , container
) {
    commonElements <- intersect(argNames, names(container))
    return(as.list(container)[commonElements])
}

#' @title Set Execution Mode
#' @name SetExecutionMode
#' @importFrom future multicore plan sequential 
#' @description Sets the execution settings of a workffow. Note, this is done
#'              by ajusting settings of the \code{future} package. This might
#'              affect calling programs. See package documentation/vignettes
#'              for info.
#' @param mode Execution mode. Must use a mode that is defined in UpDraft's
#'             constants.
#' @param moduleMaxMemorySize Max memory a module can occupy without causing
#'                            a runtime error. Value is assumed to be in
#'                            Bytes.
#' @return \code{NULL}
SetExecutionSettings <- function(mode
                                 , moduleMaxMemorySize
) {
    options(future.globals.maxSize = moduleMaxMemorySize)

    if(mode == SERIAL_MODE) {
        future::plan(future::sequential)
    } else if(mode == PARALLEL_MODE) {
        future::plan(future::multicore)
    } else {
        UpDraftSettings$errorLogger("*UpDraft* selected execution mode is not valid")
    }
    
    return(NULL)
}
