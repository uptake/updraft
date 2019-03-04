#' @title Get Function Arguments
#' @name GetFunctionArgs
#' @description gets a named logical vector where the names are the arguments
#'              of the function \code{fun} and the values indicate if the
#'              associated argument is required to run \code{fun}.
#' @param func function
#' @return logical vector
GetFunctionArgs <- function(func) {
    argumentRequirements <- logical()

    funArguments <- as.list(args(func))
    for (arg in names(funArguments)) {
        if (arg == "") {
            next
        }

        if (arg == '...' || funArguments[arg] != "") {
            isRequired = FALSE
        } else {
            isRequired = TRUE
        }

        argumentRequirements[arg] <- isRequired
    }

    return(argumentRequirements)
}

#' @title Future Function Call
#' @name FutureFunctionCall
#' @description Creates a future for do.call of \code{func} with \code{args}.
#'              Wraps do.call so that, if an error occurs, the stack trace
#'              and error message get logged appriopriately.
#' @importFrom future future multiprocess plan tweak
#' @param func a R function object
#' @param args arguments to be used when calling \code{func}
#' @param funcName the name of the function being passed to this call, in case an error occurs it will log out the stack trace separately
#' @param assignedProcesses number of processes to assign to the execution of
#'                          this module. If the underlying execution code
#'                          leverages the package \code{future}, this will be
#'                          enforced by setting a future plan on behalf of the
#'                          module execution code.
#' @return created future.
FutureFunctionCall <- function(func
                               , args
                               , funcName = NULL
                               , assignedProcesses = 1
) {
    return(future::future({
        if(assignedProcesses > 1) {
            # TODO: temp set to multisession because packages uses UpDraft use multisession for parallelization
            modifiedFuturesPlan <- future::tweak(future::multisession, workers = assignedProcesses) # intermediate required to set new future plan
            future::plan(modifiedFuturesPlan)
        }

        startTime = as.numeric(Sys.time())
        tryCatch(
            {withCallingHandlers(
                {do.call(func, args)}
                , error = function(err) {
                    updraft:::LogStackTrace(moduleName = funcName, errorMessage = err, fileName = funcName)
                    UpDraftSettings$errorLogger(err)
                }
            )}
            , finally = {
                if(!is.null(funcName)) {
                    UpDraftSettings$infoLogger(sprintf('*UpDraft* %s took %g seconds to complete', funcName, as.numeric(Sys.time()) - startTime))
                }
            }
        )

    }))
}

#' @title Log Stack Trace
#' @name LogStrackTrace
#' @description Generates a stack trace of function calls and calls
#'              info logger function in UpDraftSettings with stack
#'              trace.
#' @param moduleName the name of the module that triggered the stack trace
#' @param errorMessage the error message of the the error that triggered the stack trace
#' @param fileName the name of the file to output the StackTrace too. If not specified will default to logger. Captured via sink
#' @param charLimit character limit to limit logger statements during stack
#'                  trace.
#' @param objectLimit the size in bytes that an object will be replaced with its class name when printing out the stack. Default is 10kb
#' @importFrom utils capture.output dump.frames
#' @return \code{NULL}.
LogStackTrace <- function(  moduleName
                          , errorMessage
                          , fileName = NULL
                          , charLimit = 256
                          , objectLimit = 1e5  #bytes
                          ) {
    calls <- sys.calls()
    returnMessage <- utils::capture.output({
        UpDraftSettings$infoLogger(sprintf('--------- *UpDraft* Start of Stack Trace from: %s ---------',moduleName))
        for(call in calls){
            tryCatch({
                # Log out the traceback, pruning where necessary
                argSizes <- vapply(call, FUN = utils::object.size,FUN.VALUE = vector(mode = 'numeric',length=1L)) > objectLimit
                if (any(argSizes)){
                    call[argSizes] <- vapply(call[argSizes],FUN=function(x){paste('Class of:',paste(class(x),collapse = ','))},FUN.VALUE = vector(mode='character',length=1L))
                }
                functionCall <- deparse(call)

                functionCall <- substr(functionCall,1,charLimit)
                UpDraftSettings$infoLogger(functionCall)
            } , error = function(err) {
                UpDraftSettings$infoLogger('Error while parsing stack trace: ', err)
            })
        }
        UpDraftSettings$infoLogger(sprintf('--------- *UpDraft* End of Stack Trace from %s ---------',moduleName))
        UpDraftSettings$infoLogger(paste0("[FATAL ERROR]: ",errorMessage))
    })

    if(is.null(fileName)){
        lapply(returnMessage,UpDraftSettings$infoLogger)
    }else{
        writeLines(text = returnMessage,con = paste0(fileName,".error"))
        utils::dump.frames(dumpto = fileName,to.file = TRUE)
    }
    return(invisible(NULL))
}
