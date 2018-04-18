#' @title Execute a Workflow
#' @name Execute
#' @description executes a workflow
#' @param workflow implementation instance of \code{WorkflowInterface}
#' @param moduleArgs a list of lists where the name of each element corresponds
#'                   to a module name in \code{workflow} and the elements are
#'                   argument lists to be used for arguments not fulfilled by
#'                   workflow connections to execute a module. Note, values
#'                   passed with an actual connection in the workflow will
#'                   override anything specified in this list.
#' @param argsContainer a list or environment of named values to use whenever
#'                      the name of a value in the container interects with
#'                      an input argument of a module in \code{workflow}. Note,
#'                      values passed with an actual connection in
#'                      \code{workflow} or specified by \code{moduleArgs}
#'                      override anything specified by this container.
#' @param mode Defines execution mode, e.x. running modules in parallel. All
#'             defined modes are in the constants of this package.
#' @param clearCache boolean flag that indicated if will clear out workflow
#'                   module output caches.
#' @param moduleInputsSaveDir Name of directory to save module inputs to.
#'                            Inputs are saved in .rds format in form of
#'                            'module_name' + '_inputs.rds'. If \code{NULL}
#'                            , the default, inputs are not saved. Please note,
#'                            this feature is intended for debugging purposes
#'                            -- will slow execution down and take up disk
#'                            space.
#' @export 
Execute <- function(workflow
                    , moduleArgs = NULL
                    , argsContainer = NULL
                    , mode = SERIAL_MODE
                    , clearCache = TRUE
                    , moduleInputsSaveDir = NULL
) {
    if(!is.Workflow(workflow)) {
        UpDraftSettings$errorLogger("workflow parameter is not a valid workflow obj")
    }
    
    UpDraftSettings$infoLogger("*UpDraft* Error Checking Workflow")
    # TODO: figure out how to error check with moduleArgs
    workflow$errorCheck(executionCheck = TRUE, names(argsContainer))
    
    SetExecutionSettings(mode, UpDraftSettings$moduleMaxMemorySize)
    
    modulesToExecute <- workflow$getStartingModules() # queue of modules to check if dependecies have been met - once met, start the module execution
    modulesExecutionStarted <- list()
    monitorRate <- UpDraftSettings$monitorRate # cache this out of settings to avoid lookup costs
    UpDraftSettings$infoLogger("*UpDraft* Starting Workflow Execution!!!!!*")
    while (length(modulesToExecute) > 0) {
        # TODO:: no flag based wakeup mechanism in R that is concurrency proof
        # stuck using sleep to throttle this loop
        Sys.sleep(monitorRate)
        
        # Doing index based looping to make sure modulesToExecute does not shrink until after loop completes
        for (moduleIndex in 1:length(modulesToExecute)) {
            module <- modulesToExecute[[moduleIndex]]
            upstreamModules <- workflow$getUpstreamModules(module)

            areDependenciesMet <- TRUE
            for (upstreamModule in upstreamModules) {
                if (!upstreamModule$hasCompleted()) {
                    areDependenciesMet <- FALSE
                    break
                }
            }

            if (areDependenciesMet) {
                modulesToExecute[[moduleIndex]] <- NaN
                
                # Start execution
                inputArguments <- list()
                for(upstreamModule in upstreamModules) {
                    # TODO: The following double for-loop is not be necessary if require one connection per argument
                    for (connection in workflow$getConnections(upstreamModule, module)) {
                       for (argument in connection$getInputArgument()) {
                           if (argument != '') {
                               UpDraftSettings$infoLogger(paste0("*UpDraft* Retrieving ", upstreamModule$getName(), " for ", module$getName()))
                               
                               inputArguments[[argument]] <- connection$filterOutputValue(upstreamModule$getOutput())
                           }
                       }          
                    }
                }
                UpDraftSettings$infoLogger(paste0("*UpDraft* Starting ", module$getName()))
                combinedArgsList <- CombineArgLists(inputArguments, moduleArgs[[module$getName()]])
                combinedArgsList <- CombineArgLists(combinedArgsList, ExtractArgsFromContainer(names(module$getInputs()), argsContainer))
                # Debug Feature - Dump Inputs to Disk
                if(!is.null(moduleInputsSaveDir)) {
                    if(!dir.exists(moduleInputsSaveDir)) {
                        UpDraftSettings$infoLogger(sprintf("*UpDraft* Creating the directory '%s' to save module inputs and outputs to", moduleInputsSaveDir))
                        dir.create(moduleInputsSaveDir)
                    }
                    saveRDS(combinedArgsList
                            , file.path(moduleInputsSaveDir
                                        , paste0(module$getName()
                                                 , '_inputs'
                                                 , '.rds')))
                }
                module$startExecution(combinedArgsList)
                modulesExecutionStarted[[module$getName()]] <- module 
                
                # Move downstreams modules to modulesToExecute
                downstreamModules <- workflow$getDownstreamModules(module)
                for (downstreamModule in downstreamModules) {
                    if(is.null(modulesExecutionStarted[[downstreamModule$getName()]])) {
                        modulesToExecute[[downstreamModule$getName()]] <- downstreamModule
                    }
                }
            }
            
            #TODO: clear cached output in modules when not needed anymore
        }
        modulesToExecute <- modulesToExecute[as.logical(lapply(modulesToExecute, is.Module))] # clears out NaNs when modules moved from modulesToExecute to modulesToExecute
    }
    
    #TODO: Temp way to check ending modules complete before exiting
    endingModules <- workflow$getEndingModules()
    while (length(endingModules) > 0) {
        Sys.sleep(monitorRate)

        for (moduleIndex in 1:length(endingModules)) {
            endingModule <- endingModules[[moduleIndex]]
            
            if(endingModule$hasCompleted()) {
                UpDraftSettings$infoLogger(paste0("*UpDraft* ", endingModule$getName(), " has completed"))
                endingModule$getOutput()
                endingModules[[moduleIndex]] <- NaN
            }
        }
        
        endingModules <- endingModules[as.logical(lapply(endingModules, is.Module))]
    }
    
    #TODO: Temp solution to clear out output caches
    if(clearCache) {
        for (module in workflow$getAllModules()) {
            module$clearOutputCache()
        }
    }
}
