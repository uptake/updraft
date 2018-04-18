DEFAULTS <- list(
    ### Execution
    moduleMaxMemorySize =  128 * 2^30 # 128 GB
    , monitorRate = 1.0 # in seconds
    
    ### Logger Statements
    , errorLogger = stop
    , infoLogger = print
    , warningLogger = warning
    
    ### Workflow Visualizations
    , moduleBorderColor = '#E87600'
    , moduleFillColor = 'white'
    , singleConnectionColor = '#9AA5AF'
    , multipleConnectionsColor = '#212721'
)

#' @title Serial Mode Constant
#' @name SERIAL_MODE
#' @description value to set \code{mode} in \code{Execution} to run in serial mode
#' @export 
SERIAL_MODE <- 0 # module execution happens sequentially

#' @title Parallel Mode Constant
#' @name PARALLEL_MODE
#' @description value to set \code{mode} in \code{Execution} to run in parallel mode
#' @export 
PARALLEL_MODE <- 1 # module execution happens in parallel
