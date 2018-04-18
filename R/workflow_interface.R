#' @title Workflow Interface
#' @name WorkflowInterface
#' @importFrom R6 R6Class
#' @description Defines a contract of implementation for any workflow based class, i.e. a class that
#'              extends this interface. This class cannot be instantiated directly as it is abstract. 
#' @section Contracted Methods:
#' \describe{
#'      \item{\code{addConnections(connections)}}{Adds \code{connections}, a list of implementation instances of \code{ConnectionInterface}, to this workflow.}
#'      \item{\code{addModules(modules)}}{Adds \code{modules}, a list of implementation instances of \code{ModuleInterface}, to this workflow.}
#'      \item{\code{getWorkflowInputs()}}{Gets a named logical vector where names are the possible inputs into the workflow and the values indicate if an input is required.}
#'      \item{\code{getModuleInputs(module)}}{Gets a named logical vector where the names are the inputs, excluding those provided by connections from upstream modules, into a module and the values indicate if an input is required.}
#'      \item{\code{getConnections(module1, module2)}}{Gets the connections between \code{module1} and \code{module2}.}
#'      \item{\code{getEndingModules()}}{Gets a list of modules that are the ending modules of a workflow.}
#'      \item{\code{getAllModules()}}{Gets a list of all modules in a workflow.}
#'      \item{\code{getStartingModules()}}{Gets a list of modules that are the starting modules of a workflow.}
#'      \item{\code{getDownstreamModules(module)}}{Gets modules downstream of \code{module} in a workflow.}
#'      \item{\code{getUpstreamModules(module)}}{Gets modules upstream of \code{module} in a workflow.}
#'      \item{\code{initFromFile(filename)}}{Initializes this workflow from a save state stored in \code{filename}.}
#'      \item{\code{removeConnection(connection)}}{Removes \code{connection}, an implementation instance of \code{ConnectionInterface}, to this workflow.}
#'      \item{\code{removeModule(module)}}{Adds \code{module}, an implementation instance of \code{ModuleInterface}, to this workflow.}
#'      \item{\code{save(filename)}}{Saves the state of this workflow into a file named \code{filename} so that future workflows can be initialized to this workflow.}
#'      \item{\code{visualize()}}{Visualizes this workflow.}
#' }
#' @export
WorkflowInterface <- R6::R6Class( "WorkflowInterface"
    , inherit = UpDraftComponentInterface
    
    , public = list(
        addConnections = function(connections) {
            UpDraftSettings$errorLogger("addConnection not implemented in ", class(self)[1])
        }
        
        , addModules = function(modules) {
            UpDraftSettings$errorLogger("addModule not implemented in ", class(self)[1])
        }

        , getWorkflowInputs = function() {
            UpDraftSettings$errorLogger("getWorkflowInputs not implemented in ", class(self)[1])
        }

        , getModuleInputs = function(module) {
            UpDraftSettings$errorLogger("getModuleInputs not implemented in ", class(self)[1])
        }
        
        , getConnections = function(module1, module2) {
            UpDraftSettings$errorLogger("getConnections not implemented in ", class(self)[1])    
        }
        
        , getEndingModules = function() {
            UpDraftSettings$errorLogger("getEndingModules not implemented in ", class(self)[1])
        }
        
        , getAllModules = function() {
            UpDraftSettings$errorLogger("getModules not implemented in ", class(self)[1])
        }

        , getStartingModules = function() {
            UpDraftSettings$errorLogger("getStartingModules not implemented in ", class(self)[1])
        }
        
        , getDownstreamModules = function(module) {
            UpDraftSettings$errorLogger("getDownstreamModules not implemented in ", class(self)[1])
        }

        , getUpstreamModules = function(module) {
            UpDraftSettings$errorLogger("getUpstreamModules not implemented in ", class(self)[1])    
        }
        
        , removeConnection = function(connection) {
            UpDraftSettings$errorLogger("removeConnection not implemented in ", class(self)[1])
        }
        
        , removeModule = function(module)  {
            UpDraftSettings$errorLogger("removeModule not implemented in ", class(self)[1])
        }
        
        , save = function(filename) {
            UpDraftSettings$errorLogger("save not implemented in ", class(self)[1])
        }
        
        , visualize = function() {
            UpDraftSettings$errorLogger("visualize not implemented in ", class(self)[1])
        }
    )
)

#' @title WorkflowInterface Init From File
#' @name WorkflowInterface$initFromFile
#' @importFrom jsonlite fromJSON
#' @importFrom utils getFromNamespace
#' @description Static factory method that generates a workflow object from
#'              json file.
#' @param filename json file of saved workflow from \code{save} method.
#' @return initialized \code{PackageFunctionModule}
WorkflowInterface$initFromFile <- function(filename) {
    tryCatch({
        workflowClass <- jsonlite::fromJSON(filename)$class
    }
    , error = function(err) {
        UpDraftSettings$errorLogger("Could not extract a workflow class from ", filename, " - cannot dispatch to class's initFromFile static method")
    })
    
    return(utils::getFromNamespace(workflowClass, "updraft")$initFromFile(filename))
}

# TODO: Unit Tests?
#' @title is Workflow
#' @name is.Workflow
#' @description Returns a boolean indicating if \code{obj} is a valid
#'              implementation instance of \code{Workflow}.
#' @importFrom R6 is.R6
#' @param obj object instance
#' @return boolean where \code{TRUE} indicates \code{obj} is a valid
#'         implementation instance of \code{WorkflowInterface}
#'         and \code{FALSE} otherwise.
#' @export
is.Workflow <- function(obj) {
    if ("WorkflowInterface" %in% class(obj) && R6::is.R6(obj)) {
        return(TRUE)
    }
    
    return(FALSE)
}
