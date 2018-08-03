#' @title DAG Workflow Class
#' @name DAGWorkflow
#' @description Directed Acyclic Graph (DAG) workflow implementation
#' @importFrom DiagrammeR add_global_graph_attrs create_edge_df create_graph create_node_df generate_dot grViz
#' @importFrom igraph degree E edge graph.empty get.edgelist neighbors subgraph.edges V vertex
#' @importFrom jsonlite toJSON
#' @importFrom R6 R6Class
#' @section Class Constructor:
#' \describe{
#'     \item{\code{new(name = "")}}{
#'         \itemize{
#'             \item{\code{name}: character string that identifies this workflow}
#'         }
#'     }
#' }
#' @section Public:
#' \describe{
#'     \item{\code{addConnections(connections)}}{
#'         \itemize{
#'             \item{Adds a list of connection to a workflow.}
#'             \item{\bold{\code{connection}}: list of implementation \code{ConnectionInterface} object.}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     \item{\code{addModules(modules)}}{
#'         \itemize{
#'             \item{Adds a list of modules to a workflow.}
#'             \item{\bold{\code{modules}}: list of implementation \code{ModuleInterface} objects.}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     \item{\code{errorCheck(executionCheck = FALSE, ...)}}{
#'         \itemize{
#'             \item{Runs error checking on the internal state of a workflow.}
#'             \item{\code{executionCheck}: when set to \code{TRUE}, runs additional checks to determine if ready for execution.}
#'             \item{\code{...}: character vectors of additional argument names from sources outside of connections, such as an enviroment.}
#'             \item{\bold{Returns}: \code{NULL}, will raise a fatal error if an error is found}
#'         }
#'     }
#'      \item{\code{getWorkflowInputs()}}{
#'         \itemize{
#'             \item{Gets a named logical vector where names are the possible inputs into the workflow and the values indicate if an input is required.}
#'             \item{\bold{Returns}: logical vector}
#'         }
#'     }
#'      \item{\code{getModuleInputs(module)}}{
#'         \itemize{
#'             \item{Gets a named logical vector where the names are the inputs, excluding those provided by connections from upstream modules, into a module and the values indicate if an input is required.}
#'             \item{\bold{\code{module}}: implementation obj of \code{ModuleInterface} or a character string of a module in a workflow.}
#'             \item{\bold{Returns}: logical vector}
#'         }
#'     }
#'     \item{\code{getConnections(module1, module2)}}{
#'         \itemize{
#'             \item{Gets a list of connections between \code{module1} and \code{module2}.}
#'             \item{\bold{\code{module1}}: implementation obj of \code{ModuleInterface} or a character string of a module in a workflow. Because this is a directed implementation class, module1 is the head module.}
#'             \item{\bold{\code{module2}}: implementation obj of \code{ModuleInterface} or a character string of a module in a workflow. Because this is a directed implementation class, module1 is the tail module.}
#'             \item{\bold{Returns}: vector of character strings}
#'         }
#'     }
#'     \item{\code{getDownstreamModules(module)}}{
#'         \itemize{
#'             \item{Gets a list of modules downstream of \code{module}.}
#'             \item{\bold{\code{module}}: valid implementation \code{ModuleInterface} obj or name of module that is present in a workflow.}
#'             \item{\bold{Returns}: list of downstream modules}
#'         }
#'     }
#'     \item{\code{getEndingModules()}}{
#'         \itemize{
#'             \item{Gets a list of ending modules in a workflow.}
#'             \item{\bold{Returns}: a list of ending modules}
#'         }
#'     }
#'     \item{\code{getAllModules()}}{
#'         \itemize{
#'             \item{gets a list of all modules in a workflow.}
#'             \item{\bold{Returns}: a list of all modules}
#'         }
#'     }
#'     \item{\code{getName()}}{
#'         \itemize{
#'             \item{Gets the name of a workflow.}
#'             \item{\bold{Returns}: character string}
#'         }
#'     }
#'     \item{\code{getSaveInfo()}}{
#'         \itemize{
#'             \item{DOES NOT APPLY TO WORKFLOWS.}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     \item{\code{getStartingModules()}}{
#'         \itemize{
#'             \item{Gets a list of starting modules in a workflow.}
#'             \item{\bold{Returns}: a list of starting modules}
#'         }
#'     }
#'     \item{\code{getUpstreamModules(module)}}{
#'         \itemize{
#'             \item{Gets a list of modules upstream of \code{module}.}
#'             \item{\bold{\code{module}}: valid implementation \code{ModuleInterface} obj or name of module that is present in a workflow.}
#'             \item{\bold{Returns}: list of downstream modules}
#'         }
#'     }
#'     \item{\code{removeConnection(connection)}}{
#'         \itemize{
#'             \item{Removes a connection from a workflow.}
#'             \item{\bold{\code{connection}}: implementation object of \code{ConnectionInterface}, or a character vector of connection names}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     \item{\code{removeModule(module)}}{
#'         \itemize{
#'             \item{Removes a modules from a workflow. Also, removes associated connections with \code{module}.}
#'             \item{\bold{\code{module}}: implementation object of \code{ModuleInterface}, or a character vector of module names}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     \item{\code{save(filename)}}{
#'         \itemize{
#'             \item{Saves a workflow onto disk at \code{filename}.}
#'             \item{\bold{\code{filename}}: location to store file}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     \item{\code{visualize()}}{
#'         \itemize{
#'             \item{Visualizes a workflow.}
#'             \item{\bold{Returns}: NULL. Output will be on a plot, etc.}
#'         }
#'     }
#' }
#' @section Private:
#' \describe{
#'     \item{\code{connections}}{
#'         \itemize{
#'             \item{Stores a workflow's connections.}
#'         }
#'     }
#'     \item{\code{graph}}{
#'         \itemize{
#'             \item{Stores a workflow's internal graph representation.}
#'         }
#'     }
#'     \item{\code{modules}}{
#'         \itemize{
#'             \item{Stores a workflow's modules.}
#'         }
#'     }
#'     \item{\code{name}}{
#'         \itemize{
#'             \item{Stores a workflow's name.}
#'         }
#'     }
#'     \item{\code{getNeighbors(graph, module, direction)}}{
#'         \itemize{
#'             \item{Gets a list of the neighbor modules of \code{module} in \code{graph}.}
#'             \item{\bold{\code{graph}}: valid igraph object.}
#'             \item{\bold{\code{module}}: module obj or module name.}
#'             \item{\bold{\code{direction}}}: "in" for upstream neighbors or "out" for downstream neighbors.
#'             \item{\bold{Returns}: a list of neighbor modules.}
#'         }
#'     }
#'     \item{\code{getRevertState()}}{
#'         \itemize{
#'             \item{Gets a list of data that can be used with the \code{revert} method to revert a workflow to the state outputted from this method.}
#'             \item{\bold{Returns}: a list that contains revert state data.}
#'         }
#'     }
#'     \item{\code{revert(previousState)}}{
#'         \itemize{
#'             \item{Returns a workflow to the state stored in \code{previousState}.}
#'             \item{\bold{Returns}: \code{NULL}.}
#'         }
#'     }
#' }
#' @section Static Class Methods:
#' \describe{
#'     \item{\link[updraft]{DAGWorkflow-cash-initFromFile}}{}
#' }
#' @export
DAGWorkflow <- R6::R6Class("DAGWorkflow"
    , inherit = WorkflowInterface
    
    , public = list(
        initialize = function(name = "") {
            private$name <- name
            private$modules <- list()
            private$connections <- list()
            private$graph <- igraph::graph.empty()
            
            self$errorCheck()
        }
        
        , addConnections = function(connections) {
            if (is.DirectedConnection(connections)) {
                connections <- list(connections)
            }
            
            for (connection in connections) {
                if (!is.DirectedConnection(connection)) {
                    UpDraftSettings$errorLogger("connection parameter is not a valid DirectedConnection obj")
                }
                
                revertState <- private$getRevertState()
                
                tryCatch({
                    private$connections[[connection$getName()]] <- connection
                    private$graph <- suppressWarnings(private$graph 
                                                      + igraph::edge(connection$getHeadModuleName()
                                                                     , connection$getTailModuleName()
                                                                     , name = connection$getName()))
                    self$errorCheck()
                }
                , error = function(err) {
                    private$revert(revertState)
                    UpDraftSettings$errorLogger(err)
                })    
            }
            
            return(invisible(NULL))
        }
        
        , addModules = function(modules) {
            if (is.Module(modules)) {
                modules <- list(modules)
            }
    
            for (module in modules) {
                if (!is.Module(module)) {
                    UpDraftSettings$errorLogger("modules parameter must contain valid implementation objects of ModuleInterface")
                }
                
                revertState <- private$getRevertState()
                
                tryCatch({
                    private$modules[[module$getName()]] <- module
                    private$graph <- private$graph + igraph::vertex(module$getName())
                    self$errorCheck()
                }
                , error = function(err) {
                    private$revert(revertState)
                    UpDraftSettings$errorLogger(err)
                })   
            }
            
            return(invisible(NULL))
        }
        
        , errorCheck = function(executionCheck = FALSE
                                , ...
        ) {
            if(!is.character(private$name) || length(private$name) > 1) {
                UpDraftSettings$errorLogger("Workflow name is not a single character string")
            }

            if(length(unique(names(private$connections))) != length(names(private$connections))) {
                UpDraftSettings$errorLogger("There are non-unique connection names")  
            }
            
            if(length(unique(names(private$modules))) != length(names(private$modules))) {
                UpDraftSettings$errorLogger("There are non-unique module names")    
            }
            

    
            if(executionCheck) {
                if(!is.null(private$graph) && !igraph::is.dag(private$graph)) {
                    UpDraftSettings$errorLogger("This workflow is not a Directed Acyclic Graph (DAG)")
                }
                
                # check that there is at most only one connection per module input argument
                # and all required arguments are met through connections and ...
                if(length(private$connections) > 0 && length(private$modules) > 0) {
                    suppliedArgNames <- unlist(list(...))

                    modulesWithErrors = list()
                    for(module in private$modules) {
                        moduleArgs <- module$getInputs()
                        requiredArgNames <- names(moduleArgs[moduleArgs]) # moduleArgs value is True is required. The name of element corrsponds to the actual argument. 
                        
                        inputConns <- private$connections[names(lapply(igraph::incident(private$graph, module$getName(), mode = 'in'), attr, 'name'))]
                        connectionSuppliedArgs <- vapply(inputConns, function(conn){conn$getInputArgument()}, as.character(length(inputConns)))
                        connectionSuppliedArgs <- connectionSuppliedArgs[connectionSuppliedArgs != '']
                        if(length(connectionSuppliedArgs) != length(unique(connectionSuppliedArgs))) {
                            UpDraftSettings$errorLogger(module$getName(), " module has mutiple connections assigned in the same input arguments(s): ", unique(connectionSuppliedArgs[duplicated(connectionSuppliedArgs)]))
                        }
                        
                        metArgs <- intersect(requiredArgNames, c(suppliedArgNames, connectionSuppliedArgs))
                        if(length(metArgs) != length(requiredArgNames)) {
                            UpDraftSettings$errorLogger(module$getName(), " module has unmet required arguments: ", setdiff(requiredArgNames, metArgs))
                        }
                    }
                }
            }

            return(NULL)
        }
        
        , getWorkflowInputs = function() {
            allModuleInputs <- lapply(private$modules, self$getModuleInputs)

            allInputs <- Reduce(function(inputs1, inputs2) {
                inputIntersect <- intersect(names(inputs1), names(inputs2))
                inputDiff <- setdiff(names(inputs2), names(inputs1))
                if (length(inputIntersect) > 0) {
                    inputs1[inputIntersect] <- (inputs1[inputIntersect] | inputs2[inputIntersect])
                }
                if (length(inputDiff) > 0) {
                    inputs1[inputDiff] <- inputs2[inputDiff]
                }
                inputs1
            }, allModuleInputs)

            return(allInputs)
        }

        , getModuleInputs = function(module) {
            if (!is.Module(module)) {
                module <- private$modules[[module]]
            }

            upstreamModules <- self$getUpstreamModules(module)
            internalArguments <- unique(unlist(lapply(upstreamModules, function(upstreamModule) {
                conns <- self$getConnections(upstreamModule, module)
                lapply(conns, function(conn) conn$getInputArgument())
            })))

            allInputs <- module$getInputs()
            return(allInputs[setdiff(names(allInputs), internalArguments)])
        }

        , getConnections = function(module1, module2) {
            if (is.Module(module1)) {
                module1 <- module1$getName()
            }
            if (is.Module(module2)) {
                module2 <- module2$getName()
            }
            
            connections = list()
            
            graphEdges <- igraph::E(private$graph)
            connectingEdges <- graphEdges[module1 %->% module2]
            for (edgeIndex in connectingEdges) {
                selectedEdge <- graphEdges[edgeIndex]
                connections[[selectedEdge$name]] <- private$connections[[selectedEdge$name]]
            }
            
            return(connections)
        }
        
        , getDownstreamModules = function(module) {
            return(private$getNeighbors(private$graph
                                        , module
                                        , "out"))
        }
        
        , getEndingModules = function() {
            endingModules <- list()
            
            graphVertices <- igraph::V(private$graph)
            degreeOut <- igraph::degree(private$graph, mode="out")
            for (vertexIndex in graphVertices[degreeOut == 0]) {
                vertexModule <- private$modules[[graphVertices[vertexIndex]$name]]
                endingModules[[vertexModule$getName()]] <- vertexModule
            }
            
            return(endingModules)
        }
        
        , getAllModules = function() {
            return(private$modules)
        }
        
        , getStartingModules = function() {
            startingModules <- list()

            graphVertices <- igraph::V(private$graph)
            degreeIn <- igraph::degree(private$graph, mode="in")
            for (vertexIndex in graphVertices[degreeIn == 0]) {
                vertexModule <- private$modules[[graphVertices[vertexIndex]$name]]
                requiredInputs <- vertexModule$getInputs()
                
                # TODO: figure out a better way to not run unused vertices in a particular graph
                # Does not work with outside argument injection! Ignore for now 
                # if (!any(requiredInputs)) {
                #     startingModules[[vertexModule$getName()]] <- vertexModule
                # }
                startingModules[[vertexModule$getName()]] <- vertexModule
            }

            return(startingModules)
        }
        
        , getName = function() {
            return(private$name)
        }
        
        
        , getUpstreamModules = function(module) {
            return(private$getNeighbors(private$graph
                                        , module
                                        , "in"))
        }
        
        , getSaveInfo = function() {
            UpDraftSettings$warningLogger("getSaveInfo method does not apply to workflows - run save method()")
            return(list())    
        }
        
        , removeConnection = function(connection) {
            if(is.Connection(connection)) {
                connection <- connection$getName()
            }
            
            tryCatch({
                private$graph <- suppressWarnings(private$graph - igraph::edge(connection))
            }
            , error = function(err) {
                UpDraftSettings$errorLogger("connection parameter does not relate to a current connection in this workflow")    
            })

            private$connections[connection] <- NULL
            
            return(NULL)
        }
        
        , removeModule = function(module)  {
            if(is.Module(module)) {
                module <- module$getName()
            }
            
            tryCatch({
                private$graph <- suppressWarnings(private$graph - igraph::vertex(module))
            }
            , error = function(err) {
                UpDraftSettings$errorLogger("module parameter does not relate to a current module in this workflow")    
            })
            
            private$modules[module] <- NULL
            
            connectionsLost <- setdiff(names(private$connections)
                                       , names(igraph::E(private$graph)))
            private$connections[connectionsLost] <- NULL
            
            return(NULL)
        }
        
        , save = function(filename) {
            workflowData <- list()
            
            workflowData[['class']] <- "DAGWorkflow"
            workflowData[['name']] <- private$name
            
            workflowData[['modules']] <- list()
            for (module in private$modules) {
                workflowData[['modules']][[module$getName()]] <- module$getSaveInfo()
            }
            
            workflowData[['connections']] <- list()
            for (connection in private$connections) {
                workflowData[['connections']][[connection$getName()]] <- connection$getSaveInfo()
            }
            
            write(jsonlite::toJSON(workflowData), filename)
            
            return(NULL)
        }
        
        , visualize = function() {
            # Node Graphics Setup
            nodes_df <- DiagrammeR::create_node_df(n = length(igraph::V(private$graph))
                                                   , label = igraph::V(private$graph)$name
                                                   , fontname = 'arial'
                                                   , type = "modules"
                                                   , color = UpDraftSettings$moduleBorderColor
                                                   , fillcolor = UpDraftSettings$moduleFillColor
                                                   , style = 'filled'
                                                   , penwidth = 3.0
                                                   , tooltip = igraph::V(private$graph)$name)

            # Edge Graphics Setup
            uniqueEdgeList <- unique(igraph::get.edgelist(private$graph, names = TRUE))
            uniqueEdgeListNoNames <- unique(igraph::get.edgelist(private$graph, names = FALSE))
            edgeDisplayColor <- character()
            edgeToolTips <- character()
            for(ix in 1:nrow(uniqueEdgeList)) {
                connectionsBetween <- self$getConnections(uniqueEdgeList[ix,1], uniqueEdgeList[ix, 2])
                if(length(connectionsBetween) == 1) {
                    edgeDisplayColor <- c(edgeDisplayColor, UpDraftSettings$singleConnectionColor)
                } else {
                    edgeDisplayColor <- c(edgeDisplayColor, UpDraftSettings$multipleConnectionsColor)
                }
                
                toolTip <- ""
                for (connection in connectionsBetween) {
                    toolTip <- paste0(toolTip, names(connection$getInputArgument()), '->' , connection$getInputArgument() , '\n')
                }
                edgeToolTips <- c(edgeToolTips, substr(toolTip,1,nchar(toolTip)-1)) # substr removes last '\n' character 
            }
            edges_df <- DiagrammeR::create_edge_df(from = uniqueEdgeListNoNames[,1]
                                                   , to = uniqueEdgeListNoNames[,2]
                                                   , tooltip = edgeToolTips
                                                   , color = edgeDisplayColor
                                                   , penwidth = 3.0)
            
            # Render
            diagrammerGraph <- DiagrammeR::create_graph(nodes_df = nodes_df
                                                        , edges_df = edges_df
                                                        , graph_name = self$getName())
            diagrammerGraph <- DiagrammeR::add_global_graph_attrs(diagrammerGraph, "layout", "dot", attr_type = "graph")
            return(DiagrammeR::grViz(DiagrammeR::generate_dot(diagrammerGraph)))
            
        }
    )
    
    , private = list(
        connections = NULL
        , graph = NULL
        , modules = NULL
        , name = ""
        
        , getNeighbors = function(graph
                                  , module
                                  , direction
        ) {
            if(is.Module(module)) {
                module <- module$getName()
            }
            
            neighborModules <- list()

            graphVertices <- igraph::V(graph)
            downstreamVertices <- igraph::neighbors(graph, module, mode = direction)
            for (downstreamIndex in downstreamVertices) {
                downstreamVertex <- graphVertices[[downstreamIndex]]
                neighborModules[[downstreamVertex$name]] <- private$modules[[downstreamVertex$name]]
            }
            
            return(neighborModules)
        }
        
        , getRevertState = function() {
            revertState <- list()
            
            revertState[["modules"]] <- private$modules
            revertState[["connections"]] <- private$connections
            revertState[["graph"]] <- private$graph
            revertState[["name"]] <- private$name
            
            return(revertState)
        }
        
        , revert = function(previousState) {
            private$modules <- previousState[['modules']]
            private$connections <- previousState[["connections"]]
            private$graph <- previousState[["graph"]]
            private$name <- previousState[["name"]]
            
            return(NULL)
        }
    )
)

#' @title DAG Workflow initFromFile Static Class Method
#' @name DAGWorkflow$initFromFile
#' @description static factory method to initialize a \code{DAGWorkflow} object from a file
#' @param filename file to initialize from
#' @return DAGWorkflow obj
#' @importFrom jsonlite fromJSON
#' @importFrom utils getFromNamespace
DAGWorkflow$initFromFile <- function(filename) {
    tryCatch({
        workflowData <- jsonlite::fromJSON(filename)
        
        workflow <- DAGWorkflow$new(name = workflowData$name)
        
        modules <- list()
        for (moduleData in workflowData$modules) {
            modules[[length(modules) + 1]] <- utils::getFromNamespace(moduleData$class, "updraft")$initFromSaveData(moduleData)
        }
        workflow$addModules(modules)

        connections <- list()
        for (connectionData in workflowData$connections) {
            connections[[length(connections) + 1]] <- utils::getFromNamespace(connectionData$class, "updraft")$initFromSaveData(connectionData)
        }
        workflow$addConnections(connections)
    }
    , error = function(err) {
        UpDraftSettings$errorLogger(filename," is corrupted - cannot load a workflow from it")
    })
    
    return(workflow)
}
