##################
# INITIALIZATION.
##################
# Clear workspace.
rm(list=ls(all=TRUE))

# Set Up Testing Directory Paths
baseDir <- normalizePath(file.path('.'))
testInputDir <- normalizePath(file.path(baseDir,'inst'))
workingDir <- normalizePath(file.path(baseDir, "output"))

# Break line in log.
context("\n>> execution")

# Build Workflows to Test
workflow1 <- DAGWorkflow$new(name="workflow1") # Dependency -- assumes working WorkflowDAG, etc.
module1_1 <- PackageFunctionModule$new(name = "module1_1", fun ="rnorm", package = "stats")
module2_1 <- PackageFunctionModule$new(name = "module2_1", fun ="rnorm", package = "stats") 
module3_1 <- PackageFunctionModule$new(name = "module3_1", fun ="rnorm", package = "stats") 
module4_1 <- CustomFunctionModule$new(name = "module4_1", fun = function(a,b,c){cat(a+b+c, file=file.path(workingDir, file = "workflow1_output.txt"))})
connection1_1 <- DirectedConnection$new(name = "connection1_1", headModule = module1_1, tailModule = module4_1, inputArgument = c('a'))
connection2_1 <- DirectedConnection$new(name = "connection2_1", headModule = module2_1, tailModule = module4_1, inputArgument = c('b'))
connection3_1 <- DirectedConnection$new(name = "connection3_1", headModule = module3_1, tailModule = module4_1, inputArgument = c('c'))
workflow1$addModules(list(module1_1
                          , module2_1
                          , module3_1
                          , module4_1))
workflow1$addConnections(list(connection1_1
                              , connection2_1
                              , connection3_1))

workflow2 <-  DAGWorkflow$new(name="workflow2")
module1_2 <- CustomFunctionModule$new(name = "module1_2", fun = function(a,b){saveRDS(a+b, file=file.path(workingDir, file = "workflow2_output.rds"))})
workflow2$addModules(module1_2)

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")
test_that("Testing Execution Runs", {
    expect_true({
        capture.output(Execute(workflow1
                               , argsContainer = list(n = 1)
                               , mode = SERIAL_MODE))
        file.exists(file.path(workingDir, file = "workflow1_output.txt"))
    })
    unlink(file.path(workingDir, file = "workflow1_output.txt"))
    
    expect_true({
        capture.output(Execute(workflow1
                               , argsContainer = list(n = 1)
                               , mode = PARALLEL_MODE))
        file.exists(file.path(workingDir, file = "workflow1_output.txt"))
    })
    unlink(file.path(workingDir, file = "workflow1_output.txt"))
})

#########################
# OUTPUT FILE TESTING.
#########################
context("Output File Testing")


#########################
# EXPECTED TYPE TESTING
#########################
context("Expected Type Testing")

###########################
# EXPECTED VALUES TESTING
###########################
context("Expected Value Testing")
test_that("Testing Execution Results Values", {
    capture.output(Execute(workflow2
                           , argsContainer = list(a = 1
                                                  ,b = 14)
                           , mode = SERIAL_MODE))
    expect_equal({
       readRDS(file.path(workingDir, "workflow2_output.rds"))
    }, 15)
    expect_true(!any(grepl('_inputs.rds', list.files())))
    expect_true(!any(grepl('_inputs.rds', list.files(workingDir))))
    unlink(file.path(workingDir, file = "workflow2_output.rds"))
    
    capture.output(Execute(workflow2
                           , argsContainer = list(a = 4
                                                  ,b = -1)
                           , moduleInputsSaveDir = workingDir
                           , mode = PARALLEL_MODE))
    expect_equal({
        readRDS(file.path(workingDir, "workflow2_output.rds"))
    }, 3)
    inputsRDSFiles = list.files(workingDir)
    expect_true('module1_2_inputs.rds' %in% inputsRDSFiles)
    unserializedInputs <- readRDS(file.path(workingDir, file = "module1_2_inputs.rds"))
    expect_equal(class(unserializedInputs), 'list')
    expect_equal(length(unserializedInputs), 2)
    expect_equal(unserializedInputs$a, 4)
    expect_equal(unserializedInputs$b, -1)
    unlink(file.path(workingDir, file = "workflow2_output.rds"))
    unlink(file.path(workingDir, file = "module1_2_inputs.rds"))
    
    capture.output(Execute(workflow2
                           , argsContainer = list(a = 4
                                                  ,b = -1)
                           , moduleInputsSaveDir = file.path(workingDir,'does_not_exist')
                           , mode = PARALLEL_MODE))
    inputsRDSFiles = list.files(file.path(workingDir, 'does_not_exist'))
    expect_true('module1_2_inputs.rds' %in% inputsRDSFiles)
    unlink(file.path(workingDir, file = "workflow2_output.rds"))
    unlink(file.path(workingDir, 'does_not_exist', file = "module1_2_inputs.rds"))
    unlink(file.path(workingDir, 'does_not_exist'))
})

#########################
# CLEAN UP OUTPUT FILES.
#########################
# Rm output directory.
outputDirs <- list.dirs(file.path(workingDir), recursive = FALSE)
unlink(outputDirs[grepl("output", outputDirs)], recursive = TRUE)

# Rm misc data files.
outputFiles <- list.files(workingDir, full.names = TRUE)

##################
# FINALLY...
##################
# Check that nothing is in the output folder.
context("Unittest Output Directory Check")

test_that("Output directory is empty.", expect_equal(length(list.files(workingDir)), 0))
