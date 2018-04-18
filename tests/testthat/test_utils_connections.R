##################
# INITIALIZATION.
##################
# Clear workspace.
rm(list=ls(all=TRUE))

# Break line in log.
context("\n>> utils_connections")

# Custom functions to test Autowiring
func1 <- function(a, b) {
    return(list(a = a, b = b, c = a + b))
}

func2 <- function(a, c) {
    return(list(a = a + c))
}
# should not work with autowiring
func3 <- function(a) {
    return(a)
}
# should not work with autowiring
func4 <- function(a, b) {
    if(a) {
        return(list(b = b))
    } else {
        return(list(b = 1))
    }
}

# Testing Modules
module1 <- CustomFunctionModule$new(name = "mod1", fun = func1) # dependency -- assumes working CustomFunctionModule
module2 <- CustomFunctionModule$new(name = "mod2", fun = func2) # dependency -- assumes working CustomFunctionModule
module3 <- CustomFunctionModule$new(name = "mod3", fun = func3) # dependency -- assumes working CustomFunctionModule
module4 <- CustomFunctionModule$new(name = "mod4", fun = func4) # dependency -- assumes working CustomFunctionModule

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")
test_that("Testing Autowire Runs", {
    expect_true({
        Autowire(headModules = module1
                 , tailModules = module2
                 , connectionNamePrefix = 'conn')
        TRUE
    })
    
    expect_true({
        Autowire(headModules = module2
                 , tailModules = module1
                 , connectionNamePrefix = 'conn')
        TRUE
    })

    expect_true({
        Autowire(headModules = module4
                 , tailModules = module1
                 , connectionNamePrefix = 'conn')
        TRUE
    })
})
test_that("Testing Autowire Fails when Appropriate", {
    expect_error({
        Autowire(headModules = module3
                 , tailModules = module1
                 , connectionNamePrefix = 'conn')
        TRUE
    }, regexp = 'autowire')
})
test_that("Testing CreateDirectedConnections Runs", {
    expect_true({
        CreateDirectedConnections(headModules = module1
                 , tailModules = list(module2, module3)
                 , inputArgument = c(a = 'a')
                 , connectionNamePrefix = 'conn')
        TRUE
    })
})

#########################
# OUTPUT FILE TESTING.
#########################
context("Output File Testing")

#########################
# EXPECTED TYPE TESTING
#########################
context("Expected Type Testing")
test_that("Testing Autowire Return Types", {
    connections <- Autowire(headModules = module1
                            , tailModules = module2
                            , connectionNamePrefix = 'conn')
    expect_is({connections}, 'list')
    expect_is({connections[[1]]}, 'DirectedUnpackConnection')
})
test_that("Testing CreateDirectedConnections Return Types", {
    connections <- CreateDirectedConnections(headModules = module1
                                             , tailModules = list(module2, module3)
                                             , inputArgument = c(a = 'a')
                                             , connectionNamePrefix = 'conn')
    expect_is({connections}, 'list')
    expect_is({connections[[1]]}, 'DirectedConnection')
})

###########################
# EXPECTED VALUES TESTING
###########################
context("Expected Value Testing")
test_that("Testing Autowire Return Values", {
    connections <- Autowire(headModules = module1
                            , tailModules = module2
                            , connectionNamePrefix = 'conn')
    expect_equal({length(connections)}, 2)
    expect_equal({connections[[1]]$getHeadModuleName()}, module1$getName())
    expect_equal({connections[[1]]$getTailModuleName()}, module2$getName())
    expect_equal({connections[[1]]$getInputArgument()}, c(a = "a"))
    expect_equal({connections[[2]]$getInputArgument()}, c(c = "c"))
})
test_that("Testing CreateDirectedConnections Return Values", {
    connections <- CreateDirectedConnections(headModules = module1
                                             , tailModules = list(module2, module3)
                                             , inputArgument = c(a = 'a')
                                             , connectionNamePrefix = 'conn')
    expect_equal({length(connections)}, 2)
    expect_equal({connections[[1]]$getHeadModuleName()}, module1$getName())
    expect_equal({connections[[1]]$getTailModuleName()}, module2$getName())
    expect_equal({connections[[2]]$getTailModuleName()}, module3$getName())
    expect_equal({connections[[1]]$getInputArgument()}, c(a = "a"))
    expect_equal({connections[[2]]$getInputArgument()}, c(a = "a"))
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
