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
context("\n>> connection_directed")

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")

test_that("Testing DirectedConnection has properly implemented inherited interfaces", {
    expect_true({
        updraft:::CheckInterfaceImplementation(DirectedConnection)
        TRUE
    })
})

test_that("Testing DirectedConnection errors out with bad input", {
    headModule = PackageFunctionModule$new(name = "headModule", fun = "paste") # dependency to have PackageFunctionModule working
    tailModule = PackageFunctionModule$new(name = "tailModule", fun = "print")
    
    expect_error({
        DirectedConnection$new(name = 1.0
                               , headModule = headModule
                               , tailModule = tailModule
                               , inputArgument = "a")
    }, regexp = 'name')
    
    expect_error({
        DirectedConnection$new(name = c("should not", "be 2 strings here")
                               , headModule = headModule
                               , tailModule = tailModule
                               , inputArgument = "a")
    }, regexp = 'name')
    
    expect_error({
        DirectedConnection$new(name = "conn"
                               ,headModule = 1.0
                               , tailModule = tailModule
                               , inputArgument = "a")
    }, regexp = "Module")
    
    expect_error({
        DirectedConnection$new(name = "conn"
                               , headModule = headModule
                               , tailModule = 1.0
                               , inputArgument = "a")
    }, regexp = "Module")
    
    expect_error({
        DirectedConnection$new(name = "conn"
                               , headModule = 1.0
                               , tailModule = 1.0
                               , inputArgument = "a")
    }, regexp = "Module")
    
    expect_error({
        DirectedConnection$new(name = "conn"
                               , headModule = headModule
                               , tailModule = tailModule
                               , inputArgument = 1.0)
    }, regexp = "input")
    
    expect_error({
        DirectedConnection$initFromSaveData(list(NULL))
    }, regexp = "name")
    
    expect_error({
        DirectedConnection$initFromSaveData(list(name = "conn"
                                                 , headModule = headModule
                                                 , tailModule = tailModule
                                                 , inputArgument = 1.0))
    }, regexp = "input")
    
    expect_error({
        DirectedConnection$initFromSaveData(list(name = "conn"
                                                 , headModule = 1.0
                                                 , tailModule = tailModule
                                                 , inputArgument = 'a'))
    }, regexp = "Module")
    
    expect_error({
        DirectedConnection$initFromSaveData(list(name = 1.0
                                                 , headModule = headModule
                                                 , tailModule = tailModule
                                                 , inputArgument = 'a'))
    }, regexp = "name")
})

test_that("Testing DirectedConnection should complete without error", {
    headModule = PackageFunctionModule$new(name = "headModule", fun = "paste") # dependency to have PackageFunctionModule working
    tailModule = PackageFunctionModule$new(name = "tailModule", fun = "print")
    
    expect_true({
        conn <- DirectedConnection$new(name = "conn"
                                       , headModule = headModule
                                       , tailModule = tailModule
                                       , inputArgument = "a")
        TRUE
    })
    
    expect_true({
        conn <- DirectedConnection$new(name = "conn"
                                       , headModule = headModule
                                       , tailModule = tailModule
                                       , inputArgument = "a")
        TRUE
    })
    
    expect_true({
        conn <- DirectedConnection$new(name = "conn"
                                       , headModule = headModule
                                       , tailModule = "tailModule"
                                       , inputArgument = "a")
        conn$getName()
        conn$getHeadModuleName()
        conn$getTailModuleName()
        conn$getInputArgument()
        conn$filterOutputValue(35)
        TRUE
    })
    
    expect_true({
        conn <- DirectedConnection$initFromSaveData(list(name = "conn"
                                                         , headModule = headModule
                                                         , tailModule = "tailModule"
                                                         , inputArgument = "a"))
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
test_that("Check DirectedConnection constructor class and methods that return a value", {
    conn <- DirectedConnection$new(name = "conn"
                           , headModule = "headModule"
                           , tailModule = "tailModule"
                           , inputArgument = "a")
    expect_is({
        conn
    }, "R6")
    
    expect_is({
        conn
    }, "DirectedConnection")
    
    expect_is({
        conn$getSaveInfo()
    }, "list")
    
    expect_is({
        conn$getName()
    }, "character")
    
    expect_is({
        conn$getHeadModuleName()
    }, "character")
    
    expect_is({
        conn$getTailModuleName()
    }, "character")
    
    expect_is({
        conn$getInputArgument()
    }, "character")
    
    expect_is({
        conn$filterOutputValue(35)
    }, "numeric")
    
    expect_is({
        conn$filterOutputValue("test")
    }, "character")
    
    expect_is({
        DirectedConnection$initFromSaveData(conn$getSaveInfo())
    }, "R6")
})


###########################
# EXPECTED VALUES TESTING
###########################
context("Expected Value Testing")
test_that("Check method return values", {
    conn <- DirectedConnection$new(name = "conn"
                                   , headModule = "headModule"
                                   , tailModule = "tailModule"
                                   , inputArgument = "a")
    expect_equal({
        conn$getName()
    }, "conn")
    expect_equal({
        conn$getHeadModuleName()
    }, "headModule")
    expect_equal({
        conn$getTailModuleName()
    }, "tailModule")
    expect_gt({
        length(conn$getSaveInfo())
    }, 0)
    expect_equal({
        conn$getInputArgument()
    }, "a")
    expect_equal({
        conn$filterOutputValue(35)
    }, 35)
    expect_equal({
        conn$filterOutputValue("This is a test")
    }, "This is a test")
    
    conn2 <- DirectedConnection$initFromSaveData(conn$getSaveInfo())
    expect_equal({
        conn2$getHeadModuleName()
    }, conn$getHeadModuleName())
    expect_equal({
        conn2$getTailModuleName()
    }, conn$getTailModuleName())
    expect_equal({
        conn2$getInputArgument()
    }, conn$getInputArgument())
})

##################
# FINALLY...
##################
# Check that nothing is in the output folder.
context("Unittest Output Directory Check")

test_that("Output directory is empty.", expect_equal(length(list.files(workingDir)), 0))

