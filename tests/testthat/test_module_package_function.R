##################
# INITIALIZATION.
##################
# Clear workspace.
rm(list=ls(all=TRUE))

# Break line in log.
context("\n>> module_package_function")

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")

test_that("Testing PackageFunctionModule has properly implemented inherited interfaces", {
    expect_true({
        updraft:::CheckInterfaceImplementation(PackageFunctionModule)
        TRUE
    })
})

test_that("Fatal error testing PackageFunctionModule", {
    expect_error({
        PackageFunctionModule$new(name = "test"
                                  , fun = 1.0
                                  , package = "")
    }, regexp = 'fun')
    expect_error({
        PackageFunctionModule$new(name = "test"
                                  , fun = list()
                                  , package = "")
    }, regexp = 'fun')
    expect_error({
        PackageFunctionModule$new(package = "")
    }, regexp = 'name')
    expect_error({
        PackageFunctionModule$new(name = "test"
                                  , fun = "paste"
                                  , package = 1.0)
    }, regexp = 'package')
    expect_error({
        PackageFunctionModule$new(name = "test"
                                  , fun = "paste"
                                  , package = list())
    })
    expect_error({
        PackageFunctionModule$new(name = list()
                                  , fun = "paste"
                                  , package = "")
    }, regexp = "name")
    expect_error({
        PackageFunctionModule$new(name = 1.0
                                  , fun = "paste"
                                  , package = "")
    }, regexp = "name")
    expect_error({
        PackageFunctionModule$initFromSaveData(list(name = 1.0
                                                    , fun = "paste"
                                                    , package = "base"))
    }, regexp = "name")
    expect_error({
        PackageFunctionModule$initFromSaveData(list(name = 'test'
                                                    , fun = 1.0
                                                    , package = "base"))
    }, regexp = "fun")
    expect_error({
        module <- PackageFunctionModule$new(name = "test"
                                            , fun = "paste"
                                            , package = "does_not_exist")
        module$startExecution(list("Should", "Not Work"))
        module$getOutput()
    }, regexp = "package")
})

# Expect completion
test_that("Should complete testing PackageFunctionModule", {
    expect_true({
        module <- PackageFunctionModule$new(name = "test", fun = "paste", package = "base")
        module$errorCheck()
        module$getName()
        module$hasCompleted()
        module$getExecutionTimings()
        R.utils::withTimeout(module$getOutput(), timeout = 1.0)
        module$clearOutputCache()
        module$getSaveInfo()
        module$startExecution(list("This", "should work"))
        module$hasCompleted()
        module$getExecutionTimings()
        module$getOutput()
        module$getFuncObj()
        module$getInputs()
        PackageFunctionModule$initFromSaveData(module$getSaveInfo())
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
test_that("Check PackageFunctionModule constructor class and methods that return a value", {
    module <- PackageFunctionModule$new(name = "test", fun = "paste", package = "base")
    expect_is({
        module
    }, "R6")
    expect_is({
        module
    }, "PackageFunctionModule")
    expect_is({
        module$getExecutionTimings()[['startTime']]
    }, 'NULL')
    expect_is({
        module$getExecutionTimings()[['endTime']]
    }, 'NULL')
    expect_is({
        module$startExecution(list("This", "should work"))
        module$getOutput()
    }, "character")
    expect_is({
        module$getExecutionTimings()[['startTime']]
    }, c("POSIXct", "POSIXt"))
    expect_is({
        module$getExecutionTimings()[['endTime']]
    }, c("POSIXct", "POSIXt"))
    expect_is({
        module$getName()
    }, "character")
    expect_is({
        module$getSaveInfo()
    }, "list")
    expect_is({
        module$getFuncObj()
    }, "function")
    expect_is({
        module$getInputs()
    }, "logical")
    expect_is({
        PackageFunctionModule$initFromSaveData(module$getSaveInfo())
    }, "PackageFunctionModule")
})

###########################
# EXPECTED VALUES TESTING
###########################
context("Expected Value Testing")
test_that("Check method return values", {
    module <- PackageFunctionModule$new(name = "test", fun = "paste", package = "base")
    expect_null({module$getExecutionTimings()[['startTime']]})
    expect_null({module$getExecutionTimings()[['endTime']]})
    expect_equal({
        module$startExecution(list("This", "should work"))
        module$getOutput()
    }, "This should work")
    expect_gt({
        as.numeric(module$getExecutionTimings()[['endTime']] - module$getExecutionTimings()[['startTime']])
    }, 0)
    expect_equal({
        module$getName()
    }, "test")
    expect_gt({
        length(module$getSaveInfo())
    }, 0)
    expect_equal({
        module$getFuncObj()
    }, paste)
    expect_equal({
        module$getInputs()
    }, c(...=FALSE, sep=FALSE, collapse=FALSE))
    
    module2 <- PackageFunctionModule$initFromSaveData(module$getSaveInfo())
    expect_equal({
        module2$getInputs()
    }, module$getInputs())
    expect_equal({
        module2$getFuncObj()
    }, module$getFuncObj())
    expect_equal({
        module2$getName()
    }, module$getName())
})

##################
# FINALLY...
##################
# Check that nothing is in the output folder.
context("Unittest Output Directory Check")

test_that("Output directory is empty.", expect_equal(length(list.files(workingDir)), 0))
