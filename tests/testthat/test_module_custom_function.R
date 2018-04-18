##################
# INITIALIZATION.
##################
# Clear workspace.
rm(list=ls(all=TRUE))

# Define Custom Function to reuse during unit tests
func <- function(a, b) {
    return(a + b)
}

# Break line in log.
context("\n>> module_custom_function")

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")

test_that("Testing CustomFunctionModule has properly implemented inherited interfaces", {
    expect_true({
        updraft:::CheckInterfaceImplementation(CustomFunctionModule)
        TRUE
    })
})

test_that("Fatal error testing CustomFunctionModule", {
    expect_error({
        CustomFunctionModule$new(name = "test"
                                 , fun = 1.0)
    }, regexp = 'fun')
    expect_error({
        CustomFunctionModule$new(name = "test"
                                 , fun = list())
    }, regexp = 'fun')
    expect_error({
        CustomFunctionModule$initFromSaveData(list(name = 1.0
                                                    , fun = func))
    }, regexp = 'json')
    expect_error({
        module <- CustomFunctionModule$new(name = "test"
                                            , fun = NULL)
        module$startExecution(list(a = 1))
        module$getOutput()
    }, regexp = 'fun')
})

# Expect completion
test_that("Should complete testing CustomFunctionModule", {
    expect_true({
        module <- CustomFunctionModule$new(name = "test", fun = func)
        module$errorCheck()
        module$getName()
        module$hasCompleted()
        R.utils::withTimeout(module$getOutput(), timeout = 1.0)
        module$clearOutputCache()
        module$getSaveInfo()
        module$startExecution(list(a = 1, b = 2))
        module$getOutput()
        module$getFuncObj()
        module$getInputs()
        CustomFunctionModule$initFromSaveData(module$getSaveInfo())
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
test_that("Check CustomFunctionModule constructor class and methods that return a value", {
    module <- CustomFunctionModule$new(name = "test", fun = func)
    expect_is({
        module
    }, "R6")
    expect_is({
        module
    }, "CustomFunctionModule")
    expect_is({
        module$startExecution(list(a = 1, b = 2))
        module$getOutput()
    }, "numeric")
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
        CustomFunctionModule$initFromSaveData(module$getSaveInfo())
    }, "CustomFunctionModule")
})

###########################
# EXPECTED VALUES TESTING
###########################
context("Expected Value Testing")
test_that("Check method return values", {
    module <- CustomFunctionModule$new(name = "test", fun = func)
    expect_equal({
        module$startExecution(list(a = 1, b = 2))
        module$getOutput()
    }, 3)
    expect_equal({
        module$getName()
    }, "test")
    expect_gt({
        length(module$getSaveInfo())
    }, 0)
    expect_equal({
        module$getFuncObj()
    }, func)
    expect_equal({
        module$getInputs()
    }, c(a=TRUE, b=TRUE))

    module2 <- CustomFunctionModule$initFromSaveData(module$getSaveInfo())
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
