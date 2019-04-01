##################
# INITIALIZATION.
##################
# Clear workspace.
rm(list=ls(all=TRUE))

# Set Up Testing Directory Paths
baseDir <- normalizePath(file.path('.'))
testInputDir <- normalizePath(file.path(baseDir,'inst'))

outputDir <- file.path(tempdir(), "output")
dir.create(outputDir)

# Break line in log.
context("\n>> connection_directed_unpack")

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")

test_that("Testing DirectedUnpackConnection has properly implemented inherited interfaces", {
    expect_true({
        updraft:::CheckInterfaceImplementation(DirectedUnpackConnection)
        TRUE
    })
})

test_that("Testing DirectedUnpackConnection errors out with bad input", {
    headModule = PackageFunctionModule$new(name = "headModule", fun = "paste") # dependency to have PackageFunctionModule working
    tailModule = PackageFunctionModule$new(name = "tailModule", fun = "print")

    expect_error({
        DirectedUnpackConnection$new(name = 1.0
                                     , headModule = headModule
                                     , tailModule = tailModule
                                     , inputArgument = c(test = "a"))
    }, regexp = 'name')

    expect_error({
        DirectedUnpackConnection$new(name = c("should not", "be 2 strings here")
                                     , headModule = headModule
                                     , tailModule = tailModule
                                     , inputArgument = c(test = "a"))
    }, regexp = 'name')

    expect_error({
        DirectedUnpackConnection$new(name = "conn"
                                     , headModule = 1.0
                                     , tailModule = tailModule
                                     , inputArgument = c(test = "a"))
    }, regexp = 'Module')

    expect_error({
        DirectedUnpackConnection$new(name = "conn"
                                     , headModule = headModule
                                     , tailModule = 1.0
                                     , inputArgument = c(test = "a"))
    }, regexp = 'Module')

    expect_error({
        DirectedUnpackConnection$new(name = "conn"
                                     , headModule = 1.0
                                     , tailModule = 1.0
                                     , inputArgument = c(test = "a"))
    }, regexp = 'Module')

    expect_error({
        DirectedUnpackConnection$new(name = "conn"
                                     , headModule = headModule
                                     , tailModule = tailModule
                                     , inputArgument = "a")
    }, regexp = 'missing')

    expect_error({
        DirectedUnpackConnection$new(name = "conn"
                                     , headModule = headModule
                                     , tailModule = tailModule
                                     , inputArgument = 1.0)
    }, regexp = 'input')

    expect_error({
        DirectedUnpackConnection$initFromSaveData(list(NULL))
    }, regexp = 'name')

    expect_error({
        DirectedUnpackConnection$initFromSaveData(list(name = "conn"
                                                       , headModule = headModule
                                                       , tailModule = tailModule
                                                       , inputArgument = 1.0
                                                       , unpackValue = 'test'))
    }, regexp = 'input')

    expect_error({
        DirectedUnpackConnection$initFromSaveData(list(name = "conn"
                                                       , headModule = 1.0
                                                       , tailModule = tailModule
                                                       , inputArgument =  'a'
                                                       , unpackValue = 'test'))
    })

    expect_error({
        DirectedUnpackConnection$initFromSaveData(list(name = 1.0
                                                       , headModule = headModule
                                                       , tailModule = tailModule
                                                       , inputArgument = 'a'
                                                       , unpackValue = 'test'))
    }, regexp = 'name')
})

test_that("Testing DirectedUnpackConnection should complete without error", {
    headModule = PackageFunctionModule$new(name = "headModule", fun = "paste") # dependency to have PackageFunctionModule working
    tailModule = PackageFunctionModule$new(name = "tailModule", fun = "print")

    expect_true({
        conn <- DirectedUnpackConnection$new(name = "conn"
                                             , headModule = headModule
                                             , tailModule = tailModule
                                             , inputArgument = c(test = "a"))
        TRUE
    })

    expect_true({
        conn <- DirectedUnpackConnection$new(name = "conn"
                                             , headModule = headModule
                                             , tailModule = "tailModule"
                                             , inputArgument = c(test ="a"))
        conn$getName()
        conn$getHeadModuleName()
        conn$getTailModuleName()
        conn$getInputArgument()
        conn$filterOutputValue(list(test = 35))
        TRUE
    })

    expect_error({
        conn <- DirectedUnpackConnection$new(name = "conn"
                                             , headModule = headModule
                                             , tailModule = "tailModule"
                                             , inputArgument = c(test ="a"))
        conn$getName()
        conn$getHeadModuleName()
        conn$getTailModuleName()
        conn$getInputArgument()
        conn$filterOutputValue(list(output = 35))
    },regexp = "inputArgument")

    expect_true({
        conn <- DirectedUnpackConnection$new(name = "conn"
                                             , headModule = headModule
                                             , tailModule = "tailModule"
                                             , inputArgument = c(test ="a"))
        conn$getName()
        conn$getHeadModuleName()
        conn$getTailModuleName()
        conn$getInputArgument()
        returnVal <- conn$filterOutputValue(list(test=NULL))
        expect_null(returnVal)
        TRUE
    })

    expect_true({
        conn <- DirectedUnpackConnection$initFromSaveData(list(name = "conn"
                                                               , headModule = headModule
                                                               , tailModule = "tailModule"
                                                               , inputArgument = "a"
                                                               , unpackValue = 'test'))
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
test_that("Check DirectedUnpackConnection constructor class and methods that return a value", {
    conn <- DirectedUnpackConnection$new(name = "conn"
                                         , headModule = "headModule"
                                         , tailModule = "tailModule"
                                         , inputArgument = c(test = "a"))
    expect_is({
        conn
    }, "R6")

    expect_is({
        conn
    }, "DirectedUnpackConnection")

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
        conn$filterOutputValue(list(test = 35))
    }, "numeric")

    expect_is({
        conn$filterOutputValue(list(test = "test"))
    }, "character")

    expect_is({
        DirectedUnpackConnection$initFromSaveData(conn$getSaveInfo())
    }, "R6")
})


###########################
# EXPECTED VALUES TESTING
###########################
context("Expected Value Testing")
test_that("Check method return values", {
    conn <- DirectedUnpackConnection$new(name = "conn"
                                         , headModule = "headModule"
                                         , tailModule = "tailModule"
                                         , inputArgument = c(test = "a"))
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
    }, c(test = "a"))
    expect_equal({
        conn$filterOutputValue(list(test = 35))
    }, 35)
    expect_equal({
        conn$filterOutputValue(list(test = "This is a test"))
    }, "This is a test")

    conn2 <- DirectedUnpackConnection$initFromSaveData(conn$getSaveInfo())
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

test_that("Output directory is empty.", expect_equal(length(list.files(outputDir)), 0))
