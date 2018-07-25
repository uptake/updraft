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
context("\n>> utils_connections")

# Custom functions to test Autowiring
func1 <- function(a, b) {
    return(list(a = a, b = b, c = a + b))
}

func2 <- function(a, c) {
    stop("Sorry! Error.")
    return(list(a = a + c))
}

# Testing Modules
originalErrorLogger <- UpDraftSettings$errorLogger
originalInfoLogger <- UpDraftSettings$infoLogger
UpDraftSettings$update(infoLogger = function(x){print(paste("INFO",lapply(x,as.character),"\n",collapse = ","))})
UpDraftSettings$update(errorLogger = function(x){print(paste("ERROR!",lapply(x,as.character),"\n",collapse = ",")); stop("ERROR")})

# Testing Workflow

###################
# END-TO-END TESTS
###################
test_that("Testing LogStackTrace works with no file", {
    output <- capture.output(value <- FutureFunctionCall(func = func2, args = list(a=1,c=2)))
    expect_error(future::value(value))
    # expect_true(grepl("ERROR",value$value$message))  # TODO Get this Unit Test Working with Travis
})

test_that("Testing LogStackTrace works with a file", {
    output <- capture.output(value <- FutureFunctionCall(func = func2, args = list(a=1,c=2),funcName = "Func2"))
    expect_error(future::value(value))
    expect_true(file.exists("Func2.rda")) # check dump frames worked
    expect_true(file.exists("Func2.error")) # check stack trace worked
    testEnv <- new.env()
    load("Func2.rda",envir = testEnv)
    expect_true(class(testEnv$Func2) == "dump.frames")
    file.remove("Func2.rda")
    file.remove("Func2.error")
    rm(testEnv)
})

test_that("Expect No LogStackTrace with a working func", {
    output <- capture.output(value <- FutureFunctionCall(func = func1, args = list(a=1,b=2),funcName = "Func1"))
    expect_error(!future::value(value))
    expect_true(!file.exists("Func1.rda")) # check dump frames worked
    expect_true(!file.exists("Func1.error")) # check stack trace worked
    expect_true(!any(grepl("ERROR",output))) # check that there is a printed error
})


## Reset logger
UpDraftSettings$update(infoLogger = originalInfoLogger)
UpDraftSettings$update(errorLogger = originalErrorLogger)

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


##################
# FINALLY...
##################
# Check that nothing is in the output folder.
context("Unittest Output Directory Check")

