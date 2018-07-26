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
context("\n>> settings")

# Define Reporting Functions
LoggerFunc1 <- function(...) {
    print(...)
}
LoggerFunc2 <- function(...) {
    warning(...)
}
LoggerFunc3 <- function(...) {
    stop(...)
}

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")
test_that("Testing UpDraftSettings$update(...) runs", {
    expect_true({
        UpDraftSettings$update()
        TRUE
    })
})
test_that("Testing UpDraftSettings$update(...) errors correctly", {
    expect_error({
        UpDraftSettings$update(does_not_exist = "does_not_exist")
    }, regex = 'not setting')
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
test_that("Testing UpDraftSettings$update(...) changes logger settings", {
    UpDraftSettings$update(infoLogger = LoggerFunc1
                           , warningLogger = LoggerFunc2
                           , errorLogger = LoggerFunc3)
    expect_equal({
        UpDraftSettings$infoLogger
    }, LoggerFunc1)
    expect_equal({
       UpDraftSettings$warningLogger
    }, LoggerFunc2)
    expect_equal({
      UpDraftSettings$errorLogger
    }, LoggerFunc3)
})

test_that("Testing UpDraftSettings active bindings reset logger settings", {
    UpDraftSettings$infoLogger = updraft:::DEFAULTS$infoLogger
    UpDraftSettings$warningLogger = updraft:::DEFAULTS$warningLogger
    UpDraftSettings$errorLogger = updraft:::DEFAULTS$errorLogger

    expect_equal({
        UpDraftSettings$infoLogger
    }, updraft:::DEFAULTS$infoLogger)
    expect_equal({
        UpDraftSettings$warningLogger
    },  updraft:::DEFAULTS$warningLogger)
    expect_equal({
        UpDraftSettings$errorLogger
    }, updraft:::DEFAULTS$errorLogger)
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