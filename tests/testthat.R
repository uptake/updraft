# Note that you would never run this file directly. This is used by testthat
# to execute the tests in the testthat folder

# NOTE: ALL NECESSARY LIBRARIES ARE CALLED IN testthat/test_aaa.R.

library(testthat)
library(updraft)

testthat::test_check("updraft")
