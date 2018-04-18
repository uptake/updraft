# Clear workspace.
rm(list=ls(all=TRUE)) # any locals.
rm(list = ls(envir = .GlobalEnv), envir=.GlobalEnv) # any globals.

# Library loading.
library(R.utils)
library(updraft)

# set in global env for access by all test scripts
assign("baseDir", normalizePath(file.path('.')), envir = .GlobalEnv) # testthis directory.
assign("testInputDir", normalizePath(file.path(baseDir,'inst')), envir = .GlobalEnv) #test input
assign("workingDir", normalizePath(file.path(baseDir, "output")), envir = .GlobalEnv)
