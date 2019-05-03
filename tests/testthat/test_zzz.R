#' Unit Test UpDraft Package
#'
#' This test script will be executed at the very end of unit tests
#' to perform some clean-ups for temporary testing output.
outputFiles <-list.files('.')
outputFiles <-outputFiles[!grepl('^test_.*.R|inst|output|unwritten_tests|README.rst',outputFiles)]

for(file in outputFiles){
  unlink(file, recursive = T,force=T)
}

# Free up some unused memory
invisible(gc())
