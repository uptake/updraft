#' @title UpDraft Settings
#' @name UpDraftSettings
#' @description Stores UpDraft settings. User can directly adjust settings in
#'              this object to inject things into UpDraft such as custom logger
#'              functions.
#' @importFrom R6 R6Class
#' @section Adjustable Settings:
#' \describe{
#'     \item{\code{errorLogger}}{
#'         \itemize{
#'             \item{function to use for UpDraft error statements. Specified function should allow \code{...} string inputs. Caution, specified function should throw a standard R error when called.}
#'          }
#'     }
#'     \item{\code{infoLogger}}{
#'         \itemize{
#'             \item{function to use for UpDraft info statements. Specified function should allow \code{...} string inputs.}
#'         }
#'     }
#'     \item{\code{moduleMaxMemorySize}}{
#'         \itemize{
#'             \item{Max memory a module can occupy without causing a runtime error. Value is assumed to be in Bytes.}
#'         }
#'     }
#'     \item{\code{moduleBorderColor}}{
#'         \itemize{
#'             \item{Graphviz color string of color to render the border of modules in UpDraft workflow visualization.}
#'         }
#'     }
#'     \item{\code{moduleFillColor}}{
#'         \itemize{
#'             \item{Graphviz color string of color to render the fill of modules in UpDraft workflow visualization.}
#'         }
#'     }
#'     \item{\code{monitorRate}}{
#'         \itemize{
#'             \item{Numeric value in seconds for the amount of time the monitoring process sleeps between checking if modules have completed.}
#'         }
#'     }
#'     \item{\code{multipleConnectionsColor}}{
#'         \itemize{
#'             \item{Graphviz color string of color to render multiple connections in UpDraft workflow visualization.}
#'         }
#'     }
#'     \item{\code{singleConnectionColor}}{
#'         \itemize{
#'             \item{Graphviz color string of color to render single connections in UpDraft workflow visualization.}
#'         }
#'     }
#'     \item{\code{warnLogger}}{
#'         \itemize{
#'             \item{function to use for UpDraft warning statements. Specified function should allow \code{...} string inputs. Caution, specified function should throw a standard R warning when called.}
#'         }
#'     }
#' }
#' @section Public:
#' \describe{
#'     \item{\code{update(...)}}{
#'         \itemize{
#'             \item{Updates multiple settings at once.}
#'             \item{\bold{\code{...}}: named parameters where the name of each parameter is the setting to change and the value of the parameter is the setting's new value.}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#' }
#' @section Private:
#' \describe{
#'     \item{\code{values}}{
#'         \itemize{
#'             \item{Stores values of settings.}
#'         }
#'     }
#'     \item{\code{setFunction(value, setting)}}{
#'         \itemize{
#'             \item{Updates a setting that stores a function type obj. This is a helper function for this object's active bindings.}
#'             \item{\bold{\code{value}}: value to update setting with. If \code{NULL}, signifies to return current value of \code{setting}.}
#'             \item{\bold{\code{setting}}: string of setting to update.}
#'             \item{\bold{Returns}: If \code{value} is \code{NULL}, returns current setting value. Else, updates setting to \code{value}.}
#'         }
#'     }
#'     \item{\code{setPositiveNumeric(value, setting)}}{
#'         \itemize{
#'             \item{Updates a setting that stores a positive numeric. This is a helper function for this object's active bindings.}
#'             \item{\bold{\code{value}}: value to update setting with. If \code{NULL}, signifies to return current value of \code{setting}.}
#'             \item{\bold{\code{setting}}: string of setting to update.}
#'             \item{\bold{Returns}: If \code{value} is \code{NULL}, returns current setting value. Else, updates setting to \code{value}.}
#'         }
#'     }
#'     \item{\code{setString(value, setting)}}{
#'         \itemize{
#'             \item{Updates a setting that stores a single string. This is a helper function for this object's active bindings.}
#'             \item{\bold{\code{value}}: value to update setting with. If \code{NULL}, signifies to return current value of \code{setting}.}
#'             \item{\bold{\code{setting}}: string of setting to update.}
#'             \item{\bold{Returns}: If \code{value} is \code{NULL}, returns current setting value. Else, updates setting to \code{value}.}
#'         }
#'     }
#' }
#' @export
UpDraftSettings <- R6::R6Class("UpDraftSettings"
    , public = list(
        initialize = function() {
            private$values$errorLogger <- DEFAULTS$errorLogger
            private$values$infoLogger <- DEFAULTS$infoLogger
            private$values$warningLogger <- DEFAULTS$warningLogger
            
            private$values$monitorRate <- DEFAULTS$monitorRate
            private$values$moduleMaxMemorySize <- DEFAULTS$moduleMaxMemorySize
            
            private$values$moduleBorderColor <- DEFAULTS$moduleBorderColor
            private$values$moduleFillColor <- DEFAULTS$moduleFillColor
            private$values$singleConnectionColor <- DEFAULTS$singleConnectionColor
            private$values$multipleConnectionsColor <- DEFAULTS$multipleConnectionsColor
        }
        
        , update = function(...) {
            values <- list(...)
            fields <- setdiff(names(values), "")
            
            if(length(values) != length(fields)) {
                self$errorLogger("All values in '...' must be named") # assumes errorLogger is valid    
            }
            
            notSupportedSettings <- setdiff(fields, names(self))
            if(length(notSupportedSettings) > 0) {
                self$errorLogger(paste(notSupportedSettings, collapse = ' '), " is(are) not setting(s) in UpDraft") # assumes errorLogger is valid   
            }
                
            if(length(values) > 0) {
                sapply(1:length(values), function(ix) {self[[ fields[[ix]] ]] <- values[[ix]]})    
            }
            
            return(NULL)
        }
    )
    
    , private = list(
        values = new.env()
        , setFunction = function(value, setting) {
            if(missing(value)) {
                return(private$values[[setting]])
            }
            
            private$values[[setting]] <- value
            
            return(NULL)
        }

        , setPositiveNumeric = function(value, setting) {
            if(missing(value)) {
                return(private$values[[setting]])
            }
            
            if((is.numeric(value) || is.integer(value))
                && (value > 0)) {
                private$values[[setting]] <- value
            } else {
                    self$errorLogger(setting, "must be a number greater than zero") # assumes errorLogger is valid   
            }
            
            return(NULL)
        }
        
        , setString = function(value, setting) {
            if(missing(value)) {
                return(private$values[[setting]])
            }
            
            if(is.character(value) && length(value) == 1) {
                private$values$moduleBorderColor <- value
            } else {
                self$errorLogger(setting, "must be a single string") # assumes errorLogger is valid
            }
            
            return(NULL)
        }
    )
    
    , active = list(
        errorLogger = function(value) {
            return(private$setFunction(value, 'errorLogger'))
        }
        
        , infoLogger = function(value) {
            return(private$setFunction(value, 'infoLogger'))
        }
        
        , moduleMaxMemorySize = function(value) {
            return(private$setPositiveNumeric(value, 'moduleMaxMemorySize'))
        }
        
        , moduleBorderColor = function(value) {
            return(private$setString(value, 'moduleBorderColor'))
        }
        
        , moduleFillColor = function(value) {
            return(private$setString(value, 'moduleFillColor'))
        }
        
        , monitorRate = function(value) {
            return(private$setPositiveNumeric(value, 'monitorRate'))
        }
        
        , multipleConnectionsColor = function(value) {
            return(private$setString(value, 'multipleConnectionsColor'))
        }
        
        , singleConnectionColor = function(value) {
            return(private$setString(value, 'singleConnectionColor'))
        }

        , warningLogger = function(value) {
            return(private$setFunction(value, 'warningLogger'))
        }
    )
)$new() # instantiate to use active bindings instead of static fields / methods for error checking and because it looks cool
