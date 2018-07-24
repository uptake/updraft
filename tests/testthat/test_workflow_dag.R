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
context("\n>> workflow_dag")

###################
# END-TO-END TESTS
###################
context("End-to-End Testing")

test_that("Testing DAGWorkflow Class has properly implemented inherited interfaces", {
    expect_true({
        updraft:::CheckInterfaceImplementation(DAGWorkflow)
        TRUE
    })
})

test_that("Testing DAGWorkflow Class constructor and obj methods run to completion", {
    expect_true({
        workflow <- DAGWorkflow$new()
        TRUE
    })
    for (i in 1:50) {
        module <- PackageFunctionModule$new(paste0("mod", i), 'paste') # dependency that PackageFunctionModule is working
        expect_true({
            workflow$addModules(module)
            TRUE
        })
    }
    for(i in 1:50) {
        headName <- paste0('mod', sample(1:50, 1))
        tailName <- paste0('mod', sample(1:50, 1))
        connection <- DirectedConnection$new(paste0("conn", i), headName, tailName, '...')
        expect_true({
            workflow$addConnections(connection)
            TRUE
        })
    }
    expect_true({
        workflow$getName()
        TRUE
    })
    expect_true({
        workflow$visualize()
        TRUE
    })
    expect_true({
        workflow$save(file.path(workingDir, "test.json"))
        TRUE
    })
    expect_true({
        workflow$getAllModules()
        TRUE
    })
    expect_true({
        workflow$getStartingModules()
        TRUE
    })
    expect_true({
        workflow$getEndingModules()
        TRUE
    })
    expect_true({
        workflow$getDownstreamModules('mod1')
        TRUE
    })
    expect_true({
        workflow$getUpstreamModules('mod2')
        TRUE
    })
    expect_true({
        workflow$getConnections('mod1', 'mod2')
        TRUE
    })
    expect_true({
        workflow$getModuleInputs('mod2')
        TRUE
    })
    expect_true({
        workflow$getWorkflowInputs()
        TRUE
    })
    removeConnections = paste0("conn", sample(1:50, 10))
    expect_true({
        workflow$removeConnection(removeConnections[1:9])
        TRUE
    })
    expect_true({
        workflow$removeConnection(removeConnections[10])
        TRUE
    })
    
    removeModules = paste0("mod", sample(1:50, 10))
    expect_true({
        workflow$removeModule(removeModules[1:9])
        TRUE
    })
    expect_true({
        workflow$removeModule(removeModules[10])
        TRUE
    })
})

test_that("Testing DAGWorkflow Class Static initFromFile Method", {
    expect_true({
        workflow <- DAGWorkflow$initFromFile(file.path(testInputDir, 'test_dag_workflow.json'))
        TRUE
    })

    expect_true({
        workflow <- WorkflowInterface$initFromFile(file.path(testInputDir, 'test_dag_workflow.json'))
        TRUE
    })
})

test_that("Testing DAGWorkflow obj methods error when appriopriate", {
    workflow <- DAGWorkflow$new()
    connection <- DirectedConnection$new("conn", "mod1", "mod2", '...')
    mod1 <- PackageFunctionModule$new("mod1", "paste")
    mod2 <- PackageFunctionModule$new("mod2", "paste")
    
    expect_error({
        workflow$addConnections(1.0)
    }, regexp = 'connection parameter')
    expect_error({
        workflow$addConnections("test")
    }, regexp = 'connection parameter')
    expect_error({
        workflow$addModules(1.0)
    }, regexp = 'modules parameter')
    expect_error({
        workflow$addModules("test")
    }, regexp = 'modules parameter')
    expect_error({
        workflow$removeConnection(1.0)
    }, regexp = 'connection parameter')
    expect_error({
        workflow$removeConnection("test")
    }, regexp = 'connection parameter')
    expect_error({
        workflow$removeModule(1.0)
    }, regexp = 'module parameter')
    expect_error({
        workflow$removeModule("test")
    }, regexp = 'module parameter')
    expect_error({
        workflow$removeModule(mod1)
    }, regexp = 'module parameter')
})

test_that("Testing DAGWorkflow static class methods error when appriopriate", {
    expect_error({
        DAGWorkflow$initFromFile(file.path(testInputDir, "test_dag_workflow_corrupted.json"))
    }, regexp = 'corrupted')
    expect_error({
        WorkflowInterface$initFromFile(file.path(testInputDir, "test_dag_workflow_corrupted.json"))
    }, regexp = 'corrupted')
    expect_error({
        DAGWorkflow$initFromFile(file.path(testInputDir, "does_not_exist.json"))
    }, regexp = 'corrupted')
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
test_that("Testing DAGWorkflow obj methods return values", {
    workflow <- DAGWorkflow$new(name = "test_name")
    module1 <- PackageFunctionModule$new("module1", 'paste')  # dependency that PackageFunctionModule is working
    module2 <- PackageFunctionModule$new("module2", 'paste')  # dependency that PackageFunctionModule is working
    connection <- DirectedConnection$new("conn", module1, module2, '...')  # dependency that DirectedConnection is working
    workflow$addModules(list(module1, module2))
    workflow$addConnections(connection)
    expect_equal({
        workflow$getName()
    }, "test_name")
    expect_equal({
        length(workflow$getAllModules())
    }, 2)
    expect_equal({
        length(workflow$getConnections(module1, module2))
    }, 1)
    expect_equal({
        length(workflow$getConnections(module2, module1))
    }, 0)
    expect_equal({
        length(workflow$getUpstreamModules(module2))
    }, 1)
    expect_equal({
        length(workflow$getUpstreamModules(module1))
    }, 0)
    expect_equal({
        length(workflow$getDownstreamModules(module1))
    }, 1)
    expect_equal({
        length(workflow$getDownstreamModules(module2))
    }, 0)
    expect_equal({
        length(workflow$getEndingModules())
    }, 1)
    expect_equal({
        length(workflow$getStartingModules())
    }, 1)
    expect_equal({
        workflow$getModuleInputs(module2)
    }, c(sep = FALSE, collapse = FALSE))
    expect_equal({
        workflow$getWorkflowInputs()
    }, c(`...` = FALSE, sep = FALSE, collapse = FALSE))
})


#########################
# CLEAN UP OUTPUT FILES.
#########################
# Rm output directory.
outputDirs <- list.dirs(file.path(workingDir), recursive = FALSE)
unlink(outputDirs[grepl("output", outputDirs)], recursive = TRUE)

# Rm misc data files.
outputFiles <- list.files(workingDir, full.names = TRUE)
unlink(outputFiles[grepl("test.json", outputFiles)])


##################
# FINALLY...
##################

# Check that nothing is in the output folder.
context("Unittest Output Directory Check")



test_that("Output directory is empty.", expect_equal(length(list.files(workingDir)), 0))

