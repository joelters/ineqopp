set.seed(123)
mad <- mad2019[1:300,]
X <- dplyr::select(mad,-Y)
Y <- mad$Y

#LOGLINEAR PLUG IN

test_that("Loglin specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <-IOp(Y,
                                    X,
                                    est_method = "Plugin",
                                    ineq = c("Gini"),
                                    plugin_method = c("loglin"),
                                    sterr = TRUE,
                                    boots = 3,
                                    IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("Loglin specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("loglin"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("Loglin specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <-IOp(Y,
                                    X,
                                    est_method = "Plugin",
                                    ineq = c("Gini", "MLD"),
                                    plugin_method = c("loglin"),
                                    sterr = FALSE,
                                    boots = 3,
                                    IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("loglin"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("Loglin specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("loglin"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

test_that("Loglin specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE and fitted_values TRUE, we
          consider just ineq = c(Gini,MLD) for this,
          we are only checking whether fitted_values
          works correctly", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("loglin"),
                                     sterr = FALSE,
                                     fitted_values = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel","FVs"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))
            expect_type(a$FVs, "double")
            expect_equal(length(a$FVs), length(Y))
          })

#NP PLUG IN

test_that("NP specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("NP"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("NP"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("NP"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("NP specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("NP"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("NP"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("NP"),
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("NP specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <-IOp(Y,
                                    X,
                                    est_method = "Plugin",
                                    ineq = c("Gini", "MLD"),
                                    plugin_method = c("NP"),
                                    sterr = FALSE,
                                    boots = 3,
                                    IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("NP"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("NP"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("NP specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("NP"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("NP"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("NP"),
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

test_that("Loglin specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE and fitted_values TRUE, we
          consider just ineq = c(Gini,MLD) for this,
          we are only checking whether fitted_values
          works correctly", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("NP"),
                                     sterr = FALSE,
                                     fitted_values = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel","FVs"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))
            expect_type(a$FVs, "double")
            expect_equal(length(a$FVs), length(Y))
          })

#ML PLUG IN

#Lasso
test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

test_that("Loglin specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE and fitted_values TRUE, we
          consider just ineq = c(Gini,MLD) for this,
          we are only checking whether fitted_values
          works correctly. We test only with Lasso", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     fitted_values = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel","FVs"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))
            expect_type(a$FVs, "double")
            expect_equal(length(a$FVs), length(Y))
          })

#Ridge
test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

#RF
test_that("ML RF specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("ML RF specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML RF specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML RF specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

# CIF
test_that("ML CIF specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("ML CIF specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML CIF specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML CIF specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

#XGB
test_that("ML XGB specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("ML XGB specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML XGB specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML XGB specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

# CB
test_that("ML CB specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel","se"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })


test_that("ML CB specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp","se"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML CB specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML CB specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

#SL

test_that("ML SL specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_true(is.null(a[[2]]))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_true(is.null(a[[2]]))
          })

test_that("ML SL specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini","MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("Gini"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Plugin",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a[[1]], "double")
            expect_equal(rownames(a[[1]]), c("IOp"))
            expect_equal(colnames(a[[1]]), c("MLD"))
            expect_type(a[[2]], "double")
            expect_equal(rownames(a[[2]]), c("IOp_rel"))
            expect_equal(colnames(a[[2]]), c("MLD"))
          })

