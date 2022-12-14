set.seed(123)
mad <- mad2019[1:300,]
X <- dplyr::select(mad,-Y)
Y <- mad$Y

############## WITHOUT CF ###############

#Lasso
test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE, fitted_values = TRUE. We try
          only with Lasso", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     fitted_values = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel","FVs"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))
            expect_type(a$FVs, "double")
            expect_equal(length(a$FVs),length(Y))
          })


test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
#
#Ridge
test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
#RF
test_that("ML RF specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML RF specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML RF specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML RF specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#CIF
test_that("ML CIF specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML CIF specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CIF specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CIF specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#XGB
test_that("ML XGB specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML XGB specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML XGB specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML XGB specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#CB
test_that("ML CB specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML CB specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CB specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CB specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#SL

test_that("ML SL specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML SL specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML SL specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML SL specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     CFit = FALSE,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
#
#
# ############## WITH CF ###############

#Lasso
test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Lasso specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Lasso",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
#
#Ridge
test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML Ridge specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "Ridge",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
#RF
test_that("ML RF specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML RF specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML RF specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML RF specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "RF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
#CIF
test_that("ML CIF specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML CIF specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CIF specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CIF specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CIF",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#XGB
test_that("ML XGB specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML XGB specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML XGB specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML XGB specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "XGB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#CB
test_that("ML CB specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML CB specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CB specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML CB specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "CB",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })

#SL

test_that("ML SL specification has no error,
          gives expected output with IOp_rel TRUE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel","se"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })


test_that("ML SL specification has no error,
          gives expected output with IOp_rel FALSE
          and se TRUE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = TRUE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp","se"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML SL specification has no error,
          gives expected output with IOp_rel FALSE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = FALSE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_true(is.null(a$IOp_rel))
          })

test_that("ML SL specification has no error,
          gives expected output with IOp_rel TRUE
          and se FALSE", {
            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini", "MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini","MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini","MLD"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("Gini"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("Gini"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("Gini"))

            expect_no_error(a <- IOp(Y,
                                     X,
                                     est_method = "Debiased",
                                     ineq = c("MLD"),
                                     plugin_method = c("ML"),
                                     ML = "SL",
                                     sterr = FALSE,
                                     boots = 3,
                                     IOp_rel = TRUE))

            expect_type(a, "list")
            expect_equal(names(a),c("IOp","RMSE1","IOp_rel"))
            expect_type(a$IOp, "double")
            expect_equal(rownames(a$IOp), c("IOp"))
            expect_equal(colnames(a$IOp), c("MLD"))
            expect_type(a$RMSE1, "double")
            expect_equal(rownames(a$IOp_rel), c("IOp_rel"))
            expect_equal(colnames(a$IOp_rel), c("MLD"))
          })
