set.seed(123)
mad <- mad2019[1:250,]
X <- dplyr::select(mad,-Y)
Y <- mad$Y
circs <- c("educM","educF")
iop <- IOp(Y,
           X,
           est_method = "Debiased",
           CFit = TRUE,
           ineq = c("Gini","MLD"),
           plugin_method = c("ML"),
           ML = "Lasso",
           sterr = FALSE,
           boots = 2,
           IOp_rel = TRUE,
           fitted_values = TRUE)
FVs <- iop$FVs
iop_gini <- iop$IOp["IOp","Gini"]
iop_mld <- iop$IOp["IOp","MLD"]

test_that("Partial effects without parallelizing work,
          pe_rel TRUE, Gini and MLD", {
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("Gini","MLD"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = TRUE,
                                         parallel = FALSE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("Gini","MLD"))
            expect_type(a[[1]]$PE_rel,"double")
            expect_equal(rownames(a[[1]]$PE_rel),c("PE_rel","se"))
            expect_equal(colnames(a[[1]]$PE_rel),c("Gini","MLD"))
          })

test_that("Partial effects without parallelizing work,
          pe_rel TRUE, Gini", {
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("Gini"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = TRUE,
                                         parallel = FALSE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("Gini"))
            expect_type(a[[1]]$PE_rel,"double")
            expect_equal(rownames(a[[1]]$PE_rel),c("PE_rel","se"))
            expect_equal(colnames(a[[1]]$PE_rel),c("Gini"))
          })

test_that("Partial effects without parallelizing work,
          pe_rel TRUE, MLD", {
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("MLD"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = TRUE,
                                         parallel = FALSE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("MLD"))
            expect_type(a[[1]]$PE_rel,"double")
            expect_equal(rownames(a[[1]]$PE_rel),c("PE_rel","se"))
            expect_equal(colnames(a[[1]]$PE_rel),c("MLD"))
          })

test_that("Partial effects without parallelizing work,
          pe_rel FALSE, Gini and MLD", {
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("Gini","MLD"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = FALSE,
                                         parallel = FALSE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("Gini","MLD"))
            expect_true(is.null(a[[1]]$PE_rel))
          })

test_that("Partial effects without parallelizing work,
          pe_rel FALSE, Gini", {
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("Gini"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = FALSE,
                                         parallel = FALSE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("Gini"))
            expect_true(is.null(a[[1]]$PE_rel))
          })

test_that("Partial effects without parallelizing work,
          pe_rel FALSE, MLD", {
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("MLD"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = FALSE,
                                         parallel = FALSE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("MLD"))
            expect_true(is.null(a[[1]]$PE_rel))
          })

test_that("Partial effects with parallelizing work",{
            expect_no_error(a <- peffect(Y,
                                         X,
                                         circs,
                                         FVs = FVs,
                                         ineq = c("Gini","MLD"),
                                         ML = "Lasso",
                                         K = 5,
                                         iop_gini = iop_gini,
                                         iop_mld = iop_mld,
                                         pe_rel = TRUE,
                                         parallel = TRUE))
            expect_type(a, "list")
            expect_equal(length(a),length(circs))
            expect_equal(names(a),circs)
            expect_type(a[[1]],"list")
            expect_type(a[[1]]$PE,"double")
            expect_equal(rownames(a[[1]]$PE),c("PE","se"))
            expect_equal(colnames(a[[1]]$PE),c("Gini","MLD"))
            expect_type(a[[1]]$PE_rel,"double")
            expect_equal(rownames(a[[1]]$PE_rel),c("PE_rel","se"))
            expect_equal(colnames(a[[1]]$PE_rel),c("Gini","MLD"))
          })

