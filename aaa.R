set.seed(123)
X <- dplyr::select(mad2019,-Y)[1:1000,1:5]
 Y <- mad2019$Y[1:1000]



 iop_deb <- IOp(Y,
                X,
                est_method = "Debiased",
                CFit = TRUE,
                ctree_control(mincriterion = 0.9, minbucket = 50),
                ML = "CIF",
                sterr = TRUE,
                IOp_rel = TRUE,
                fitted_values = TRUE)
iop_deb$IOp


 iop_deb <- IOp(Y,
                X,
                est_method = "Debiased",
                CFit = TRUE,
                ctree_control(mincriterion = 0.9, minbucket = 50),
                ML = "CIF",
                sterr = TRUE,
                IOp_rel = TRUE,
                fitted_values = TRUE)
iop_deb$IOp

uu = iop_deb$FVsinfold

ML::MLest(
  X, Y,
  ML = "CIF",
  ntree = 50,
  control = partykit::ctree_control(
    mincriterion = 0.9,
    minbucket = 50
  )
)
