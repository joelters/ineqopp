
rm(list = ls())
set.seed(123)
X <- dplyr::select(mad2019,-Y)
 Y <- mad2019$Y

# X[] <- lapply(X, function(x) {
#   if (is.character(x)) as.factor(x) else x
# })

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
fvsdf = iop_deb$FVsdf
plot(fvsdf$FVs, fvsdf$Y, xlab = "Fitted Values", ylab = "Observed Values", main = "Fitted vs Observed Values")
plot(fvsdf$FVsinsample, fvsdf$Y, xlab = "Fitted Values (In Sample)", ylab = "Observed Values", main = "Fitted vs Observed Values (In Sample)")
plot(fvsdf$FVsinsample, fvsdf$FVs, xlab = "Fitted Values (In Fold)", ylab = "Fitted Values", main = "Fitted vs Fitted Values (In Fold)")
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

e <- new.env()
load("data/mad2019.rda", envir = e)

mad2019 <- e$mad2019
mad2019[] <- lapply(mad2019, function(x) {
  if (is.character(x)) as.factor(x) else x
})
