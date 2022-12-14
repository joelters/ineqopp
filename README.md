The goal of ineqopp is to  provide estimates and tests for Inequality of Opportunity (IOp)
using several methods. Debiased IOp estimates can be computed using any Machine
Learner among: Lasso, Ridge, Random Forest, Conditional Inferest Forest, Extreme
Gradient Boosting, Catboosting or some combination of these five machine learners.
Plug in estimates can also be computed estimating the fitted values with a log
linear regression, non-parametrically (for discrete circumstances)
or with any of the aforementioned machine learners. Debiased partial effects can
also be computed. The package includes a survey with income and circumstances for
Madrid in 2018 (from Encuesta de Condiciones de vida (ECV) 2019).

## Installation

You can install the development version of ineqopp from [GitHub](https://github.com/) with:
      
``` r
# install devtools if not installed
install.packages("devtools")
# First install ML from github (if not installed)
devtools::install_github("joelters/ML")
# Then install the ineqopp package
devtools::install_github("joelters/ineqopp")
```
There are four main functions, IOp, peffect, IOptest and IOpgrouptest. IOp provides IOp estimates, peffect provides debiased IOp partial effects, IOptest tests for equality of IOp between two populations
and IOpgrouptest tests joint significance of circumstances. Here are some examples

``` r
mad <- mad2019[1:250,]
X <- dplyr::select(mad,-Y)
Y <- mad$Y

#IOp
iop <- IOp(Y,
           X,
           est_method = "Debiased",
           CFit = TRUE,
           ineq = c("Gini","MLD"), 
           plugin_method = c("ML"),
           ML = "CIF",
           sterr = FALSE,
           boots = 2,
           IOp_rel = TRUE,
           fitted_values = TRUE)
FVs <- iop$FVs
iop_gini <- iop$IOp["IOp","Gini"]
iop_mld <- iop$IOp["IOp","MLD"]


#Partial effects
circs <- c("educM","educF")
pe <- peffect(X,
              Y,
              circs,
              FVs = FVs,
              ineq = c("Gini","MLD"),
              ML = "Lasso",
              K = 5,
              iop_gini = iop_gini,
              iop_mld = iop_mld,
              pe_rel = TRUE,
              parallel = FALSE)


# Difference in IOp test              
A <- dplyr::filter(mad2019, sex == "Female")
B <- dplyr::filter(mad2019, sex == "Male")
XA <- dplyr::select(A,-Y)
YA <- A$Y
XB <- dplyr::select(B,-Y)
YB <- B$Y
iopA <- IOp(YA,
            XA,
            est_method = "Debiased",
            CFit = TRUE,
            ineq = "Gini",
            plugin_method = c("ML"),
            ML = "XGB",
            sterr = TRUE)
iopB <- IOp(YB,
            XB,
            est_method = "Debiased",
            CFit = TRUE,
            ineq = "Gini",
            plugin_method = c("ML"),
            ML = "XGB",
            sterr = TRUE)
iop_a <- iopA$IOp["IOp","Gini"]
se_a <- iopA$IOp["se","Gini"]
iop_b <- iopB$IOp["IOp","Gini"]
se_b <- iopB$IOp["se","Gini"]
IOptest(iop_a, iop_b, se_a, se_b)


# Joint significance test
circs <- c("educM","educF")
iop <- IOp(Y,
           X,
           est_method = "Debiased",
           CFit = TRUE,
           ineq = c("Gini","MLD"),
           plugin_method = c("ML"),
           ML = "RF",
           sterr = FALSE,
           fitted_values = TRUE)
FVs <- iop$FVs
iop_gini <- iop$IOp["IOp","Gini"]
iop_mld <- iop$IOp["IOp","MLD"]
Gtest <- IOpgrouptest(Y,
                      X,
                      FVs = FVs,
                      ineq = c("Gini","MLD"),
                      ML = "RF",
                      K = 5,
                      iop_gini = iop_gini,
                      iop_mld = iop_mld,
                      circs = circs)
Gtest$Gini
Gtest$MLD
```
For more info  and examples install the package and see the documentation of the functions with
?IOp, ?peffect. For now it is not possible to change the tuning parameters.
For the moment I suggest using the trace() function to change the tuning parameters in the 
modest function of the ML package (or in wrappers_SL if you are using the SuperLearner)
(see [here](https://stackoverflow.com/questions/34800331/r-modify-and-rebuild-package)).

