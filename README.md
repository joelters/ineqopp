PLEASE READ: The package is under development please notify any errors. Ensemble
methods and SuperLearner methods still not developed, they take too long hence they
are not maintained and will give errors. 

The goal of ineqopp is to  provide estimates and tests for Inequality of Opportunity (IOp)
using a Debiased estimator. Debiased IOp estimates can be computed using any Machine
Learner among: Lasso, Ridge, Random Forest, Conditional Inferest Forest, Extreme
Gradient Boosting, Catboosting, Neural networks, log linear regression, Nonlinear
Least squares with exp(x'b).
Plug in estimates can also be computed estimating with any of the aforementioned
machine learners but we advise that there is no valid inferential theory for plug-in
estimators. Debiased partial effects where the difference of IOp with all circumstances
vs IOp without some circumstances is computed. Also IOp difference tests 
among independent populations can also be implemented. The package includes
a survey with income and circumstances for Madrid in 2018 
(from Encuesta de Condiciones de vida (ECV) 2019). For ML based IOp estimation it 
is important that the ML estimation is of good quality, hence we recommend doing
cross-validation to choose hyperparameters (for instance with the ML package 
https://github.com/joelters/ML)

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
There are four main functions: IOp, peffect and IOptest. IOp provides IOp estimates,
peffect provides debiased IOp partial effects and IOptest tests for equality
of IOp between two populations. See the documentation of these packages for examples
of their use.

For more info  and examples install the package and see the
documentation of the functions. 

