# NOTE: You need RTools to make TMB function!!!!
# Follow the online instructions here:
# https://github.com/kaskr/adcomp
require(TMB)
require(tmbstan)
require(shinystan)


# Write model
tmb_model = "
// Linear regression model
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Y);
  DATA_VECTOR(x);
  DATA_INTEGER(nondiff);
  PARAMETER(a);
  PARAMETER(b);
  PARAMETER(logSigma);

  if(nondiff == 1){ // Add non-differentiable aspect
    if(b > 3.2) {
      b = 3.15;
    }
  }

  Type nll = 0;
  for(int i=0; i<x.size(); i++)
    nll -= dnorm(Y[i], a + b * x[i], exp(logSigma), true);
  return nll;
}
"

## Write model to C++
write(tmb_model, file = "linreg.cpp")


## Load model template
compile("linreg.cpp") # Compile a C++ templated into a shared object file
dyn.load(dynlib("linreg"))


## Simulate data
set.seed(123)
beta <- 3
x <- seq(0, 10, length.out = 51)
parameters <- list(a=0, b=3.5, logSigma=0)

## Fit model with differentiable likelihood
data <- list(Y = rnorm(length(x)) + beta * x, x = x, nondiff = FALSE)
obj <- MakeADFun(data, parameters, DLL="linreg")
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt$par
obj$gr()
mcmc<- tmbstan(obj, iter=2000, chains=1, cores=1)
launch_shinystan(mcmc)

## Fit model with non-differentiable likelihood, start above
data2 <- list(Y = rnorm(length(x)) + beta * x, x = x, nondiff = TRUE)
obj2 <- MakeADFun(data2, parameters, DLL="linreg")
opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
opt2$par
obj2$gr()
mcmc2<- tmbstan(obj2, iter=2000, chains=1, cores=1)
launch_shinystan(mcmc2)

## Fit model with non-differentiable likelihood, start below
parameters$b <- 2.5
obj3 <- MakeADFun(data2, parameters, DLL="linreg")
opt3 <- nlminb(obj3$par, obj3$fn, obj3$gr)
opt3$par
obj3$gr(opt3$par)
mcmc3<- tmbstan(obj3, iter=2000, chains=1, cores=1)
