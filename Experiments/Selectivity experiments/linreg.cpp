
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

