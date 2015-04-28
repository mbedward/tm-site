#include <Rcpp.h>
using namespace Rcpp;

// This is just to keep Rcpp happy during packing builds until we
// add some real sources.
// [[Rcpp::export]]
int foo(int x) {
   return x;
}
