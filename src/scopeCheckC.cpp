#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' TestScope
//' @param scope Scope to test
//' @param valid Valid scopes to test against
// [[Rcpp::export]]
void scopeCheckC(CharacterVector scope, CharacterVector valid) {
  
  int n = scope.size();
  
  for(int i = 0; i < n; ++i){
    if (std::find(valid.begin(), valid.end(), scope[i]) == valid.end()){
      Rf_error("Wrong permission. \nSee ?fbAuthenticate for valid values.");
    }
  }
}