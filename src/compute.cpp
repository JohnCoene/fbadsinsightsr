#include <Rcpp.h>
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

//' Compute CVR
//' @param clicks Vector of clicks
//' @param installs Vector of installs
// [[Rcpp::export]]
NumericVector cvr(NumericVector clicks, NumericVector installs) {
  NumericVector cvr;
  cvr = installs / clicks;
  return cvr;
}

//' Compute CPI
//' @param installs Vector of installs
//' @param spend Vector of spend
// [[Rcpp::export]]
NumericVector cpi(NumericVector installs, NumericVector spend) {
  NumericVector cpi;
  cpi = spend / installs;
  return cpi;
}

//' Compute CPL
//' @param likes Vector of likes
//' @param spend Vector of spend
// [[Rcpp::export]]
NumericVector cpl(NumericVector likes, NumericVector spend) {
  NumericVector cpl;
  cpl = spend / likes;
  return cpl;
}

//' Compute CPL
//' @param actions Vector of actions
//' @param spend Vector of spend
// [[Rcpp::export]]
NumericVector cpa(NumericVector actions, NumericVector spend) {
  NumericVector cpa;
  cpa = spend / actions;
  return cpa;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
cvr(c(100, 1202, 123), c(50, 394, 5))
cvr(10, 1)
cpi(c(193, 122, 348), c(500, 452, 1981))
cpi(10, 1)
cpl(c(193, 122, 348), c(500, 452, 1981))
cpl(10, 1)
*/
