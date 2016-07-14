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

// [[Rcpp::export]]
String createFields(CharacterVector x) {
  int n = x.size();
  std::ostringstream y;
  
  for(int i = 0; i < n; ++i){
    if(i != n - 1){
      y << x[i] << "%2C";
    } else {
      y << x[i];
    }
  }
  
  return y.str();
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
createFields(c("ctr", "clicks", "impressions"))
createFields(c("ctr", "cpp"))
createFields("ctr")
*/
