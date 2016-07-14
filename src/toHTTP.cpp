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

//' Build http
//' 
//' @param params Parameters
// [[Rcpp::export]]
String toHTTP(CharacterVector params) {
  int n = params.size();
  std::ostringstream fields;
  
  if(n > 1){
    for(int i = 0; i < n; ++i){
      if(i == 0){
        fields << "[%22" << params[i] << "%22%2C%20%22";
      } else if (i == n - 1){
        fields << params[i] << "%22]";
      } else {
        fields << params[i] << "%22%2C%20%22";
      }
      
    }
  } else {
    fields << "[%22" << params[0] << "%22]";
  }
  
  return fields.str();
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
toHTTP(c("ctr", "cvr", "impressions"))
toHTTP(c("ctr", "clicks"))
toHTTP(c("ctr"))
*/
