#include <Rcpp.h>
#include <algorithm>
#include <string>
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

//' createFields
//' 
//' @param x fields
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

//' scopeCheck
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

//' buildBreakdowns
//' @param breakdowns Breakdowns
//' @param f testParam
// [[Rcpp::export]]
String buildBreakdowns(CharacterVector breakdowns, Function f){
  
  int n = breakdowns.size();
  std::ostringstream br;
  std::string http;
  String x, y, age = "age", gender = "gender", impression = "impression_device", 
    place = "placement";
  
  f("breakdowns", breakdowns);
  
  http = toHTTP(breakdowns);
  
  if(n == 1){
    
    x = as<std::string>(breakdowns[0]);
    
    if(x == impression){
      stop("impression_device cannot be used on its own");
    } else {
      br << "&breakdowns=" << http;
    }
    
  } if (n == 2){
    
    x = as<std::string>(breakdowns[0]);
    y = as<std::string>(breakdowns[1]);
    
    if(x == age && y == gender){
      br << "&breakdowns=" << http;
    } else if (x == impression && y == place){
      br << "&breakdowns=" << http;
    }
    
  } else if (n >= 3){
    stop("Too many breakdowns specified. See @param");
  }
  
  return br.str();
}

//' parseP
//' @param param Parameter to parse
// [[Rcpp::export]]
CharacterVector parseP(CharacterVector param){
  
  int nParam = param.size();
  
  CharacterVector newP(nParam);
  
  std::string p;
  
  // gsub
  for(int i = 0; i < nParam; ++i){
    p = as<std::string>(param[i]);
    std::replace(p.begin(), p.end(), '_', '.');
    newP[i] = p;
  }
  
  return newP;
}

//' get options
//' @param params parameters to test
//' @param fp \code{\link{findParams}}
//' @param ff \code{\link{findFields}}
//' @param fct Function passed from parent FUN
//[[Rcpp::export]]
CharacterVector optIt(String params, Function fp, Function ff, 
                      String fct = "getAny"){
  
  CharacterVector options;
  String fields = "fields", art = "action.report.time", lv = "level";
  
  if (params == fields) {
    options = ff(fct);
  } else if (params == art){
    options = CharacterVector::create("impression", "conversion");
  } else if (params == lv){
    options = CharacterVector::create("ad", "adset", "campaign", "account");
  } else {
    options = fp(params);
  }
  
  return options;
}
