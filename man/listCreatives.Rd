% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listCreatives.R
\name{listCreatives}
\alias{listCreatives}
\title{List creatives}
\usage{
listCreatives(id, token, n = 100, fields = "default", verbose = FALSE,
  limit = 100)
}
\arguments{
\item{id}{The id of the object you want to grab from (Required), see 
\href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.}

\item{token}{A valid token as returned by \code{\link{fbAuthenticate}} 
or a short-term token from 
\href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.}

\item{n}{Number of results to retrieve, defaults to \code{100}. 
When you make an API request, you will usually not receive all of the 
results of that request in a single response. This is because some 
responses could contain thousands of objects so most responses are 
paginated by default.}

\item{fields}{Defaults to \code{default} (\code{id} and \code{name}). 
See \code{findFields}}

\item{verbose}{Defaults to \code{FALSE} if \code{TRUE} will print information on the 
queries in the console.}

\item{limit}{Number of results requested at each API call, defaults to 
\code{100}.}
}
\value{
Returns IDs and URLs of all images contained in object 
(single row if ad.id is passed as arguement)
}
\description{
Fetches URL(s) name(s) and more about creatives used in an ad.
}
\details{
Calls 
\href{https://developers.facebook.com/docs/marketing-api/reference/adgroup/adcreatives/}{adcreatives}.
}
\examples{
\dontrun{
# authenticate
fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
                          app.secret = "16xx79321xx0130x2x10a08x3e2x30xx", 
                          scope = "ads_management")
# get account ids
act <- listAccounts(id = "me", token = fbOAuth)

# get all creatives in ad account
img_acc <- listCreatives(id = act[2,2], token = fbOAuth)

# get campaigns, adsets and ads IDs from account
ads <- grabAds(account.id = act[2,2], token = token = fbOAuth)

# get ad creatives
crea_ad <- listCreatives(id = sample(ads$id, 1), token = fbOAuth)
}

}
\seealso{
\code{\link{fbAuthenticate}}, \code{\link{listAccounts}},
 \code{\link{grabAds}}
}
\author{
John Coene \email{jcoenep@gmail.com}
}
