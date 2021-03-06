% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAdset.R
\name{getAdset}
\alias{getAdset}
\title{Get data on an adset}
\usage{
getAdset(adset.id, token, fields = "default", n = 100,
  action.attribution.windows, action.breakdowns, action.report.time, breakdowns,
  date.preset, level, time.increment, time.range, summary = FALSE,
  verbose = FALSE, limit = 100)
}
\arguments{
\item{adset.id}{The id of the adset you want to retrieve (Required), see 
\href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.}

\item{token}{A valid token as returned by \code{\link{fbAuthenticate}} or a 
short-term token from 
\href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.}

\item{fields}{There are in total 73 valid fields defaults to 
(\code{default}) which returns the most popular ones. 
Run \code{\link{findFields}} to see all valid fields.}

\item{n}{Number of results to retrieve, defaults to \code{100}. 
When you make an API request, you will usually not receive all of the 
results of that request in a single response. 
This is because some responses could contain thousands of objects so 
most responses are paginated by default.}

\item{action.attribution.windows}{Determines what is the attribution window for the actions.
For example, \code{c("28d_click")} means the API returns all actions
 that happened 28 days after someone clicked on the ad (Optional).
  See details below for valid values.}

\item{action.breakdowns}{How to break down action results. Supports more than one breakdowns 
(Optional). Run \code{\link{findParams}} to see all 
valid action breakdowns.}

\item{action.report.time}{Determines the report time of action stats. 
For example, if a person saw the ad on Jan 1st but converted on Jan 2nd,
 when you query the API with \code{action.report.time="impression"}, 
 you will see a conversion on Jan 1st. When you query the API with 
 \code{action.report.time="conversion"}, you will see a conversion 
 on Jan 2nd (Optional).}

\item{breakdowns}{How to break down the result. Does not support more than one breakdown,
except \code{c("age", "gender")} and 
\code{"impression_device", "placement"}. The option 
\code{impression_device} cannot be used by itself (Optional). Run 
\code{\link{findParams}} to see all valid breakdowns.}

\item{date.preset}{Represents a relative time range (Optional). This field is ignored if 
\code{time.range} is specified. Run \code{\link{findParams}} 
to see all valid presets.}

\item{level}{Represents the level of result (Optional). Must be one  of \code{ad}, 
\code{adset}, \code{campaign}, \code{account}.}

\item{time.increment}{If it is an integer, it is the number of days from 1 to 90. 
After you pick a reporting period by using \code{time.range} or 
\code{date.preset}, you may choose to have the results for the whole
 period, or have results for smaller time slices. If "all_days" is used,
  it means one result set for the whole period. If "monthly" is used, 
  you will get one result set for each calendar month in the given period.
   Or you can have one result set for each N-day period specified by this
    param.}

\item{time.range}{time range must be \code{c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')}}

\item{summary}{Default value: false
Determine whether to return a summary section with the same fields as 
specified by fields will be included in the summary section.}

\item{verbose}{Defaults to \code{FALSE} if \code{TRUE} will print information on the 
queries in the console.}

\item{limit}{Number of results requested at each API call, defaults to 100.
Sometimes useful to bring it down if many results (\code{n}) are required as the 
API might otherwise return \code{error_code: 1} or in other words an
 "Unknown error".}
}
\description{
Get insights on advertising performances of an adset. See \href{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/insights/}{documentation} for more information
}
\details{
This function refers to the following API call \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/},
it is strongly encouraged to have a look a the latter link.
only the following parameters are not available \code{default_summary}, \code{filtering}, 
\code{summary}, \code{sort} and \code{time_ranges}.
}
\examples{
\dontrun{
# run authentication with your app details
fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
                          app.secret = "76xx79121xx0130x2x10a08x3e2x80xx", 
                          scope = "ads_management")
                          
# get information on account
adsets <- grabAdsets(account.id = "act_123456789012345", token = fbOAuth)

# take random ad.id
set.seed(123)
rand_id <- sample(adsets$id, 1)

# get date.preset
date <- findParams("date.preset")[grep("lifetime", findParams("date.preset"))]

# fetch Adset data broken down by region
data <- getAd(adset.id = rand_id, token = fbOAuth, 
date.preset = date, breakdowns = "region")
}

}
\seealso{
\code{\link{fbAuthenticate}}, \code{\link{findParams}},
 \code{\link{grabAdsets}}
}
\author{
John Coene \email{jcoenep@gmail.com}
}
