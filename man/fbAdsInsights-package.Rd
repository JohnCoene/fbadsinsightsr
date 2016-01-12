\name{fbAdsInsightsR-package}
\alias{fbAdsInsightsR-package}
\alias{fbAdsInsightsR}
\docType{package}
\title{
  \packageTitle{fbAdsInsightsR}
}
\description{
  \packageDescription{fbAdsInsightsR}
}
\details{
  
  The DESCRIPTION file:
    \packageDESCRIPTION{fbAdsInsightsR}
  \packageIndices{fbAdsInsightsR}
}
\author{
  \packageAuthor{fbAdsInsightsR}
  
  Maintainer: \packageMaintainer{fbAdsInsightsR}
}
\references{
  \href{https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src}{Documentation} is freely available online please see respective README for instructions.
}
\keyword{ package }
\seealso{
  Package makes heavy use of \code{\link[httr:httr-package]{GET}} for http
  requests as well as \code{\link[rjson:rjson-package]{fromJSON}} and \code{\link[plyr:plyr-package]{rbind.fill}} to parse the data.
}
\examples{
\dontrun{
# run authentication with your app details
OAUTH <- fbAuthenticate("1234567890123456", 
                        "76xx79121xx0130x2x10a08x3e2x80xx",
                        scope = "ads_management",
                        app.name = "myApp")
    
# grab ads on account
ads <- grabAds("act_123456789012345", OAUTH, fields = "default",
               "date_start")

# filter ads by date
ads <- ads[as.Date(ads$insights_date_start) >= as.Date("2016-01-01"),]
    
# grab adsets on account
adsets <- grabAdsets(id = "act_123456789012345", token = OAUTH)
    
# grab ads on account using temporary token (see @param) - fetching ALL fields
camp <- grabCampaigns(id = "act_123456789012345", token = "XXXXXXXXXXXXXXX",
                      fields = findFields("findCampaigns"))
    
# find valid date.preset (this quarter)
date <- findParams("date.preset")[grep("quarter", findParams("date.preset"))]

# set seed for reproducability
set.seed(19880525)

# loop through ads to find active one (to then get data)
i <- 1

while (status != "ACTIVE"){
  
  # check status
  status <- whatStatus(ads$id[i], OAUTH)[,effective_status]
  
  # iterate
  i <- i + 1
  
  # stop if all ads done
  if(i == nrow(ads)) stop("No ACTIVE ads found", call. = F)
}

# exctract active ad
active_ad <- ad[i,]
    
# fetch Ad data
dat <- getAny(id = , token = OAUTH,
              date.preset = date, time.increment = 1)

# find breakdown (region)
reg <- findParams("breakdowns")[grep("region", findParams("breakdowns"))]

# fetch Adset data
dat <- getAny(id = sample(adsets$id, 1), token = OAUTH,
              time.range= c(since = "2015-01-01", until = "2016-01-01"),
              time.increment = 1, breakdowns = reg, n = 500)
              
# find breakdown (age and gender)
age_gen <- findParams("breakdowns")[grep("^age$|^gender$", 
                      findParams("breakdowns"))]
    
# fetch Campaign data
dat <- getAny(id = sample(campaigns$id, 1), token = OAUTH,
              date.preset = date, breakdowns = age_gen)
    
# fetch Account data
dat <- getAny(id = "act_123456789012345", token = OAUTH, 
              date.preset = "lifetime", fields = "ctr")
}
}
