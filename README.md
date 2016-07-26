[comment]: <> ([![Travis-CI Build Status](https://travis-ci.org/.svg?branch=master)](https://travis-ci.org/)[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/johncheetah/fbadsinsightsr?branch=master&svg=true)](https://ci.appveyor.com/project/johncheetah/fbadsinsightsr)[![Coverage Status](https://img.shields.io/codecov/c/github//master.svg)](https://codecov.io/github/?branch=master)[![Coverage Status](https://img.shields.io/coveralls/.svg)](https://coveralls.io/r/?branch=master))
![Developped by Cheetah Mobile](http://www.mobyaffiliates.com/wp-content/uploads/2015/10/12.png)

# fbAdsInsightsR #

Current version: **v3.1** (*see "Developments" section below*)

fbAdsInsightsR is an R package that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.7). 
**Now updated to Facebook Graph API v2.7!**

## Functions ##


- OAuth
    * `fbAuthenticate`
- GET-family
    * `getAccount`
    * `getCampaign`
    * `getAdset`
    * `getAd`
    * `getAny`
    * `getActivity`
    * `getLabCampaigns` 
    * `getLabAdsets`
    * `getLabAds`
    * `getLabCreatives`
    * `getEstreach` (dev)
- List-family
    * `listBusinesses`
    * `listAccounts`
    * `listVideos`
    * `listImages`
    * `listCreatives`
    * `listApps`
    * `listAdApps` 
    * `ListLabels` 
- Find-family
    * `findFields`
    * `findParams`
- Grab-family
    * `grabCampaigns`
    * `grabAdsets`
    * `grabAds`
    * `grabCreatives` (.deprecated for `listCreatives`)
- Check-family
    * `checkTarget` (.deprecated for `checkTargetSentence`)
    * `checkStatus`
    * `checkTargetSentence` 
    * `checkTargetTree`
    * `checkUsers` (dev)
- Update-family
    * `updateCampaign`
    * `updateAdset` 
    * `updateAd`
- Create-family
    * `createLabels` 

## Documentation ##

Constantly being updated and improved. Currently the manual and examples are available in own repository (access NOT restricted) [here](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src).

* [Manual](https://bitbucket.org/JohnCheetah/fbadsinsightsr/downloads/fbAdsInsightsRv3_0.pdf)
* [Vignette](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src)

## Install ##

Since access to the repository is restricted you will need your login and password to install the package.

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password", ref = "Rcpp")`

## Examples ##

```R
# run authentication with your app details
TK <- fbAuthenticate(app.id = "1234567890123456", app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", scope = c("ads_management", "ads_read"))
                           
accounts <- listAccounts(id = "me", token = TK) # list facebook advertising accounts you have access to

ads <- grabAds(sample(accounts$id, 1), TK) # grab list of ads under random account

# get data on random ad
set.seed(19880525)
ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK)

# get daily performance data for last month with a summary
my_preset <- findParams("date.preset")[grep("last_month", findParams("date.preset"))]
ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK, n = 1000, date.preset = my_preset, time.increment = 1)
                
# get ad performance data by country 
ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK, breakdowns = "country")
                  
# get ad performance data by age and gender 
ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK, breakdowns = c("age", "gender"))
                
creatives <- listCreatives(sample(accounts$id, 1), TK) # list creatives

# check targeting of adset
adsets <- grabAdsets(sample(accounts$id, 1), TK)
(target <- checkTargetSentence(sample(adsets$id, 1), TK))

targeting <- checkTargetTree(sample(accounts$id, 1), TK) # check targeting from account

labels <- listLabels(accounts$id[1], TK) # get labels

# get ads that match ANY label
ad_labels <- getLabAds(accounts$id[1], labels = labs, operator = "ANY", token = TK)

# get campaigns that match ALL labels                        
ad_labels <- getLabAds(accounts$id[1], labels = labs, operator = "ALL", token = TK)
                      
(apps <- listApps(accounts$id[1], TK)) # list apps
(ad_apps <- listAdApps(accounts$id[1], TK)) # list advertiseable apps
```

## Developments ##

### Versions ###

See Git tags: *2.0*, *v3.0* and *v3.1* (Rcpp branch)

### Dev ###

- New functions: `checkUsers`, `getEstReach` 
- `listAccounts` id parameter defaults to `me`
- fixed return of `getAny` FUN where it sometimes would not return results when few fields were requested, also fixed sometimes returning *weirdly* named variables.
- Get-family functions now compute various ratios CVR, CPI, CPL, and CPA.
- `breakdown` parameter `placement_rhc` has been removed in API v2.7

See NEWS.md for entire changelog.

### Branches ###

- **master** - Last release or dev version
- **Rcpp** - Uses C++ to speed up certain functions

## R CMD check

R CMD check results
0 errors | 0 warnings | 0 notes

## Access & Contributors ##

Access to the package is restricted, email John Coene & GaoCong if you want to grant access to more users.

* John Coene <john.coene@cmcm.com> (Author & maintainer)
* DongYuNan <dongyunan@cmcm.com> (User & tester)
* GaoCong <gaocong1@cmcm.com> (User)

---------------------------------------------------------------------------

by Cheetah Mobile - 2016