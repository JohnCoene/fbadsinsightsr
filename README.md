![Developped by Cheetah Mobile](https://upload.wikimedia.org/wikipedia/en/thumb/f/f9/Cheetah_Mobile_Logo.png/250px-Cheetah_Mobile_Logo.png)

# fbAdsInsightsR #

Current version: **v3.0** (*see git tags for older versions*)

fbAdsInsightsR is an R package that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.6). **Updated to Facebook Graph API v2.6!**.

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
    * `getLabCampaigns` (v3.0)
    * `getLabAdsets` (v3.0)
    * `getLabAds` (v3.0)
    * `getLabCreatives` (v3.0)
- List-family
    * `listBusinesses`
    * `listAccounts`
    * `listVideos`
    * `listImages`
    * `listCreatives`
    * `listApps` (v3.0)
    * `listAdApps` (v3.0)
    * `ListLabels` (v3.0)
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
    * `checkTargetSentence` (v3.0)
    * `checkTargetTree` (v3.0)
- Update-family
    * `updateCampaign` (v3.0)
    * `updateAdset` (v3.0)
    * `updateAd` (v3.0)
- Create-family
    * `createLabels` (v3.0)

## Documentation ##

Constantly being updated and improved. Currently the manual and examples are available in own repository (access NOT restricted) [here](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src).

* [Manual](https://bitbucket.org/JohnCheetah/fbadsinsightsr/downloads/fbAdsInsightsRv3_0.pdf)
* [Vignette](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src) **updated to v3.0!**

## Install ##

Since access to the repository is restricted you will need your login and password to install the package.

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

## Examples ##

```R
# run authentication with your app details
TK <- fbAuthenticate(app.id = "1234567890123456", app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", scope = c("ads_management", "ads_read"))
                           
# list facebook advertising accounts you have access to
accounts <- listAccounts(id = "me", token = TK)

# grab list of ads under random account
ads <- grabAds(sample(accounts$id, 1), TK)

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
                
# list creatives
creatives <- listCreatives(sample(accounts$id, 1), TK)

# check targeting of adset
adsets <- grabAdsets(sample(accounts$id, 1), TK)
(target <- checkTargetSentence(sample(adsets$id, 1), TK))

# check targeting from account
targeting <- checkTargetTree(sample(accounts$id, 1), TK)

# get labels
labels <- listLabels(accounts$id[1], TK)

# get ads that match ANY label
ad_labels <- getLabAds(accounts$id[1], labels = labs, operator = "ANY", token = TK)

# get campaigns that match ALL labels                        
ad_labels <- getLabAds(accounts$id[1], labels = labs, operator = "ALL", token = TK)
                      
(apps <- listApps(accounts$id[1], TK)) # list apps
(ad_apps <- listAdApps(accounts$id[1], TK)) # list advertiseable apps
```

## Versions and Patches ##

See NEWS.md for changes.

## Access & Contributors ##

Access to the package is restricted, email John Coene & GaoCong if you want to grant access to more users.

* John Coene <john.coene@cmcm.com> (Author & maintainer)
* DongYuNan <dongyunan@cmcm.com> (Tester)
* GaoCong <gaocong1@cmcm.com> (User)

*by Cheetah Mobile &copy;*