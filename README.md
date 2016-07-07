![Developped by Cheetah Mobile](https://upload.wikimedia.org/wikipedia/en/f/f9/Cheetah_Mobile_Logo.png)

*Updated to Facebook Graph API v2.6*

# fbAdsInsightsR #

Current version: **v3.0**

fbAdsInsightsR is an R package that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.6).

## Functions ##

**OAuth**

* `fbAuthenticate`

**GET-family**

* `getAccount`
* `getCampaign`
* `getAdset`
* `getAd`
* `getAny`
* `getActivity`
* `getLabels` (v3.0)
* `getLabCampaigns` (v3.0)
* `getLabAdsets` (v3.0)
* `getLabAds` (v3.0)

**List-family**

* `listBusinesses`
* `listAccounts`
* `listVideos`
* `listImages`
* `listCreatives`
* `listApps` (v3.0)
* `listAdApps` (v3.0)

**Find-family**

* `findFields`
* `findParams`

**Grab-family**

* `grabCampaigns`
* `grabAdsets`
* `grabAds`
* `grabCreatives` (.deprecated)

**check-family**

* `checkTarget` (.deprecated)
* `checkStatus`
* `checkTargetSentence` (v3.0)
* `checkTargetTree` (v3.0)

**create-family**

* `createLabels` (v3.0)

## Documentation ##

Constantly being updated and improved. Currently the manual and examples are available in own repository (access NOT restricted) [here](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src).

* [Manual](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/downloads/fbAdsInsightsRv2_0.pdf)
* [Vignette](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src)

## Install ##

Since access to the repository is restricted you will need your login and password to install the package.

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

## Versions and Patches ##

See NEWS.md for changes.

## Contributors ##

* Admin - John Coene <john.coene@cmcm.com>
* Tester - DongYuNan <dongyunan@conew.com>