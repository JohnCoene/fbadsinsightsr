![Developped by Cheetah Mobile](https://upload.wikimedia.org/wikipedia/en/f/f9/Cheetah_Mobile_Logo.png)

# fbAdsInsightsR #

Current version: **v2.0**

fbAdsInsightsR is an R package that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.5).

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

**List-family**

* `listBusinesses`
* `listAccounts`
* `listVideos`
* `listImages`
* `listCreatives`

**Find-family**

* `findFields`
* `findParams` (Replaces multiple find-family functions from v1.1)
* `findObjects` (.Deprecated)`

**Grab-family**

* `grabCampaigns`
* `grabAdsets`
* `grabAds`
* `grabCreatives` (deprecated)

**check-family**

* `checkTarget`
* `checkStatus`

See [documentation]((https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src)) and examples for more details.

## Documentation ##

Constantly being updated and improved. Currently the manual and examples are available in own repository (access NOT restricted) [here](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src).

## Install ##

Since access to the repository is restricted you will need your login and password to install the package.

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

## Versions and Patches ##

See NEWS.md for changes.

## Contributors ##

* Admin - John Coene <john.coene@cmcm.com>
* Tester - DongYuNan <dongyunan@conew.com>