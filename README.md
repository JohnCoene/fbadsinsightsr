![Developped by Cheetah Mobile](https://upload.wikimedia.org/wikipedia/en/f/f9/Cheetah_Mobile_Logo.png)

# README #

Current version: **2.0.1**

fbAdsInsightsR is an R package that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.5) as well as a few other API calls that facilitate the use of the package.

## Functions ##

**OAuth**

* `fbAuthenticate`

**GET-family**

* `getAccount`
* `getCampaign`
* `getAdset`
* `getAd`
* `getAny`

**Find-family**

* `findDatePreset`
* `findActionBreakdowns`
* `findFields`
* `findBreakdowns`
* `findInfo`
* `findAccounts`

See `?documentation` and examples for more details.

## Documentation ##

Constantly being updated and improved. Currently the following are available upon request:

* Manual - fbAdsInsightsR.pdf
* Vignette~ish - examples and broad desciption of package inner workings.

## Development ##

* Improve documentation, ~~build manual~~ (v2.0.0) and provide thorough examples.
* ~~Data loss fix - Variables parsed (columns) may vary~~ (v1.2.0)
* ~~Implement `paginate` argument.~~ (v1.1.0)
* ~~Fetch IDs of adsets, campaigns and accounts~~ (v1.1.1)


## Versions and Patches ##

#### v2.0.1 -  ####

* `findAccounts` function added; retrieves all account IDs accessible by either a business.facebook.com or a user. See updated Manual or ?documentation. 

Bug fixes.

* Fixed minor bug amongst GET-family functions where some errors on the inital queries where not caught if the response had little content.

### v2.0.0 - Documentation ###

* `paginate` argument has been changed in the GET-family functions to `n = 100`. It indicates the number of results desired rather than using a boolean argument for paging avoids---or rather allows one to control---lengthy queries.
* Documentation has also greatly been improved, i.e.: every function comes with examples.
* On a similar note, the manual has been released. Please ask a contributor for a copy.

Bug fixes.

* `token` appropriately used in `findInfo`, it should no longer return an error when using that which is returned by `fbAuthenticate`.
* `paginate` argument removed in favour of `n` (see documentation), it should no longer generate hour-long queries unless a *massive* amount of data is requested.

#### v1.2.0 - parseJSON ####

* ParseJSON fix - now returns all variables whatever the call, still imports `plyr::rbind.fill`
* Fixed `paginate = "next"` in `getCampaign`, `getAdset`, `getAd` and `getAny`. Changed to the more adequate (and mentioned in documentation) `paginate = NULL`

#### v1.1.2 - findInfo ####

* Function renamed for consistency from `getInfo` to `findInfo`.

#### v1.1.1 - getInfo ####

* Added `getInfo` to retrieve all camapaigns, adset and ads under an account!
* First draft of documentation available

#### v1.1.0 - Paginate ####

* Paginate function implemented see `paginate` parameter in GET-family functions (i.e.: `?getAny`).

#### v1.0.3 - Sort ####

* Find-family function return lists in alpha-numerical order, at the exception of `date_preset`.

#### v1.0.2 - Nomenclature ####

* Nomenclature changed in order to comply with the [conventions suggested by Haddly Wickham](http://r-pkgs.had.co.nz/style.html).
* `httr` moved from `DEPENDS` to `IMPORTS` to make te package more self-contained and avoid errors on install.

Functions names have thus changed from i.e.: `get_account` to `getAccount` while their arguements have changed from being underscore_separated to period.separated i.e.: `app_id` changed to `app.id`

Removed internal functions that were unused, will not affect package reader.

#### v1.0.1 - Token Check ####

* Fixed Token checking function (internal) - Should not return errors when using token returned by `fb_authneticate`.

### v1.0.0 - Release ###

* Initial release

## Install ##

Since access to the repository is restricted you will need your login and password to install the package.

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

## Chip in ##

* We're looking someone well-versed in both R and JSON, the data could definitely be parsed in a better way!
* Feedback and contributions most welcome.

## Contributors ##

* Admin - John Coene <john.coene@cmcm.com>
* Tester - DongYuNan <dongyunan@conew.com>