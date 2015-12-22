# README #

Current version: **1.2.0**

fbAdsInsightsR is an R package that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.5).

## Functions ##

OAuth.
`fb_authenticate`

GET-family.
`getAccount`
`getCampaign`
`getAdset`
`getAd`
`getAny`

Find-family.
`findDatePreset`
`findActionBreakdowns`
`findFields`
`findBreakdowns`

See `?documentation` for more details

## Documentation ##

Currently being written given upon request; contact any of the contributors.

## Development ##

* Improve documentation, build vignette and provide thorough examples.
* ~~Data loss fix - Variables parsed (columns) may vary.~~
* ~~Implement `paginate` argument.~~ (v1.1.0)
* ~~Fetch IDs of adsets, campaigns and accounts~~ (v1.1.1)


## Versions and Patches ##

#### v1.2.0 - parseJSON ####

* ParseJSON fix - now returns all variables whatever the call, still imports `plyr::rbind.fill`
* Fixed `paginate = "next"` in `getCampaign`, `getAdset`, `getAd` and `getAny`.

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

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

## Contributors ##

* Admin - John Coene <john.coene@cmcm.com>