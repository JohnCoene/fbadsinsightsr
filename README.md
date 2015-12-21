# README #

Version 1.0 is out!

fbAdsInsightsR is an R pakcage that allows fetching data from the [facebook Ads Insights API](https://developers.facebook.com/docs/marketing-api/insights/v2.5).

### Vignette ###

Upon Request; contact any of the contributors.

### Development ###

* Pagination - enabling pagination (`next` and `previous`)
* Data loss fix - Variables parsed (columns) may vary.
* Fetch IDs of adsets, campaigns and accounts

### Versions ###

#### v1.0.3 ####

find-family function return lists in alpha-numerical order, at the exception of `date_preset`.

#### v1.0.2 ####

* Nomenclature changed in order to comply with the [conventions suggested by Haddly Wickham](http://r-pkgs.had.co.nz/style.html).
* httr switched from `DEPENDS` to `IMPORTS` to make te package more self-contained and avoid errors on install.

Functions names have thus changed from i.e.: `get_account` to `getAccount` while their arguements have changed from being underscore_separated to period.separated i.e.: `app_id` changed to `app.id`

Removed internal functions that were unused, will not affect package reader.

#### v1.0.1 ####

Fixed Token checking function (internal) - Should not return errors when using token returned by `fb_authneticate`.

#### v1.0.0 ####

* Initial release

### Install ###

`devtools::install_bitbucket("CMAnalytics/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

### Contributors ###

* Admin - John Coene <john.coene@cmcm.com>