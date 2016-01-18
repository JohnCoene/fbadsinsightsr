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
* `getImage`
* `getCreative`

**Find-family**

* `findFields`
* `findParams` (Replaces multiple find-family functions from v1.1)
* `findObjects` (.Deprecated)`

**Grab-family**

* `grabAccounts`
* `grabCampaigns`
* `grabAdsets`
* `grabAds`

**check-family**

* `checkTarget`
* `checkStatus`

See [documentation]((https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src)) and examples for more details.

## Documentation ##

Constantly being updated and improved. Currently the manual and examples are available in own repository (access NOT restricted) [here](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src).

## Install ##

Since access to the repository is restricted you will need your login and password to install the package.

`devtools::install_bitbucket("JohnCheetah/fbadsinsightsr", auth_user = "your_login", password = "your_password")`

## Development ##

* ~~Enable fetching ad images.~~ (v1.1)
* Enable fetching keywordstats: [https://developers.facebook.com/docs/marketing-api/reference/ad-keyword-stats](https://developers.facebook.com/docs/marketing-api/reference/ad-keyword-stats)
* ~~Deal with action-related variables which mess up data returned~~ (v1.1) ~~temporary fix~~ (v1.0).
* Clean return of `findTarget` (minor).
* ~~Document to explain workflow that package suggests.~~ (see [documentation]((https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src)))
* ~~Allow collecting data on ads' tageting specs, etc.~~ (v0.8)
* ~~Add optional feedback on GET-family functions.~~ (v0.8)
* ~~Improve documentation~~ (v1.1), ~~build manual~~ (v0.7) and provide thorough examples.
* ~~Data loss fix - Variables parsed (columns) may vary~~ (v0.6)
* ~~Implement `paginate` argument.~~ (v0.3)
* ~~Fetch IDs of adsets, campaigns and accounts~~ (v0.4)

## Versions and Patches ##

## v2.0 ##

Package overhauled; now internally functions on new S3 class for robustness, much optimisation and clean up of `utils.R`, package now split into 4 families of functions (`grab`, `check`, `find` and `get`), and much more.

See updated [documentation](https://bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src) for details.

### v1.1 ###

* Data parsing fixed at last. Now dataframe returned by GET-family functions properly displays `actions`, `unique_actions`, `cost_per_action_type`, `cost_per_unique_action_type` and `website_ctr` as respective variables (column names) regardless of parameters or fields passed to functions.
* The latter improvement means the `simplify` argument is now irrelevant and has thus been deprecated.
* Sleep when `n > 100` in GET-family functions and `findInfo` has been reduced from 3 seconds to 0.5 seconds.
* Overall clean up in `internal.R` speeds up some processes.
* `n` and `verbose` added to `findAccounts`.
* Modified `verbose = TRUE` output to something more sensible.
* `findStatus` function added, returns status of specific campaign, adset or ad.
* `findCampaigns`, `findAdsets` and `findAds` functions added.
* `findInfo` now call `findObjects`
* `getCreative` added to fetch ad Creatives from ads.
* Arguments of functions appear in correct order in order to ease simple queries.
* Check for presence of data and `warning` returned now more adequate
* Overall clean up of GET-family and find-family functions.

Bug Fixes

* `paginate` (internal funciton) no longer infinitely loops.
* `toDF` (internal function) properly parse missing data.
* typos corrected (mainly in examples)
* Number of results correctly returned when `verbose = TRUE` 
* fixed `toDF` (internal.R) - GET-family should no longer return errors.
* `verbose = TRUE` has been fixed where it was not being paste o nnew line when used inside a loop

### v1.0 ###

* Temporary fix to mess in return from GET-family functions; using `simplify = TRUE` in `getAny` will ignore the fields that cause the issue.
* Added `getImage` function to fetch URLs of images used by an ad account, adset, etc.
* `findAccounts` also returns account name.
* Updated manual

Bug Fixes:

* In GET-family functions added `stop` message when using region as `breakdowns` together with `action_carousel_card_id` and/or `action_carousel_card_name` as fields as it is not allowed by API and returned an error. The latter two fields have also been removed when using `simplify=TRUE`.
* Fixed error when querying little data using GET-family function, was caused by `stop` message when no data - has been replaced by `warning`.

### v0.9 ###

* Simplifies data returned by GET-famlily functions if possible, though rarely the case. This is first attempt at dealing with the actions-related data which produce confusing dataframes.
* `paginate` function (`utils.R`) to clean up GET-family functions.
* `verbose` arguement now also returns details on pagination.
* Some clean up in both find and GET family functions.
* Manual Updated.

Bug Fixes:

* Fixed bug in the recently added `findTarget` function where the presence of an argument broke the function - should no longer occur.
* Fixed `findTarget` where `NULL` was returned when few specifications were retrieved.
* `findTarget` now correctly returns targeting specs.
* `findAccount` is no longer "missing verbose"

### v0.8 ###

* Added `verbose` arguement to GET-family functions. See documentation.
* Added `findTarget` function to retrieve targeting specs of an ad or adset.
* `findAccounts` function added; retrieves all account IDs accessible by either a business.facebook.com or a user. See updated Manual or ?documentation. 

Bug fixes:

* Fixed minor bug amongst GET-family functions where some errors on the inital queries where not caught if the response had little content.

### v0.7 ###

* `paginate` argument has been changed in the GET-family functions to `n` and defaults to `100`. `n` indicates the number of results desired (rows) rather the previous boolean argument (`TRUE|FALSE`) avoids&mdash;or rather allows one to control&mdash;lengthy queries.
* Documentation has also greatly been improved, i.e.: every function comes with examples.
* On a similar note, the manual has been released. Please ask a contributor for a copy.

Bug fixes:

* `token` appropriately used in `findInfo`, it should no longer return an error when using that which is returned by `fbAuthenticate`.
* `paginate` argument removed in favour of `n` (see documentation), it should no longer generate hour-long queries unless a **massive** amount of data is requested.

### v0.6 ###

* ParseJSON fix - now returns all variables whatever the call, still imports `plyr::rbind.fill`
* Fixed `paginate = "next"` in `getCampaign`, `getAdset`, `getAd` and `getAny`. Changed to the more adequate (and mentioned in documentation) `paginate = NULL`

### v0.5 ###

* Function renamed for consistency from `getInfo` to `findInfo`.

### v0.4 ###

* Added `getInfo` to retrieve all campaigns, adset and ads IDs and names under an account!
* First draft of Examples available

### v0.3 ###

* Paginate function implemented see `paginate` parameter in GET-family functions (i.e.: `?getAny`).

### v0.2 ###

* Find-family functions now return vectors in alpha-numerical order, at the exception of `date_preset`.
* Nomenclature changed in order to comply with the [conventions suggested by Hadley Wickham](http://r-pkgs.had.co.nz/style.html). Functions names have thus changed from i.e.: `get_account` to `getAccount` while their arguements have changed from being underscore_separated to period.separated i.e.: `app_id` changed to `app.id`
* `httr` moved from `DEPENDS` to `IMPORTS` to make te package more self-contained and avoid errors on install.

Bug fixes:

* Removed internal functions that were unused, will not affect package reader.
* Fixed Token checking function (internal) - Should not return errors when using token returned by `fb_authneticate`.

### v0.1 ###

* Hello World

## Chip in ##

* We're looking someone well-versed in both R and JSON, the data could definitely be parsed in a better way!
* Feedback and contributions most welcome.\

## Chat Room ##

* A HipChat room is also available; ideal to keep up with developments, issues and&mdash;most importantly&mdash;other contributors.

## Contributors ##

* Admin - John Coene <john.coene@cmcm.com>
* Tester - DongYuNan <dongyunan@conew.com>