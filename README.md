## monkeyreadR

The data tables output by SurveyMonkey are not tidy: The headings in the first row are often missing, and the second row contains additional data and sub_headings, which are often redundant but sometimes required. Furthermore, there are often columns included in the output by default that contain no useful information. 

monkeyreadR is currently a one-function package: `read_sm()` is a wrapper around `vroom::vroom()` which cleans names, drops unwanted columns and jettisons the first row of the table. It goes something like this:

`read_sm("my-sm-data.csv", clean_names = TRUE, drop_surplus_cols = TRUE)`

`clean_names` can be FALSE, or it can also be a name-cleaning function of your choice. If `TRUE` (the default), `janitor::make_clean_names()` is used.

## TODO:

* Create documentation

### Installing

This isn't on CRAN, so the only place to get it is here:

```
# devtools::install_github("church-army/monkeyreadR")
# or, better yet (imo):
pak::pak("church-army/monkeyreadR")
```