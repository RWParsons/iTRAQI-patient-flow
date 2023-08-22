
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iTRAQI-patient-flow

<!-- badges: start -->
<!-- badges: end -->

How to test out the (still very much in development) shiny app.

1.  Clone this repository and open the project in RStudio (the GitHub
    desktop app may be helpful).
2.  Be using R version 4.1.0 or greater and have access to the following
    folder on U drive:
    `U:\Research\Projects\health\jti_research_data\jti_itraqi_mapping`
3.  Install `{renv}` and then run `renv::restore()`.
4.  Run the shiny app using the following code.

``` r
shiny::runApp("app")
```

> You may get lots of errors/warnings to say that you don’t have the
> necessary packages installed. Install them as they show up - I hope
> there’s not too many for you! You can run `renv::restore()` to load
> all the necessary packages. Do this and try it again. If it doesn’t
> work - please let me (Rex) know!

The first time you open the app, it will need to create the base-map and
this can take a while. It will create a file in your project directory
(`./app/fixtures/base-map.rds`) and then when re-loading the map, it’ll
be faster. But this only works while you’re in the same R session. When
you restart R, it’ll cause a bug and the map won’t display. If this
happens, it means that you need to delete the `base-map.rds` file at the
mentioned path.
