
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iTRAQI-patient-flow

<!-- badges: start -->
<!-- badges: end -->

How to test out the (still very much in development) shiny app.

1.  Clone this repository and open the project in RStudio (the GitHub
    desktop app may be helpful).
2.  Be using R version 4.1.0 or greater and have access to the following
    folder on U drive:
    `U:\Research\Projects\health\jti_research_data\jti_itraqi`
3.  Run the `read-and-wrangle.R` script with the following code:

``` r
source("read-and-wrangle.R")
```

> You may get lots of errors/warnings to say that you don’t have the
> necessary packages installed. Install them as they show up - I hope
> there’s not too many for you! (I’ll streamline this in the future with
> `{renv}`).

4.  Run the shiny app using the following code (again… you might get
    problems with missing packages).

``` r
shiny::runApp("app")
```

Let me (Rex) know if you have any issues!
