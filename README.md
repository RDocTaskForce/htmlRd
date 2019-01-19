
<!-- README.md is generated from README.Rmd. Please edit that file -->
htmlRd
======

[![Travis build status](https://travis-ci.org/RDocTaskForce/htmlRd.svg?branch=master)](https://travis-ci.org/RDocTaskForce/htmlRd) [![Coverage status](https://codecov.io/gh/RDocTaskForce/htmlRd/branch/master/graph/badge.svg)](https://codecov.io/github/RDocTaskForce/htmlRd?branch=master)

The goal of htmlRd is to provide methods for converting [htmltools](https://cran.r-project.org/package=htmltools) shiny.tag objects into Rd.

Installation
------------

You can install the released version of htmlRd from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("htmlRd")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
html <- with(htmltools::tags, {
    div( h1("Converting HTML to Rd")
       , p("Currently "
          , code(a("htmlRd", href="https://github.com/RDocTaskForce/htmlRd"))
          , " only supports HTML tags found in the "
          , em("body"), " of the document."
          )
       )
})
html
```

<!--html_preserve-->
<h1>
Converting HTML to Rd
</h1>
<p>
Currently <code> <a href="https://github.com/RDocTaskForce/htmlRd">htmlRd</a> </code> only supports HTML tags found in the <em>body</em> of the document.
</p>

<!--/html_preserve-->
``` r
toRd(html)
#> \section{Converting HTML to Rd}{
#> Currently \code{\href{{https://github.com/RDocTaskForce/htmlRd}{htmlRd}}} only supports HTML tags found in the \emph{body} of the document.
#> }
```

Acknowledgements
----------------

The `testextra` package is developed by the R Documentation Task Force, an [R Consortium](https://www.r-consortium.org) [Infrastructure Steering Committee working group](https://www.r-consortium.org/projects/isc-working-groups).
