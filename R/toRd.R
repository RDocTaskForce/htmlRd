setOldClass("shiny.tag")

#' Method and Generic `toRd.shiny.tag`
#'
#' This is both a method for \code{\link[Rd]{toRd}} and defines the
#' generic `toRd.shiny.tag` which re-dispatches on the type of
#' [HTML tag][html_get_type].
#'
#' @param obj a `shiny.tag` object.
#' @param ... passed to methods.
#' @seealso
#' * Root generic: \code{\link[Rd]{toRd}}
#' * Methods: \code{\link{toRd.shiny.tag-methods}}
#' * Generators: \code{\link{toRd.shiny.tag-generators}}
#' @examples
#' toRd(htmltools::a("some text", href="https://r-project.org"))
#' toRd(htmltools::tags$abbr("GPL"))
#' toRd(htmltools::code("plot(rnorm(100))"))
#' toRd(htmltools::tags$dfn("abc"))
#' toRd(htmltools::tags$dl( htmltools::tags$dt("term1")
#'                        , htmltools::tags$dd("definition 1.")
#'                        , htmltools::tags$dt("term2")
#'                        , htmltools::tags$dd("definition 2.")
#'                        ))
#' toRd(htmltools::tags$ol( htmltools::tags$li("First")
#'                        , htmltools::tags$li("Second")
#'                        ))
#' toRd(htmltools::tags$ul( htmltools::tags$li("First")
#'                        , htmltools::tags$li("Second")
#'                        ))
toRd.shiny.tag <-
function(obj, ...){
    UseMethod('toRd.shiny.tag', cl(obj, html_get_type(obj)))
}

# described in manual toRd.shiny.tag-methods
toRd.shiny.tag.default <- function(obj, ...){
    pkg_error(._("Cannot convert object of type %1$s to Rd.", class(obj))
              , type="cannot_convert")
}
if(FALSE){#@testing
    expect_error( toRd.shiny.tag(1L), class='htmlRd-error')
}

# described in manual toRd.shiny.tag-methods
toRd.shiny.tag.shiny.tag <- function(obj, ...){
    assert_that(inherits(obj, 'shiny.tag'))
    pkg_error(._("Cannot convert shiny.tag of type %1$s." %<<%
                     "While %1$s is a valid HTML5 tag, documentation" %<<%
                     "does not currently support it's conversion to Rd."
                 , sQuote(obj$name))
              , type="unsupported_tag")
}
