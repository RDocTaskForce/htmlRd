# Utilities --------------------------------------------------------------------
# General ======================================================================

.T <- function(...)
{
    x <- substitute(c(...))
    val <- as.character(x)
    if (!is.null(n <- names(x)))
        names(val) <- ifelse( is.na(n) | n == ''
                            , as.character(x)
                            , names(x))
    return(val[-1L])
}
if(FALSE){#@testing
    expect_equal(.T(._, s, cl, .T)
                , c('._', 's', 'cl', '.T')
                )
    expect_equal( .T(a=._, s, cl, .T)
                , c(a='._', s='s', cl='cl', .T='.T')
                )
}

s <- function( x, ...){
    new.attr <- list(...)
    if (is.null(names(new.attr)))
        names(new.attr) <- as.character(substitute(c(...)))[-1]
    else if(any(. <- is.na(names(new.attr)) | names(new.attr) == ''))
        names(new.attr) <- ifelse(., as.character(substitute(c(...)))[-1], names(new.attr))

    for (a in names(new.attr))
        attr(x, a) <- new.attr[[a]]
    return(x)
}
if(FALSE){#@testing
    msg <- "An failure message"
    val <-s(FALSE, msg)
    expect_identical(attributes(val), list(msg=msg))


    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))

    val <- s(c(a=1, b=2), count=2)
    expect_identical(names(val), c('a','b'))
}

cl <- function(x, new){s(x, class=union(new, attr(x, 'class')))}
if(FALSE){#@testing
    x <- cl(TRUE, 'success')
    expect_is(x, 'success')

    y <- cl(x, 'a big success')
    expect_is(y, 'success')
    expect_is(y, 'a big success')

    expect_identical(cl('text', 'class')
                    , structure('text', class='class'))
}

undim <- function(x)s(x, dim=NULL)
if(FALSE){#@testing
    x <- matrix(1:6, 2, 3)
    dimnames(x) <- list(rows = c('a', 'b'), cols = c('x', 'y', 'z'))

    expect_identical(undim(x), 1:6)
}

str_rep <- function(x, len, sep=''){paste(rep_len(x, length.out = len), collapse=sep)}
if(FALSE){#@testing
    expect_equal(str_rep('#', 3), '###')
    expect_equal(str_rep(c('r', 'l'), 5), "rlrlr")
}

# HTML =========================================================================

#' Get the HTML Tag Type
#'
#' Get the HTML tag of a `shiny.tag` object from the `htmltools` package.
#' @param x a `shiny.tag` object.
html_get_type <- function(x){
    if (identical(class(x), 'list')) return(sapply(x, html_get_type))
    else if (inherits(x, 'shiny.tag')) return(x$name)
    else if (is.character(x)) return("")
    else pkg_error(._("Cannot get HTML type from type %s", dQuote(class(x))))
}
if(FALSE){#@testing
    e <- htmltools::em('with some emphatic text.')
    a <- htmltools::tags$p( "Some paragraph text", e)
    b <- htmltools::code("plot(rnorm(100))")
    x <- htmltools::tags$div( a, b)

    expect_identical(html_get_type(x), 'div')
    expect_identical(html_get_type(x$children), c('p', 'code'))

    html <- htmltools::tags$li('with some emphatic text.')
    class(html) <- c('li', 'shiny.tag')
    expect_identical(html_get_type(html), 'li')

    expect_identical(html_get_type('character'), '')

    expect_error( html_get_type(NULL)
                , class = "htmlRd-error" )
}

html_is_type <- function(html, type){
    assert_that(inherits(html, 'shiny.tag'))
    html_get_type(html) == type
}
attr(html_is_type, 'fail') <- function(call, env){
    actual.class <- html_get_type(eval(call$html, envir = env))
    expected.class <- eval(call$type, envir = env)
    deparse(call$html)  %<<%
        "is of type" %<<% dQuote(actual.class) %<<<%
        ";" %<<% "expected a" %<<% dQuote(expected.class)
}
if(FALSE){#@testing html_is_type
    a <- htmltools::a('link')
    expect_true(html_is_type(a, 'a'))

    withr::with_options(list(useFancyQuotes=FALSE),
                        expect_equal( assertthat::see_if(html_is_type(a, 'li'))
                                      , s(FALSE, msg = 'a is of type "a"; expected a "li"')
                        ))
}

html_has_valid_children <-
function(html, allowed){
    ctypes <- html_get_type(html$children)
    good <- ctypes %in% allowed
    if (all(good)) return(TRUE)
    bad.tags <- sort(unique(ctypes[!good]))
    msg <- ._("HTML tag %s contains invalid child", html_get_type(html)) %<<%
           ngettext( sum(!good), "tag", "tags") %<<%
           ngettext( length(bad.tags), "of type", "of types") %<<%
           comma_list(bad.tags) %<<<% '.'
    s(FALSE, msg=msg)
}
if(FALSE){#@testing
    good.html <- with(htmltools::tags, ol(li('hello'), li("world") ))
    expect_true(assertthat::see_if(html_has_valid_children(good.html, 'li')))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dd("definition") ))
    expect_equal( assertthat::see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of types dd and dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dd("definition"), a("link") ))
    expect_equal( assertthat::see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of types a, dd, and dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term")))
    expect_equal( assertthat::see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tag of type dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dt("term")))
    expect_equal( assertthat::see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of type dt."))
}

is_header <- function(html){
    inherits(html, 'shiny.tag') && grepl('h[1-6]', html$name)
}
if(FALSE){#@testing
    expect_true(is_header(htmltools::tags$h1("yes")))
    expect_false(is_header(htmltools::tags$b("yes")))
    expect_false(is_header("yes"))
}

# URL ==============================================================
.protocols <- .T(http, https, ftp, mailto, file, data, irc)
url.pattern <- paste0('^(', collapse(.protocols, with='|'), ')://')
is_url <- function(x){grepl(url.pattern, x)}

# links ================================================================
pkg.base.pattern <- "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
pkg.pattern <-  "^" %<<<% pkg.base.pattern %<<<% "$"
name.pattern <- "[a-zA-Z.][a-zA-Z0-9_.]*(-[a-zA-Z]+)?"
pkg.dest.pattern <- "^" %<<<% pkg.base.pattern %<<<% ":" %<<<% name.pattern %<<<% "$"
is_rd_link <- function(x){
    (x == '') | grepl("^=", x) | grepl(pkg.pattern, x) | grepl(pkg.dest.pattern, x)
}
if(FALSE){#@testing
    expect_true(is_rd_link(''))
    expect_true(is_rd_link('=abc-class'))
    expect_true(is_rd_link('terms.object'))
    expect_true(is_rd_link('base:abc'))
    expect_false(is_rd_link("http://r-project.org"))
}

# Testing ==============================================================
test_equal_to_reparse <- function(rd, info=NULL, label= deparse(substitute(rd))){
    if (!is_exactly(rd, 'Rd')) rd <- Rd(rd)
    txt <- format(rd)
    reparse <- tools::parse_Rd(textConnection(txt))
    if (tail(reparse, 1L) == '\n') reparse <- head(reparse, -1L)
    testthat::expect_equal( rd, reparse
                          , label = label, info=info
                          , expected.label = "reparsed structure"
                          )
}
if(FALSE){# reprex for \\href
    library(tools)
    txt <- "\\seealso{\\href{https://r-project.org}{R-project}}"
    (reparse <- tools::parse_Rd(textConnection(txt)))
    (txt2 <- paste(as.character(reparse), sep='', collapse=''))
    rd2 <- tools::parse_Rd(textConnection(txt2))
}



