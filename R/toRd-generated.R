# Generator Functions-----------------------------------------------------------

#' @name toRd.shiny.tag-generators
#' @title Generated `toRd` methods
#'
#' @description
#' Methods for [toRd.shiny.tag()] and the functions that generated them.
#'
#' @param html.tag the name of the HTML tag
#' @param rd.tag the name of the Rd tag.  Preceding backslash is added
#'               silently if omitted.
#' @param allowed.children The tags that are allowed to by contained by
#'             the HTML tag.
#' @param envir The environment of the resulting function.
#' @seealso
#' * Root generic: \code{\link[Rd]{toRd}}
#' * Sub Generic: \code{\link{toRd.shiny.tag}}
#' * Methods: \code{\link{toRd.shiny.tag-methods}}
make_simple_toRd_converter <-
    function(html.tag, rd.tag, allowed.children=NULL, envir = parent.frame()){
        assert_that( is.string(html.tag)
                   , is.string(rd.tag)
                   )
        if (!grepl("^\\\\", rd.tag)) rd.tag <- paste0("\\", rd.tag)
        fun <- if (!is.null(allowed.children)) {
            assert_that(is.character(allowed.children))
            expr <- substitute( env=list( allowed.children = allowed.children
                                        , html.tag=html.tag
                                        , rd.tag=rd.tag
                                        ),
                # nocov start
                function(obj, ...){
                    assert_that( html_is_type(obj, html.tag)
                               , html_has_valid_children(obj, allowed=allowed.children)
                               )
                    Rd_tag(rd.tag, content = toRd(obj$children, ...))
                }
                # nocov end
                )
            s( eval(expr, envir=envir)
             , allowed.children = allowed.children
             , rd.tag = rd.tag
             )
        } else {
            # nocov start
            expr <- substitute(function(obj, ...){
                assert_that( html_is_type(obj, html.tag))
                Rd_tag(rd.tag, content=toRd(obj$children, ...))
            })
            # nocov end
            eval(expr, envir=parent.frame())
        }
        attr(fun, 'srcref') <- NULL
        fun
    }
if(FALSE){#@testing
    test_fun <- make_simple_toRd_converter('htmltag', 'rdtag')
    expect_identical( formals(test_fun)
                    , as.pairlist(alist(obj=, ...=))
                    )

    expect_identical( trimws(deparse(body(test_fun), 500))
                    , c( '{'
                       , "assert_that(html_is_type(obj, \"htmltag\"))"
                       , "Rd_tag(\"\\\\rdtag\", content = toRd(obj$children, ...))"
                       , "}"
                       )
                    )
    # expect_equal( isTRUE(is_documented('test_fun', environment(), complete=FALSE))
    #               , .document.generated)

    html <- htmltools::tag('htmltag', varArgs = list('content'))
    val <- test_fun(html)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag("\\rdtag", Rd_text("content")))


    expect_error(test_fun(htmltools::tag('not the right tag', varArgs = list('content'))))

    test_children <- make_simple_toRd_converter('htmltag', 'rdtag', allowed.children = c('tag1', 'tag2'))
    expect_is(test_children, 'function')

    expect_identical( trimws(deparse(body(test_children), 500))
                    , c( '{'
                       , 'assert_that(html_is_type(obj, "htmltag"),' %<<%
                         'html_has_valid_children(obj, allowed = c("tag1", "tag2")))'
                       , 'Rd_tag("\\\\rdtag", content = toRd(obj$children, ...))'
                       , '}'
                       )
                    )
}

#' @rdname toRd.shiny.tag-generators
#' @param warn.info.loss The default condition that should be raised
#'            when information is likely to be lost, i.e. a reverse
#'            transformation of Rd to HTML is not available or not
#'            possible.
make_simple_toRd_extractor <-
function( warn.info.loss = 'warning'
        , envir = parent.frame()
        ){
    fun <- function(obj,..., warn.info.loss="warning"){
        assert_that(inherits(obj, "shiny.tag"))
        html_conversion_information_loss(obj$name, warn.info.loss)
        toRd(obj$children)
    }
    if (warn.info.loss != "warning")
        formals(fun)$warn.info.loss <- warn.info.loss
    environment(fun) <- envir
    attr(fun, 'srcref') <- NULL
    fun
}
if(FALSE){#@testing
    test_extract <- make_simple_toRd_extractor()

    expect_equal(formals(test_extract)$warn.info.loss, 'warning')
    expect_identical( formals(test_extract)
                    , as.pairlist(alist(obj=, ...=, warn.info.loss = "warning")))

    html <- htmltools::tag('htmltag', varArgs = list('content'))
    expect_warning( val <- test_extract(html)
                  , class =  "htmlRd-warning-info_loss")
    expect_is(val, 'Rd')
    expect_identical(val, Rd(Rd_text("content")))


    test_extract_2 <- make_simple_toRd_extractor('none')
    expect_equal(formals(test_extract_2)$warn.info.loss, 'none')
    expect_silent(val2 <- test_extract_2(html))
    expect_identical(val2, Rd(Rd_text("content")))
}

simple_toRd_extractor <- make_simple_toRd_extractor()
### <td> ####
toRd.shiny.tag.td <- make_simple_toRd_extractor('none')
### <th> ####
toRd.shiny.tag.th <- make_simple_toRd_extractor('message')


# Generated Methods ------------------------------------------------------------
### <aside> #####
toRd.shiny.tag.aside <- make_simple_toRd_converter('aside', 'note')
if(FALSE){#@testing
    html <- htmltools::tags$aside(c("Just a note", "that in html is called an aside."))
    expect_equal( toRd(html)
                , Rd_tag( "\\note", "Just a notethat in html is called an aside."))
}

### <em> #####
toRd.shiny.tag.em <- make_simple_toRd_converter('em','emph')

### <cite> #####
toRd.shiny.tag.cite <- make_simple_toRd_converter('cite', 'cite')
if(FALSE){#@testing
    html <- htmltools::tags$cite("a citation")
    val <- toRd(html)
    expect_identical(val, Rd_tag('\\cite', Rd_text("a citation")))
    test_equal_to_reparse(Rd_description(val))
}

### <kbd> #####
toRd.shiny.tag.kbd <- make_simple_toRd_converter('kbd', 'kbd')
if(FALSE){#@testing
    html <- htmltools::tags$kbd("abc")
    val <- toRd(html)
    expect_identical( val, Rd_tag("\\kbd", Rd_text("abc")))
}

### <pre> #####
toRd.shiny.tag.pre <- make_simple_toRd_converter("pre", "preformatted")

### <q> #####
toRd.shiny.tag.q <- make_simple_toRd_converter("q", "dQuote")

### <samp> #####
toRd.shiny.tag.samp <- make_simple_toRd_converter("samp", "preformatted")

### <strong> #####
toRd.shiny.tag.strong <- make_simple_toRd_converter('strong','strong')
