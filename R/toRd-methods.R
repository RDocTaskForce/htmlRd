toRd.shiny.tag.a <-  function(obj, ...){
    href <- obj$attribs$href
    assert_that(all(purrr::map_lgl(obj$children, is.character)))
    if (is.null(href)) return(Rd_tag('\\link', content=toRd(obj$children)))
    if (is_rd_link(href))
        return(Rd_tag('\\link', content=toRd(obj$children), opt=Rd_text(href)))
    if (is_url(href))
        return(Rd_tag('\\href'
                      , Rd(Rd_verb(href))
                      , Rd(toRd(obj$children))
        ))
    pkg_error( ._("Don't know how to interpret href=%s", dQuote(href))
               , type = 'unknown_url_scheme' )
}
if(FALSE){#@testing
    expect_identical( toRd(htmltools::a("somewhere"))
                      , Rd_tag("\\link", Rd_text("somewhere"))
    )
    obj <- a <- htmltools::a("some text", href="https://r-project.org")

    val <- toRd(obj)
    expect_is(val, 'Rd_tag')
    expect_true(is_Rd_tag(val, '\\href'))
    expect_identical( val
                      , Rd_tag("\\href"
                               , Rd(Rd_verb("https://r-project.org"))
                               , Rd("some text")
                      ))
    # Note that \\href Rd_tag does not format correctly.
    txt <- "\\seealso{\\href{https://r-project.org}{some text}}"
    parsed <- tools::parse_Rd(textConnection(txt))[[c(1,1)]]
    expect_true(is_Rd_tag(parsed, "\\href"))
    expect_equal(val, parsed, check.attributes=FALSE)

    val <- toRd(htmltools::a("some text", href="abc"))
    expect_is(val, 'Rd_tag')
    expect_identical(format(val), "\\link[abc]{some text}")

    val <- toRd(htmltools::a("some text", href="=abc-class"))
    expect_is(val, 'Rd_tag')
    expect_identical(format(val), "\\link[=abc-class]{some text}")

    val <- toRd(htmltools::a("some text", href="pkg:dest"))
    expect_is(val, 'Rd_tag')
    expect_identical(format(val), "\\link[pkg:dest]{some text}")

    val <- toRd(htmltools::a("some text", href="=somewhere"))
    expect_is(val, 'Rd_tag')
    expect_identical(format(val), "\\link[=somewhere]{some text}")

    expect_error( toRd(htmltools::a("some text", href="somewhere over the rainbow"))
                  , class = "htmlRd-error-unknown_url_scheme" )
    expect_error( toRd(htmltools::a("some text", href="somewhere over the rainbow"))
                  , class = "htmlRd-error-unknown_url_scheme" )
}

toRd.shiny.tag.abbr <- function(obj, ..., abbr.check.caps=TRUE){
    assert_that( all(purrr::map_lgl(obj$children, is.character))
               , length(obj$children) == 1L
               , is.flag(abbr.check.caps)
               )
    content <- toRd(obj$children[[1]])
    if (abbr.check.caps && !grepl("^[A-Z]+$", content))
        pkg_warning(._( "HTML tag %s maps to Rd tag acronym, " %<<%
                            "It should consist of all capitol letters."
                        , dQuote(obj$name))
                    , type = "toRd" )
    Rd_tag('\\acronym', content=Rd(content))
}
if(FALSE){#@testing
    obj <- htmltools::tags$abbr("GPL")
    val <- toRd(obj)
    expect_identical(format(val), "\\acronym{GPL}")
    expect_identical(val, Rd_tag("\\acronym", Rd_text("GPL")))

    expect_warning( toRd(htmltools::tags$abbr("not an acronym"))
                  , class = 'htmlRd-warning-toRd')
    bad.abbr <- with(htmltools::tags, abbr( "not an acronym", b("not valid")))
    expect_error( toRd(bad.abbr))
}

toRd.shiny.tag.br <- function(obj, ...){
    assert_that( html_is_type(obj, 'br'))
    if (length(obj$children) > 0)
        error_html5_malformed('br', "cannot have children.")
    return(Rd_tag('\\cr'))
}
if(FALSE){#@testing
    obj <- htmltools::tags$br()
    val <- toRd(obj)

    expect_identical(format(val), "\\cr")

    obj <- with(htmltools::tags, br('text'))
    expect_error( toRd.shiny.tag.br(obj)
                , class = "htmlRd-error-malformed_html")
    expect_error( toRd(obj)
                , class = "htmlRd-error-malformed_html")
}

toRd.shiny.tag.code <- function(obj, ...){
    assert_that( is.list(obj$children)
               , length(obj$children)==1
               )
    content <- toRd(obj$children)
    if (is_Rd(content) && is_Rd_string(content[[1]], 'TEXT'))
        content[[1]] <- Rd_rcode(content[[1]])
    Rd_tag("\\code", content=content)
}
if(FALSE){#@testing
    obj <- htmltools::code("plot(rnorm(100))")
    rd <- toRd(obj)
    expect_true(is_Rd_tag(rd, '\\code'))
    expect_identical(format(rd), "\\code{plot(rnorm(100))}")

    obj <- htmltools::code("'a' %in% letters")
    rd <- toRd(obj)
    expect_identical(format(rd), "\\code{'a' \\%in\\% letters}")
}

toRd.shiny.tag.div <- function(obj, ..., sub.section=FALSE){
    assert_that( is.flag(sub.section))
    children <- obj$children
    title <- Rd(toRd(obj$attribs$title))
    if (is.null(obj$attribs$title))
        if (is_header(children[[1]])) {
            title <- toRd(children[[1]]$children, warn.info.loss='none')
            children <- children[-1]
        } else
            error_html5_malformed('div', "cannot determine title of section.")
    content <- Rd_c(Rd('\n'), toRd(children, sub.section=TRUE), Rd('\n'))
    content <- Rd_canonize(content)
    while (content[[1]] == '\n' && content[[2]] == '\n')
        content <- tail(content, -1)
    while (tail(content,1) == '\n') content <- head(content, -1)

    Rd_tag( if(sub.section) "\\subsection" else "\\section"
          , content = list(title, content)
          , ...)
}
if(FALSE){#@testing
    txt <- lapply(stringi::stri_rand_lipsum(3), htmltools::tags$p)
    title <- "test title"
    obj <- htmltools::tags$div(title=title)
    obj$children <- txt
    val <- toRd(obj)
    test_equal_to_reparse(val)

    expect_true(is_Rd_tag(val, "\\section"))

    h <- htmltools::tags$h3("Embedded Title")
    obj <- htmltools::tags$div(h, txt)
    val <- toRd(obj)

    expect_true(is_Rd_tag(val, "\\section"))
    expect_length(val, 2L)
    expect_identical(val[[1]], Rd('Embedded Title'))

    ss <-   htmltools::tags$div( htmltools::tags$h4("Sub-section Header")
                                 , stringi::stri_rand_lipsum(2))
    sec <- htmltools::tags$div( htmltools::tags$h3("Section Header")
                                , ss)
    val <- toRd(sec)
    test_equal_to_reparse(val)

    expect_true(is_Rd_tag(val, '\\section'))
    expect_is_exactly(val[[2]], 'Rd')
    expect_equal(val[[2]][1], Rd('\n'))
    expect_true(is_Rd_tag(val[[2]][[2]], '\\subsection'))

    expect_error( toRd(with(htmltools::tags, div( em("Section Header"), ss)))
                , class="htmlRd-error-malformed_html" )
}

toRd.shiny.tag.dfn <- function(obj, ...){
    assert_that( length(obj$children) == 1
               , is.character(obj$children[[1]])
               )
    Rd_tag('\\dfn', Rd_text(obj$children[[1]]))
}
if(FALSE){#@testing
    rd <- toRd(obj <- htmltools::tags$dfn("abc"))
    expect_identical(rd, Rd_tag('\\dfn', Rd_text("abc")))
}

toRd.shiny.tag.dt <- function(obj, ...){
    Rd(toRd(obj$children))
}
if(FALSE){#@testing
    val <- toRd(htmltools::tags$dt("my term"))
    expect_equal(val, Rd("my term"))
    expect_equal(format(val), "my term" )
}
toRd.shiny.tag.dd <- function(obj, ...){
    Rd(toRd(obj$children))
}
if(FALSE){#@testing
    val <- toRd(htmltools::tags$dd("definition 1."))
    expect_identical(val, Rd("definition 1."))
}
toRd.shiny.tag.dl <- function(obj, ...){
    assert_that( length(obj$children) %% 2 == 0
               , all( sapply(obj$children, `[[`, 'name') %in% c('dd', 'dt') )
               )
    items <- vector("list", length(obj$children) %/% 2)
    for (i in 1:length(items)) {
        j <- (i-1)*2L + 1L
        dt <- obj$children[[j]]
        dd <- obj$children[[j+1]]
        if ( dt$name != 'dt' || dd$name != 'dd' )
            error_html5_malformed('dl',
                                  ._("HTML tags %s and %s occure in the wrong order."
                                     , sQuote('dt'), sQuote('dd')))
        items[[i]] <- Rd_item( toRd(dt), toRd(dd))
    }
    Rd_tag('\\describe', content=Rd_lines(items), ...)
}
if(FALSE){#@testing
    obj <- htmltools::tags$dl( htmltools::tags$dt("term1")
                             , htmltools::tags$dd("definition 1.")
                             , htmltools::tags$dt("term2")
                             , htmltools::tags$dd("definition 2.")
                             )

    val <- toRd(obj)

    expect_equal( format(val)
                , "\\describe{" %\%
                  "\\item{term1}{definition 1.}" %\%
                  "\\item{term2}{definition 2.}" %\%
                  "}"
                )

    val <- toRd(obj, indent=TRUE, indent.with='  ')
    test_equal_to_reparse(Rd_description(val))
    expect_equal( format(val)
                , "\\describe{" %\%
                  "  \\item{term1}{definition 1.}" %\%
                  "  \\item{term2}{definition 2.}" %\%
                  "}"
                )

    obj <- htmltools::tags$dl( htmltools::tags$dt("term1")
                             , htmltools::tags$dd("definition 1.")
                             , htmltools::tags$dd("definition 2.")
                             , htmltools::tags$dt("term2")
                             )
    expect_error( toRd(obj), class="htmlRd-error-malformed_html" )
}

toRd.shiny.tag.html <- function(obj, ...){
    toRd(obj$children)
}
if(FALSE){#@testing
    html1 <- htmltools::tags$dl( htmltools::tags$dt("term1")
                               , htmltools::tags$dd("definition 1.")
                               , htmltools::tags$dt("term2")
                               , htmltools::tags$dd("definition 2.")
                               )

    html2 <- htmltools::tags$html(html1)

    expect_identical( toRd(html2)[[1]]
                    , toRd(html1)
                    )
}

toRd.shiny.tag.img <- function(obj, ...){
    assert_that( length(obj$children) == 0)
    src <- obj$attribs$src
    if (is.null(src)) error_html5_malformed('img', ._("No src defined."))
    opts <- obj$attribs[names(obj$attribs) != 'src']
    opts <- 'options:' %<<% paste0(names(opts), '=', sapply(opts, deparse))
    Rd_tag('\\figure', Rd(Rd_verb(src)), Rd(Rd_verb(opts)))
}
if(FALSE){#@testing
    obj <- htmltools::tags$img(src='test.png', alt ='alternate text', height=100, width=100)
    val <- toRd(obj)
    test_equal_to_reparse(Rd_description(val))

    expect_true(is_Rd_tag(val, '\\figure'))
    expect_equal( format(val)
                , "\\figure{test.png}{options: alt=\"alternate text\" height=100 width=100}")

    obj <- htmltools::tags$img(alt ='alternate text', height=100, width=100)
    expect_error(toRd(obj), class="htmlRd-error")

}

toRd.shiny.tag.li <- function(obj, ...){
    Rd_canonize(Rd_item(toRd(obj$children, ...)))
}
if(FALSE){#@testing
    obj <- htmltools::tags$li("some ", htmltools::tags$em('text'), '.')
    val <- toRd(obj)

    expect_equal( val, Rd( Rd_tag('\\item'), Rd_text(" some ")
                         , Rd_tag('\\emph', Rd_text("text"))
                         , Rd_text(".")))
    test_equal_to_reparse(Rd_description(Rd_tag("\\itemize", val)))
}

toRd.shiny.tag.p <- function(obj, ...){
    content <- obj$children
    if (length(content)==0) return(Rd())
    Rd_canonize(Rd_c(toRd(content), Rd('\n\n')))
}
if(FALSE){#@testing
    obj <- htmltools::tags$p(stringi::stri_rand_lipsum(1))

    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    test_equal_to_reparse(val)

    expect_identical(toRd(htmltools::p()), Rd())
    expect_is(toRd(htmltools::p("text")), 'Rd')
    expect_equal(format(toRd(htmltools::p("text"))), c("text\n\n"))
}

toRd.shiny.tag.ol <- function( obj, ...){
    assert_that( html_is_type(obj, "ol")
               , html_has_valid_children(obj, allowed = "li")
               )
    items <- lapply(obj$children, toRd)
    Rd_tag('\\enumerate', content=Rd_lines(items), ...)
}
if(FALSE){#@testing
    obj <- htmltools::tags$ol( htmltools::tags$li("First")
                             , htmltools::tags$li("Second")
                             )
    val <- toRd(obj)
    test_equal_to_reparse(Rd_description(val))
    expected <- Rd_tag( "\\enumerate", '\n'
                      , Rd_tag("\\item"), Rd_text(" First\n")
                      , Rd_tag("\\item"), Rd_text(" Second\n")
                      )
    expect_equal(val, expected)

    val <- toRd(obj, indent=TRUE, indent.with='  ')
    expect_identical( as.character(Rd(val))
                    , c( "\\enumerate", '{', '\n'
                       , "  ", "\\item", " First\n"
                       , "  ", "\\item", " Second\n"
                       , "}"
                       )
                    )
    expect_error( toRd(htmltools::tags$ol( htmltools::tags$li("First")
                                         , htmltools::tags$dl("Second")
                                         )))
}

toRd.shiny.tag.table <-function( obj, ..., col.align = 'l'){
    allowed.chilren <- c('thead', 'tbody', 'tfoot')
    ctypes <- html_get_type(obj$children)
    assert_that( all(ctypes %in% allowed.chilren)
                 , !anyDuplicated(ctypes)
    )
    if (!any(ctypes == 'tbody'))
        error_html5_malformed(._("HTML table element must contain a tbody."))

    body <- toRd(obj$children[[which(ctypes == 'tbody')]], ...)
    head <- if(any(. <- ctypes == 'thead'))
        toRd(obj$children[[which(.)]], ...)
    foot <- if(any(. <- ctypes == 'tfoot'))
        toRd(obj$children[[which(.)]], ...)

    if (!is.null(head))
        assert_that( attr(body, 'ncol') == attr(head, 'ncol')
                     , msg = "HTML tbody and thead do not have the same number of columns")
    if (!is.null(foot))
        assert_that( attr(body, 'ncol') == attr(foot, 'ncol')
                     , msg = "HTML tbody and thead do not have the same number of columns")

    nrow <- attr(body, 'nrow') +
        (attr(head, 'nrow') %||% 0) +
        (attr(foot, 'nrow') %||% 0)
    ncol <- attr(body, 'ncol')
    align <- Rd(str_rep(col.align, ncol))

    cr <- Rd_tag('\\cr')
    nl <- Rd('\n')
    content <- undim(rbind( list(Rd_c(cr, nl))
                          , list(head, body, foot)))[-1]

    content<- Rd_canonize(Rd_compact(content))
    Rd_tag("\\tabular", align, content)
}

toRd.shiny.tag.tr <- function(obj, ..., head=FALSE){
    allowed.children <- if (head) 'th' else 'td'
    assert_that( all(html_get_type(obj$children) %in% allowed.children) )
    cols <- lapply(obj$children, toRd, ...)

    nbsp <- Rd_text(" ")
    rd <- undim(rbind( list(Rd(nbsp, Rd_tag('\\tab'), nbsp))
                           , cols
                           ))[-1L]
    s( Rd_canonize(Rd_compact(rd))
     , ncol = length(cols))
}
if(FALSE){#@testing
    obj <- with(htmltools::tags, tr(td("first"), td('second')))
    expect_silent( val <- toRd(obj))
    expect_equal(attr(val, 'ncol'), 2)
    expect_equal(format(val), "first \\tab second")

    expect_error(toRd(obj, head=TRUE))
}
toRd.shiny.tag.tbody <- function( obj, ..., head=FALSE){
    assert_that( all(html_get_type(obj$children) %in% 'tr') )
    rows <- lapply( obj$children, toRd, ..., head=head)
    if (length(rows)==0) return(Rd())

    ncol <- max(purrr::map_int(rows, attr, 'ncol'))

    cr <- Rd_tag('\\cr')
    nl <- Rd('\n')
    content <- undim(rbind( list(Rd_c(cr, nl)), rows))[-1]
    s( Rd_canonize(Rd_compact(content))
     , ncol = ncol
     , nrow = length(obj$children))
}

toRd.shiny.tag.tfoot <-
    function(obj, ..., warn.info.loss  = "message"){
        if(length(obj$children)==0) return(Rd())
        html_conversion_information_loss('tfoot', cond = warn.info.loss)
        toRd.shiny.tag.tbody(obj, warn.info.loss = warn.info.loss, ...)
    }

toRd.shiny.tag.thead <- function( obj, ...){
    toRd.shiny.tag.tbody( obj, ..., head=TRUE)
}
if(FALSE){#@testing toRd.shiny.tag.* table functions
    obj <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                   , tbody( tr( td('R1'), td('O'), td('X'), td('O') )
                            , tr( td('R2'), td('X'), td('X'), td('O') )
                            , tr( td('R3'), td('O'), td('X'), td('X') )
                   )
                   , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                   , align = "right|center|center|center"
            )
        })
    thead <- obj$children[[1]]
    tbody <- obj$children[[2]]
    tfoot <- obj$children[[3]]


    expect_silent(val.body <- toRd(tbody))
    expect_equal(format(val.body)
                , "R1 \\tab O \\tab X \\tab O\\cr" %\%
                  "R2 \\tab X \\tab X \\tab O\\cr" %\%
                  "R3 \\tab O \\tab X \\tab X"
                )

    expect_message( head <- toRd(thead), class =  "htmlRd-message-info_loss")
    expect_is_exactly(head, 'Rd')
    expect_length(head, 7L)
    expect_equal(attr(head, 'ncol'), 4L)
    expect_equal(attr(head, 'nrow'), 1L)
    expect_equal(format(head), " \\tab C1 \\tab C2 \\tab C3")

    expect_message( foot <- toRd(tfoot)
                    , class =  "htmlRd-message-info_loss")

    expect_equal( format(foot)
                , "Count \\tab 1 \\tab 3 \\tab 1")

    val <- suppress_messages( toRd(obj, warn.info.loss='none')
                            , class = "htmlRd-message-info_loss")

    expect_true(is_Rd_tag(val, '\\tabular'))
    expect_length(val, 2L)
    expect_is_exactly(val[[1]], 'Rd')
    expect_is_exactly(val[[2]], 'Rd')
    expect_equal( format(val)
                , "\\tabular{llll}{" %\%
                  " \\tab C1 \\tab C2 \\tab C3\\cr" %\%
                  "R1 \\tab O \\tab X \\tab O\\cr" %\%
                  "R2 \\tab X \\tab X \\tab O\\cr" %\%
                  "R3 \\tab O \\tab X \\tab X\\cr" %\%
                  "Count \\tab 1 \\tab 3 \\tab 1" %\%
                  "}"
                )

    obj <- with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    expect_error(toRd(obj), class="htmlRd-error-malformed_html")

    obj <- with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    expect_error(toRd(obj), class="htmlRd-error-malformed_html")


    expect_identical( toRd(htmltools::tags$thead()), Rd())
    expect_identical( toRd(htmltools::tags$tbody()), Rd())
    expect_identical( toRd(htmltools::tags$tfoot()), Rd())

    thead <- with(htmltools::tags, {
        thead( tr( th(''), th('C'), th('D'), th('E') )
             , tr( th('.'), th('1'), th('2'), th('3') )
             )
    })
    val <- toRd( thead, collapse.lines=TRUE
               , warn.info.loss = 'none'
               )

    expect_identical( format(val)
                    , " \\tab C \\tab D \\tab E\\cr" %\%
                      ". \\tab 1 \\tab 2 \\tab 3"
                    )
    val <- toRd( thead, collapse.lines=FALSE
                       , warn.info.loss = 'none'
    )

    expect_identical( format(val)
                      , " \\tab C \\tab D \\tab E\\cr" %\%
                          ". \\tab 1 \\tab 2 \\tab 3"
    )

    tfoot <- with(htmltools::tags,
                  tfoot( tr( td('Count'), td('1'), td('2'), td('3') )
                         , tr( td('Total'), td('A'), td('B'), td('C') )
                  ))

    val <- toRd( tfoot, collapse.lines=TRUE
                       , warn.info.loss = 'none'
    )

    expect_identical( format(val)
                      , "Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                          "Total \\tab A \\tab B \\tab C"
    )

    val <- toRd( tfoot, collapse.lines=FALSE
                       , warn.info.loss = 'none'
    )

    expect_identical( format(val)
                      , "Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                          "Total \\tab A \\tab B \\tab C"
    )
}

toRd.shiny.tag.ul <-
    function( obj, ...){
        assert_that( html_is_type(obj, "ul")
                     , html_has_valid_children(obj, allowed = "li"))
        items <- lapply(obj$children, toRd)
        Rd_tag('\\itemize', content=Rd_lines(items), ...)
    }
if(FALSE){#@testing
    obj <- htmltools::tags$ul( htmltools::tags$li("First")
                             , htmltools::tags$li("Second")
                             )
    val <- toRd(obj)
    test_equal_to_reparse(Rd_description(val))
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag( "\\itemize", Rd_text('\n')
                                , Rd_tag("\\item"), Rd_text(" First\n")
                                , Rd_tag("\\item"), Rd_text(" Second\n")
                                ))
    expect_equal( format(val)
                , "\\itemize{" %\%
                  "\\item First" %\%
                  "\\item Second" %\%
                  "}"
                )

    val <- toRd(obj, indent=TRUE, indent.with='  ')
    test_equal_to_reparse(Rd_description(val))
    expect_equal( format(val)
                  , "\\itemize{" %\%
                      "  \\item First" %\%
                      "  \\item Second" %\%
                      "}"
    )

    expect_error( toRd(htmltools::tags$ol( htmltools::tags$li("First")
                                         , htmltools::tags$dl("Second")
                                         )))
}
