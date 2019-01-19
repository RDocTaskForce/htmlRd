# Tag Categories ----------------------------------------------------------
.html5.tags.discouraged.reasons <-
    .T( b      = "Use strong"
      , i      = "Use em"
      , small  = "Do not use formatting tags."
      , sup    = "Do not use formatting tags."
      , sub    = "Do not use formatting tags."
      , u      = "Do not use formatting tags."
      )
.html5.tags.discouraged <- names(.html5.tags.discouraged.reasons)
.html5.tags.allowed <-
    .T( a, abbr, aside
      , blockquote, br
      , caption, cite, code
      , dd, div, dl, dt
      , em
      , figcaption, figure
      , h1, h2, h3, h4, h5, h6
      , hr, html
      , img
      , kbd
      , li
      , ol
      , p, pre, q
      , samp, section, span, strong
      , table, tbody, td, tfoot, th, thead, tr
      , ul
      )
.html5.tags.disallowed <-
    .T( area, address, article, audio
      , base, bdi, bdo, body
      , canvas, center, col, colgroup, command
      , datalist, del, details, dnf
      , embed
      , fieldset, footer, form
      , head, header, hgroup
      , iframe, input, ins
      , keygen
      , label, legend, link
      , map, mark, menu, meta
      , nav, noscript
      , object, optgroup, option, output
      , param, progress
      , rp, rt, ruby
      , s      #< Used for no text that is longer correct.
      , script, select, source, style, summary
      , textarea, time, title, track, tt
      , var, video, wbr
      )
