\name{toRd.shiny.tag-methods}
\alias{toRd.shiny.tag-methods}
% specials
\alias{toRd.shiny.tag.default}
\alias{toRd.shiny.tag.shiny.tag}
% defined
\alias{toRd.shiny.tag.a}
\alias{toRd.shiny.tag.abbr}
\alias{toRd.shiny.tag.br}
\alias{toRd.shiny.tag.code}
\alias{toRd.shiny.tag.div}
\alias{toRd.shiny.tag.dfn}
\alias{toRd.shiny.tag.dd}
\alias{toRd.shiny.tag.dt}
\alias{toRd.shiny.tag.dl}
\alias{toRd.shiny.tag.html}
\alias{toRd.shiny.tag.img}
\alias{toRd.shiny.tag.li}
\alias{toRd.shiny.tag.p}
\alias{toRd.shiny.tag.ol}
\alias{toRd.shiny.tag.section}
\alias{toRd.shiny.tag.table}
\alias{toRd.shiny.tag.tr}
\alias{toRd.shiny.tag.tbody}
\alias{toRd.shiny.tag.tfoot}
\alias{toRd.shiny.tag.thead}
\alias{toRd.shiny.tag.ul}
% extractors
\alias{toRd.shiny.tag.h1}
\alias{toRd.shiny.tag.h2}
\alias{toRd.shiny.tag.h3}
\alias{toRd.shiny.tag.h4}
\alias{toRd.shiny.tag.h5}
\alias{toRd.shiny.tag.span}
\alias{toRd.shiny.tag.td}
\alias{toRd.shiny.tag.th}
% Converters
\alias{toRd.shiny.tag.aside}
\alias{toRd.shiny.tag.em}
\alias{toRd.shiny.tag.cite}
\alias{toRd.shiny.tag.kbd}
\alias{toRd.shiny.tag.pre}
\alias{toRd.shiny.tag.q}
\alias{toRd.shiny.tag.samp}
\alias{toRd.shiny.tag.strong}
\title{Methods for \code{toRd.shiny.tag}}
\usage{
# Special Methods ------------------------------------------------------
\method{toRd.shiny.tag}{default}(obj, ...)
\method{toRd.shiny.tag}{shiny.tag}(obj, ...)
# Defined Methods ------------------------------------------------------
\method{toRd.shiny.tag}{a}(obj, ...)
\method{toRd.shiny.tag}{abbr}(obj, ..., abbr.check.caps=TRUE)
\method{toRd.shiny.tag}{br}(obj, ...)
\method{toRd.shiny.tag}{code}(obj, ...)
\method{toRd.shiny.tag}{div}(obj, ..., sub.section=FALSE)
\method{toRd.shiny.tag}{dfn}(obj, ...)
\method{toRd.shiny.tag}{dd}(obj, ...)
\method{toRd.shiny.tag}{dt}(obj, ...)
\method{toRd.shiny.tag}{dl}(obj, ...)
\method{toRd.shiny.tag}{html}(obj, ...)
\method{toRd.shiny.tag}{img}(obj, ...)
\method{toRd.shiny.tag}{li}(obj, ...)
\method{toRd.shiny.tag}{p}(obj, ...)
\method{toRd.shiny.tag}{ol}(obj, ...)
\method{toRd.shiny.tag}{section}(obj, ..., sub.section=FALSE)
\method{toRd.shiny.tag}{table}(obj, ..., col.align = 'l')
\method{toRd.shiny.tag}{tr}(obj, ..., head=FALSE)
\method{toRd.shiny.tag}{tbody}(obj, ..., head=FALSE)
\method{toRd.shiny.tag}{tfoot}(obj, ..., warn.info.loss = "message")
\method{toRd.shiny.tag}{thead}(obj, ...)
\method{toRd.shiny.tag}{ul}(obj, ...)
# Generated Methods ----------------------------------------------------
## Extractors ==========================================================
\method{toRd.shiny.tag}{h1}(obj, ..., warn.info.loss="warning")
\method{toRd.shiny.tag}{h2}(obj, ..., warn.info.loss="warning")
\method{toRd.shiny.tag}{h3}(obj, ..., warn.info.loss="warning")
\method{toRd.shiny.tag}{h4}(obj, ..., warn.info.loss="warning")
\method{toRd.shiny.tag}{h5}(obj, ..., warn.info.loss="warning")
\method{toRd.shiny.tag}{span}(obj, ..., warn.info.loss="warning")
\method{toRd.shiny.tag}{td}(obj, ..., warn.info.loss="none")
\method{toRd.shiny.tag}{th}(obj, ..., warn.info.loss="message")
## Converters ==========================================================
\method{toRd.shiny.tag}{aside}(obj, ...)
\method{toRd.shiny.tag}{em}(obj, ...)
\method{toRd.shiny.tag}{cite}(obj, ...)
\method{toRd.shiny.tag}{kbd}(obj, ...)
\method{toRd.shiny.tag}{pre}(obj, ...)
\method{toRd.shiny.tag}{q}(obj, ...)
\method{toRd.shiny.tag}{samp}(obj, ...)
\method{toRd.shiny.tag}{strong}(obj, ...)
}
\arguments{
    \item{obj}{a \code{shiny.tag} object.}

    \item{...}{passed to methods.}

    \item{warn.info.loss}{should warning be issued when information
                might be lost. Note that defaults are different for
                different methods.}

    \item{abbr.check.caps}{Check is abbreviations in all caps.}
    \item{col.align}{Column alignment for tables.}
    \item{head}{Used internally to indicate if a child of a \code{<thead>} node.}
    \item{sub.section}{Used internally for nested \code{<section>}s
            and \code{<div>}s.}
}
\description{
Methods for \code{\link[=toRd.shiny.tag]{toRd.shiny.tag()}}.
}
\section{Methods (by HTML tag)}{
\subsection{Special Methods}{
  These are defined for completeness but always produce errors.

  \itemize{
    \item \code{toRd.shiny.tag.}\strong{\code{default}:} Generates informative error, but should not
            be called in the normal course of execution.
    \item \code{toRd.shiny.tag.shiny.}\strong{\code{tag}:} If no method exists to explicitly handle
            the tag an error is generated.
  }
}
\subsection{Defined Methods}{\itemize{
    \item \code{toRd.shiny.tag.}\strong{\code{a}:} \code{<a>} → \code{\\link} if a link
             to another help file, or → \code{\\href} if a url scheme
             is detected, and may throw an error if the link cannot be
             determined to be either.
    \item \code{toRd.shiny.tag.}\strong{\code{abbr}:} \code{<abbr>} → \code{\\acronym}.
            May not have children.
            Will warn if abbreviation is not in all caps,
            use \code{abbr.check.caps=FALSE} to disable.
    \item \code{toRd.shiny.tag.}\strong{\code{br}:} \code{<br>} → \code{\\cr}.
            May not have children.
    \item \code{toRd.shiny.tag.}\strong{\code{code}:} \code{<code>} → \code{\\code}.
    \item \code{toRd.shiny.tag.}\strong{\code{div}:} See \code{toRd.shiny.tag.}\strong{\code{section}}.
    \item \code{toRd.shiny.tag.}\strong{\code{dfn}:} \code{<dfn>} → \code{\\dfn}.
            Must have a single plain text element.
    \item \code{toRd.shiny.tag.}\strong{\code{dd}:} Used with \code{<dl>} extraction.
    \item \code{toRd.shiny.tag.}\strong{\code{dt}:} Used with \code{<dt>} extraction.
    \item \code{toRd.shiny.tag.}\strong{\code{dl}:}
            \code{<dl>}
                \code{<dt>}\emph{\code{term}}\code{</dt>}
                \code{<dd>}\emph{\code{definition}}\code{</dd>}
                \code{\dots}
            \code{</dl>} →
            \code{\\describe\{}
                \code{\\item\{}\emph{\code{term}}\code{\}}
                \code{\{}\emph{\code{definition}}\code{\}\}}.
    \item \code{toRd.shiny.tag.}\strong{\code{html}:} \code{<html>} → \code{Rd} container.
    \item \code{toRd.shiny.tag.}\strong{\code{img}:} \code{<img>} → \code{\\figure}.
            HTML must contain a \code{"src"} attribute.
    \item \code{toRd.shiny.tag.}\strong{\code{li}:} \code{<li>} → \code{\\item}.
    \item \code{toRd.shiny.tag.}\strong{\code{p}:} \code{<p>} → Rd container of
            content with two newlines appended for paragraph break.
    \item \code{toRd.shiny.tag.}\strong{\code{ol}:} \code{<ol>} → \code{\\enumerate}.
            May only contain \code{<li>} nodes.
    \item \code{toRd.shiny.tag.}\strong{\code{section}:} \code{<section>} → \code{\\section} or
            \code{\\subsection}, depending on nesting.
            Must be able to determine a title, otherwise will
            generate an error.
    \item \code{toRd.shiny.tag.}\strong{\code{table}:} \code{<table>} → \code{\\tabular}.
            Table rows are separated with \code{\\cr} and columns within
            rows by \code{\\tab}.
            May only contain \code{<thead>}, \code{<tbody>}, and
            \code{<tfoot>} type nodes, and \strong{must} contain a
            \code{<tbody>} node.
            Note that column and row spans are not currently supported.
    \item \code{toRd.shiny.tag.}\strong{\code{tr}:} Table row.
            Used with \code{<thead>}, \code{<tbody>}, and \code{<tfoot>}.
            Cells of table row are returned in an \code{Rd} container
            separated by \code{\\tab} tags.
            May only contain \code{<td>} nodes unless it is contained in
            a \code{<thead>} node then must contain \code{<th>} nodes.
    \item \code{toRd.shiny.tag.}\strong{\code{tbody}:} Table body.
            Returns rows of the table body in an \code{Rd} container
            separated by \code{\\cr} tags.
            May only contain \code{<tr>} nodes.
    \item \code{toRd.shiny.tag.}\strong{\code{tfoot}:} Table footer.
            Same as \code{tbody} with the added message warning that
            information will be lost as the portion of the table that
            was the footer will no longer be identifiable.
    \item \code{toRd.shiny.tag.}\strong{\code{thead}:} Table header.
            Same as \code{tbody} and and \code{tfoot} except the
            information loss message is given as a warning.
    \item \code{toRd.shiny.tag.}\strong{\code{ul}:} \code{<ul>} → \code{\\itemize}.
            May only contain \code{<li>} nodes.
}}
\subsection{Generated Methods}{
  These methods are \link[=toRd.shiny.tag-generators]{generated}.

  \emph{Extractors:} Only extract the contents but do not wrap
  in an Rd tag.
  \itemize{
    \item \code{toRd.shiny.tag.}\strong{\code{h1}:} extract with warning
    \item \code{toRd.shiny.tag.}\strong{\code{h2}:} extract with warning
    \item \code{toRd.shiny.tag.}\strong{\code{h3}:} extract with warning
    \item \code{toRd.shiny.tag.}\strong{\code{h4}:} extract with warning
    \item \code{toRd.shiny.tag.}\strong{\code{h5}:} extract with warning
    \item \code{toRd.shiny.tag.}\strong{\code{span}:} extract with warning
    \item \code{toRd.shiny.tag.}\strong{\code{td}:} extracts silently by default.
    \item \code{toRd.shiny.tag.}\strong{\code{th}:} extract, but with a message.
  }
  \emph{Converters:} Convert simply from an HTML tag to an Rd tag.
  \itemize{
    \item \code{toRd.shiny.tag.}\strong{\code{aside}:}  \code{<aside>} → \code{\\note}
    \item \code{toRd.shiny.tag.}\strong{\code{em}:} \code{<em>} → \code{\\emph}
    \item \code{toRd.shiny.tag.}\strong{\code{cite}:}  \code{<cite>} → \code{\\cite}
    \item \code{toRd.shiny.tag.}\strong{\code{kbd}:}  \code{<kbd>} → \code{\\kbd}
    \item \code{toRd.shiny.tag.}\strong{\code{pre}:}  \code{<pre>} → \code{\\preformatted}
    \item \code{toRd.shiny.tag.}\strong{\code{q}:}  \code{<q>} → \code{\\dQuote}
    \item \code{toRd.shiny.tag.}\strong{\code{samp}:}  \code{<samp>} → \code{\\preformatted}
    \item \code{toRd.shiny.tag.}\strong{\code{strong}:}  \code{<strong>} → \code{\\strong}
 }
}
}
\seealso{\itemize{
\item Root generic: \code{\link[Rd]{toRd}}
\item Sub Generic: \code{\link{toRd.shiny.tag}}
\item Generators: \code{\link{toRd.shiny.tag-generators}}
}}
\concept{Rd}
