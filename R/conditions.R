# Condition Utilities ==========================================================
warn_html5_discouraged <- skip_scope(function(tag, alt=NULL){
    pkg_warning(._("Use of HTML direct formatting tags %s is discouraged.", dQuote(tag)) %<<%
                (if (!is.null(alt))
                    ._("We recommend using the tag %s as an alternate", dQuote(alt)) )
               , type = "discouraged_tag")
})
error_html5_malformed <- skip_scope(function(tag, msg=NULL){
    pkg_error(._("Malformed HTML in tag %s:", dQuote(tag)) %<<% msg
             , type = "malformed_html")
})
html_conversion_information_loss <- skip_scope(function(tag, cond='warning'){
    condition( type = 'info_loss'
               , cond = cond
               , ._('Extracting text from HTML tag %s, information will be lost'
                    , sQuote(tag))
    )
})
