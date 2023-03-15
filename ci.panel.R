
ci.panel = function(x, lower, upper, border) {
    if(border) {
        llines(x, lower, lty=3, col=2)
        llines(x, upper, lty=3, col=2)
    } else {
    panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                  col='lightgrey', border=FALSE)
    }
}

