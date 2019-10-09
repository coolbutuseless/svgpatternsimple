

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a pure colour svg pattern
#'
#' @inheritParams create_stripe_pattern
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_null_pattern <- function(id, colour = '#ffffff', alpha = 1, ...) {

  style <- glue::glue('fill:{colour}; fill-opacity:{alpha}; stroke:none;')

  pattern <- minisvg::svg_pattern(
    id           = id,
    width        = 100,
    height       = 100,
    patternUnits = 'userSpaceOnUse',
    minisvg::stag$rect(
      width  = '100%',
      height = '100%',
      style  = style
    )
  )

  pattern
}

