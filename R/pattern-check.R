

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create checkerboard svg pattern
#'
#' @inheritParams create_stripe_pattern
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_check_pattern <- function(id,
                                 angle         = 45,
                                 spacing       = 20,
                                 alpha         = 1.0,
                                 fg_alpha      = 1.0,
                                 colour        = '#000000',
                                 ...) {

  spacing <- max(spacing, 2)
  w       <- spacing/2


  rect_style  <- glue::glue('fill:#ffffff; fill-opacity:{alpha}; stroke:none;')
  inner_style <- glue::glue('fill:{colour}; fill-opacity:{fg_alpha};')

  pattern <- minisvg::svg_pattern(
    id               = id,
    width            = spacing,
    height           = spacing,
    patternTransform = glue::glue('rotate({angle} 0 0)'),
    patternUnits     = 'userSpaceOnUse',
    minisvg::stag$rect(
      width  = '100%',
      height = '100%',
      style  = rect_style
    ),

    minisvg::stag$g(
      minisvg::stag$rect(width = w, height = w, x = 0, y = 0, style = inner_style),
      minisvg::stag$rect(width = w, height = w, x = w, y = w, style = inner_style)
    )
  )

  pattern
}

