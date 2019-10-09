

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create dot svg pattern
#'
#' @inheritParams create_stripe_pattern
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_dot_pattern <- function(id,
                               angle         = 45,
                               spacing       = 20,
                               fill_fraction = 0.2,
                               alpha         = 1.0,
                               fg_alpha      = 1.0,
                               colour        = '#000000',
                               ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert fill_fraction into a radius
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spacing <- max(spacing, 5)
  radius  <- fill_fraction * spacing / 2.1


  rect_style  <- glue::glue('fill:#ffffff; fill-opacity:{alpha}; stroke:none;')
  inner_style <- glue::glue('stroke:{colour}; fill:{colour}; fill-opacity:{fg_alpha}; stroke-opacity:{fg_alpha};')

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
    minisvg::stag$circle(
      cx    = round(spacing/2, 2),
      cy    = round(spacing/2, 2),
      r     = round(radius   , 2),
      style = inner_style
    )
  )

  pattern
}









