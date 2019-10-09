

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create stripe svg pattern
#'
#' @param angle rotation angle (degrees)
#' @param spacing space between features
#' @param fill_fraction size of features. In range [0, 1]
#' @param alpha default: 1.0 (opaque)
#' @param id id to use for pattern. If NULL then generate rando ID
#' @param fg_alpha alpha of the features
#' @param colour colour of the features. e.g. '#345678'
#' @param ... other arguments ignored
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_stripe_pattern <- function(id,
                                  angle         = 45,
                                  spacing       = 20,
                                  fill_fraction = 0.2,
                                  alpha         = 1.0,
                                  fg_alpha      = 1.0,
                                  colour        = '#000000',
                                  ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert fill_fraction into a line thickness
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  line_thickness <- fill_fraction * spacing * 2

  line_thickness <- max(line_thickness, 2)
  spacing        <- max(spacing, 2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pattern
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rect_style  <- glue::glue('fill:#ffffff; fill-opacity:{alpha}; stroke:none;')
  inner_style <- glue::glue('stroke:{colour}; stroke-width:{line_thickness}; stroke-opacity:{fg_alpha};')

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
    minisvg::stag$line(x1=0, y1=0, x2=0, y2=spacing, style = inner_style)
  )

  pattern
}

