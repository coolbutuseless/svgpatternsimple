

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an SVG checkerboard pattern
#'
#' Create an SVG pattern which will \code{fill} an element with checks.
#'
#' @inheritParams create_pattern_stripe
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Create an SVG document
#' library(minisvg)
#' doc   <- minisvg::svg_doc()
#'
#' # Create the pattern and add to the SVG definitions
#' my_pattern <- create_pattern_check(id = 'mypattern')
#' doc$defs(my_pattern)
#'
#' # Create a rectangle with the animation
#' rect  <- stag$rect(
#'   x      = "10%",
#'   y      = "10%",
#'   width  = "80%",
#'   height = "80%",
#'   stroke = 'black',
#'   fill   = my_pattern
#' )
#'
#' # Add this rectangle to the document, show the SVG text, then render it
#' doc$append(rect)
#' doc
#' doc$show()
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_check <- function(id,
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

