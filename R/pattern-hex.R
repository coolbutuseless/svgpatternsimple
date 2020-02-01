


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an SVG hex pattern
#'
#' Create an SVG pattern which will \code{fill} an element with hexes.
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
#' my_pattern <- create_pattern_hex(id = 'mypattern')
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
create_pattern_hex <- function(id,
                               angle         = 0,
                               spacing       = 20,
                               fill_fraction = 0.01,
                               alpha         = 1.0,
                               fg_alpha      = 1.0,
                               colour        = '#000000',
                               ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert fill_fraction into a line thickness
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lt <- fill_fraction * spacing * 1.6
  lt <- max(lt, 1)
  spacing <- max(spacing, 2)

  w <- sqrt(3) * spacing
  h <- 2       * spacing

  qh <- h/4
  hw <- w/2

  rect_style  <- glue::glue('fill:#ffffff; fill-opacity:{alpha}; stroke:none;')
  inner_style <- glue::glue('stroke:{colour}; stroke-width:{lt}; stroke-opacity:{fg_alpha};')

  pattern <- minisvg::svg_pattern(
    id               = id,
    width            = w,
    height           = 6*qh,
    patternTransform = glue::glue('rotate({angle} 0 0)'),
    patternUnits     = 'userSpaceOnUse',
    minisvg::stag$rect(
      width  = '100%',
      height = '100%',
      style  = rect_style
    ),

    minisvg::stag$g(
      style = inner_style,
      minisvg::stag$line(x1=hw, y1=qh  , x2=0 , y2=2*qh),
      minisvg::stag$line(x1=hw, y1=qh  , x2=w , y2=2*qh),
      minisvg::stag$line(x1=hw, y1=qh*5, x2=0 , y2=4*qh),
      minisvg::stag$line(x1=hw, y1=qh*5, x2=w , y2=4*qh),
      minisvg::stag$line(x1=hw, y1=0   , x2=hw, y2=1*qh),
      minisvg::stag$line(x1=hw, y1=qh*5, x2=hw, y2=6*qh),
      minisvg::stag$line(x1=0 , y1=qh*2, x2=0 , y2=4*qh),
      minisvg::stag$line(x1=w , y1=qh*2, x2=w , y2=4*qh)
    )
  )

  pattern
}

