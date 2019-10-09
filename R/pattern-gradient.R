
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create linear gradient svg pattern
#'
#' @inheritParams create_stripe_pattern
#' @param colour1,colour2 the start and end colours of the gradient
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_gradient_pattern <- function(id,
                                    angle         = 45,
                                    colour1       = '#ffffff',
                                    colour2       = '#000000',
                                    alpha         = 1.0,
                                    ...) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # invert angle to cope with inverted y axis in svg coords.
  # i.e. convert angle so clockwise from x axis to be positive
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle <- (360 - (angle %% 360)) %% 360
  tinc  <- tan((angle %% 45) * pi/180)
  tdec  <- 1 - tinc

  if (angle < 1 * 45) {
    x1 = 0; y1 = 0; x2 = 1; y2 = tinc;
  } else if (angle < 2 * 45) {
    x1 = 0; y1 = 0; x2 = tdec; y2 = 1;
  } else if (angle < 3 * 45) {
    x1 = 1; y1 = 0; x2 = tdec; y2 = 1;
  } else if (angle < 4 * 45) {
    x1 = 1; y1 = 0; x2 =    0; y2 = tdec;
  } else if (angle < 5 * 45) {
    x1 = 1; y1 = 1; x2 =    0; y2 = tdec;
  } else if (angle < 6 * 45) {
    x1 = 1; y1 = 1; x2 = tinc; y2 = 0;
  } else if (angle < 7 * 45) {
    x1 = 0; y1 = 1; x2 = tinc; y2 = 0;
  } else if (angle < 8 * 45) {
    x1 = 0; y1 = 1; x2 =    1; y2 = tinc;
  } else {
    x1 = 0; y1 = 0; x2 = 1; y2 = 1
  }

  # Format as percentages
  x1 <- paste0(round(x1 * 100, 2), '%')
  y1 <- paste0(round(y1 * 100, 2), '%')
  x2 <- paste0(round(x2 * 100, 2), '%')
  y2 <- paste0(round(y2 * 100, 2), '%')


  pattern <- minisvg::svg_pattern(
    name = 'linearGradient',
    id   = id,
    x1   = x1,
    y1   = y1,
    x2   = x2,
    y2   = y2,
    minisvg::stag$stop(offset =   "0%", style = glue::glue("stop-color:{colour1};stop-opacity:{alpha}")),
    minisvg::stag$stop(offset = "100%", style = glue::glue("stop-color:{colour2};stop-opacity:{alpha}"))
  )

  pattern
}





