


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create stipple svg pattern
#'
#' @inheritParams create_stripe_pattern
#'
#' @return minisvg::SVGPattern object
#'
#' @import minisvg
#' @import glue
#' @importFrom poissoned poisson_disc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_stipple_pattern <- function(id,
                                   spacing       = 20,
                                   fill_fraction = 0.2,
                                   alpha         = 1.0,
                                   colour        = '#000000',
                                   fg_alpha      = 1, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Spacing corresponds to cell size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cell_size <- spacing
  nrows     <- 20
  ncols     <- 20
  width     <- ncols * cell_size
  height    <- nrows * cell_size


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generate a bunch of points using the 'poissoned' package
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pts   <- poissoned::poisson_disc(ncols         = ncols,
                                   nrows         = nrows,
                                   cell_size     = cell_size,
                                   k             = 20,
                                   keep_boundary = TRUE,
                                   verbose       = FALSE)
  pts$x <- round(pts$x, 2)
  pts$y <- round(pts$y, 2)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # fill_fraction influences radius
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r <- spacing * fill_fraction

  inner_style <- glue::glue("fill: {colour}; stroke: {colour}; fill-opacity: {fg_alpha}; stroke-opacity: {fg_alpha}")

  circles <- glue::glue("<circle cx='{round(pts$x, 2)}' cy='{round(pts$y, 2)}' r='{r}' style='{inner_style}' />")
  circles <- paste(circles, collapse = "\n")
  circles <- minisvg::stag$g(
    circles
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pattern
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rect_style  <- glue::glue('fill:#ffffff; fill-opacity:{alpha}; stroke:none;')

  pattern <- minisvg::svg_pattern(
    id               = id,
    width            = width,
    height           = height,
    patternUnits     = 'userSpaceOnUse',
    minisvg::stag$rect(
      width  = '100%',
      height = '100%',
      style  = rect_style
    ),
    circles
  )




  pattern
}


