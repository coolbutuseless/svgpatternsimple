

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a pattern palette generator
#'
#' @param pattern_name the name of the pattern. one of 'stripe', 'dot', 'hatch', 'check', 'stipple', 'hex'
#' @param angle the vector of angles to choose from (order matters)
#' @param spacing the vector of spacing choices
#' @param fill_fraction the vector of fill_fraction choices
#'
#' @return a function suitable for a continuous or discrete colour palette generation
#'
#' @import purrr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gen_pattern_simple_pal_func <- function(pattern_name  = c('stripe', 'dot', 'hatch', 'check', 'stipple', 'hex'),
                                        angle         = c(22., 45, 67.5),
                                        spacing       = seq(5, 50, length.out = 7),
                                        fill_fraction = seq(0.1, 0.9, length.out = 3)) {

  Ncombos <- length(pattern_name) * length(angle) * length(spacing) * length(fill_fraction)

  if (Ncombos < 4) {
    warning("That's probably far too few combinations.")
  } else if (Ncombos > 1000) {
    stop("Too many combos. Limit is 1000. You requested: ", Ncombos)
  }

  pattern_ref_grid <- expand.grid(
    angle         = angle,
    spacing       = spacing,
    fill_fraction = fill_fraction,
    pattern_name  = pattern_name,
    stringsAsFactors = FALSE
  )

  pattern_ref_hex_colours <- purrr::pmap_chr(pattern_ref_grid, encode_pattern_params_as_hex_colour)

  function(n) {
    if (length(n) == 1 && round(n) == n) {
      # Discrete palette
      idx <- round(seq(1, length(pattern_ref_hex_colours), length.out = n))
      pattern_ref_hex_colours[idx]
    } else {
      # continuous palette
      n[is.na(n)] <- 0.5
      idx <- n * (length(pattern_ref_hex_colours) - 1) + 1
      pattern_ref_hex_colours[idx]
    }
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pattern fill scale for discrete colours
#'
#' @inheritParams gen_pattern_simple_pal_func
#' @param ... arguments passed to \code{ggplot2::discrete_scale()}
#' @param na.value Value to use if NA
#'
#' @importFrom ggplot2 discrete_scale
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_fill_pattern_simple <- function(...,
                                      pattern_name  = c('stripe', 'dot', 'hatch', 'check'),
                                      angle         = c(22., 45, 67.5, 135),
                                      spacing       = seq(10, 50, length.out = 5),
                                      fill_fraction = seq(0.1, 0.5, length.out = 5),
                                      na.value = 'grey50') {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Docs taken from ggplot2::discrete_scale
  # aesthetics - names of the aesthetics that this scale works with
  # scale_name - name of the scale that should be used for error messages
  # palette    - A palette function that when called with a single integer
  #              argument (the number of levels in the scale) returns the values that
  #              they should take (e.g., [scales::hue_pal()]).
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  discrete_scale(
    aesthetics = "fill",
    scale_name = "pattern_d",
    palette    = gen_pattern_simple_pal_func(pattern_name, angle, spacing, fill_fraction),
    na.value   = na.value,
    ...
  )
}

