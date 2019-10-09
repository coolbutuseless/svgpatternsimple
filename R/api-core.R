


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How bits are packed for simple patterns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_pack_specs <- list(
  name_only = list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    padding        = list(type = 'integer', nbits = 29)
  ),
  stripe = list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    angle          = list(type = 'custom' , nbits =  2, pack_func = ~round((.x %% (3.5/4*180)) / 45), unpack_func = ~.x * 45),
    spacing        = list(type = 'scaled' , nbits =  4, max = 255),
    fill_fraction  = list(type = 'scaled' , nbits =  4, max = 1),
    fg_alpha       = list(type = 'scaled' , nbits =  3, max = 1),
    colour         = list(type = 'colour' , nbits =  8),
    alpha          = list(type = 'scaled' , nbits =  8, max = 1)
  ),
  dot = list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    angle          = list(type = 'custom' , nbits =  2, pack_func = ~round((.x %% (3.5/4*90)) / 22.5), unpack_func = ~.x * 22.5),
    spacing        = list(type = 'scaled' , nbits =  4, max = 255),
    fill_fraction  = list(type = 'scaled' , nbits =  4, max = 1),
    fg_alpha       = list(type = 'scaled' , nbits =  3, max = 1),
    colour         = list(type = 'colour' , nbits =  8),
    alpha          = list(type = 'scaled' , nbits =  8, max = 1)
  ),
  stipple =  list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    colour         = list(type = 'colour' , nbits =  8),
    spacing        = list(type = 'scaled' , nbits =  5, max = 255),
    fill_fraction  = list(type = 'scaled' , nbits =  4, max = 1),
    fg_alpha       = list(type = 'scaled' , nbits =  4, max = 1),
    alpha          = list(type = 'scaled' , nbits =  8, max = 1)
  ),
  gradient = list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    angle          = list(type = 'scaled' , nbits =  5, max = 360, cyclical = TRUE),
    colour1        = list(type = 'colour' , nbits =  8),
    colour2        = list(type = 'colour' , nbits =  8),
    alpha          = list(type = 'scaled' , nbits =  8, max = 1)
  ),
  check = list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    angle          = list(type = 'scaled' , nbits =  6, max = 90, cyclical = TRUE),
    spacing        = list(type = 'scaled' , nbits =  4, max = 255),
    fg_alpha       = list(type = 'scaled' , nbits =  3, max = 1),
    colour         = list(type = 'colour' , nbits =  8),
    alpha          = list(type = 'scaled' , nbits =  8, max = 1)
  ),
  null = list(
    pattern_name   = list(type = 'choice' , nbits =  3, options = valid_pattern_names),
    colour         = list(type = 'colour' , nbits =  21),
    alpha          = list(type = 'scaled' , nbits =  8, max = 1)
  )
)

all_pack_specs[['hatch'  ]] <- all_pack_specs[['dot']]
all_pack_specs[['hex'    ]] <- all_pack_specs[['dot']]


all_defaults <- list(
  stripe = list(
    pattern_name  = 'stripe',
    angle         = 45,
    spacing       = 20,
    fill_fraction = 0.2,
    fg_alpha      = 1,
    colour        = '#000000',
    alpha         = 1
  ),
  dot = list(
    pattern_name  = 'dot',
    angle         = 22.5,
    spacing       = 20,
    fill_fraction = 0.2,
    fg_alpha      = 1,
    colour        = '#000000',
    alpha         = 1
  ),
  hatch = list(
    pattern_name  = 'hatch',
    angle         = 22.5,
    spacing       = 20,
    fill_fraction = 0.2,
    fg_alpha      = 1,
    colour        = '#000000',
    alpha         = 1
  ),
  hex = list(
    pattern_name  = 'hex',
    angle         = 22.5,
    spacing       = 20,
    fill_fraction = 0.2,
    fg_alpha      = 1,
    colour        = '#000000',
    alpha         = 1
  ),
  stipple = list(
    pattern_name  = 'stipple',
    spacing       = 20,
    fill_fraction = 0.2,
    fg_alpha      = 1,
    colour        = '#000000',
    alpha         = 1
  ),
  gradient = list(
    pattern_name = 'gradient',
    angle        = 45,
    colour1      = '#00204D',
    colour2      = '#FFEA46',
    alpha        = 1
  ),
  check = list(
    pattern_name  = 'check',
    id            = NULL,
    angle         = 45,
    spacing       = 20,
    fg_alpha      = 1.0,
    colour        = '#000000',
    alpha         = 1.0
  ),
  null = list(
    colour = '#ffffff',
    alpha  = 1
  )
)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the rgba_vec interpretable as a pattern?
#'
#' @param rgba_vec 4 element vector of RGBA values, each in the range [0, 255]
#'
#' @return logical value indicating whether or not thig RGBA colour is
#'         interpretable as a pattern in this package?
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_valid_pattern_encoding <- function(rgba_vec) {
  is_black <- function(vec) {
    vec[1] == 0 && vec[2] == 0 && vec[3] == 0
  }

  is_white <- function(vec) {
    vec[1] == 255 && vec[2] == 255 && vec[3] == 255
  }

  is_transparent <- function(vec) {
    vec[4] == 0
  }


  !(is_black(rgba_vec) || is_white(rgba_vec) || is_transparent(rgba_vec))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Encode a pattern as a hex colour
#'
#' @param ... pattern parameters
#'
#' @return Return a hex colour representing the parameters
#'
#' @import lofi
#' @importFrom utils modifyList
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
encode_pattern_params_as_hex_colour <- function(...) {
  params <- list(...)
  if (!'pattern_name' %in% names(params)) {
    stop("'svgpatternsimple' needs a 'pattern_name' argument. None given.", call. = FALSE)
  }

  pattern_name <- params$pattern_name
  pack_spec    <- all_pack_specs[[pattern_name]]
  defaults     <- all_defaults[[pattern_name]]

  values <- modifyList(defaults, params)

  int32  <- lofi::pack(values, pack_spec)
  lofi::int32_to_hex_colour(int32)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create the pattern id for a given hex colour for this package
#'
#' @param rgba_vec 4 element RGBA colour vector. Each element in range [0, 255]
#'
#' @return character string to use as a pattern ID
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_id_from_rgba_vec <- function(rgba_vec) {
  hex_colour <- lofi::rgba_vec_to_hex_colour(rgba_vec)
  paste0("simple-", substr(hex_colour, 2, 9))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decode a 4-element RGBA colour vector as an SVG pattern
#'
#' @param rgba_vec 4 element RGBA colour vector. Each element in range [0, 255]
#'
#' @return svg_pattern object
#'
#' @import lofi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decode_pattern_from_rgba_vec <- function(rgba_vec) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Need an int32 so we can unpack
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hex_colour <- lofi::rgba_vec_to_hex_colour(rgba_vec)
  int32      <- lofi::rgba_vec_to_int32(rgba_vec)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out which pattern this is by unpacking the name only
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  name_only_pack_spec <- all_pack_specs[['name_only']]
  pattern_name        <- lofi::unpack(int32, name_only_pack_spec)[['pattern_name']]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Select the correct pattern pack_spec and unpack the int32
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pack_spec <- all_pack_specs[[pattern_name]]
  params    <- lofi::unpack(int32, pack_spec)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure we have a sane ID attached to this pattern
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  params$id  <- create_pattern_id_from_rgba_vec(rgba_vec)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Select the function for creating the pattern
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- switch(
    params$pattern_name,
    stripe   = create_stripe_pattern  ,
    hatch    = create_hatch_pattern   ,
    dot      = create_dot_pattern     ,
    stipple  = create_stipple_pattern ,
    hex      = create_hex_pattern     ,
    null     = create_null_pattern    ,
    gradient = create_gradient_pattern,
    check    = create_check_pattern   ,
    stop("decode_pattern_from_hex_colour(): Unknown pattern_name: ", deparse(params$pattern_name), call. = FALSE)
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pattern from the params and return it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  do.call(f, params)
}





