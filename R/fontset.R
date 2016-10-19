#' Fontset metadata
#'
#' This returns a data frame containing metadata about fonts. The
#' columns "variant" and "style" correspond to the properties of the
#' font as reported by fontconfig. The columns "family", "italic" and
#' "bold" provide information in terms of R nomenclature of fonts. The
#' column "base" gives the base filename of the font. See
#' \code{\link{fontset_list}()} to obtain a list of available
#' fontsets.
#'
#' @param fontset A string giving the name of a set of fonts
#'   (e.g. \code{"Liberation"} or\code{"Bitstream Vera"}). Use
#'   \code{\link{fontset_list}()} to obtain the list of fontsets
#'   registered in your session.
#' @seealso \code{\link{fontset_list}()}, \code{\link{font_families}()},
#' \code{\link{font_variants}()}
#' @export
fontset_info <- function(fontset) {
  props <- fontset_props(fontset)
  info <- at_bottom(props$files, spread_attributes, "base")
  gather_tree(info, c("variant", "style"))
}

#' Fontset variants and styles
#'
#' These functions return the variants and the styles available for a
#' given fontset.
#' @inheritParams fonts
#' @export
#' @examples
#' fontset_variants("Liberation")
#' fontset_styles("Bitstream Vera", "Sans Mono")
fontset_variants <- function(fontset) {
  props <- fontset_props(fontset)
  str_prettify(names(props$files))
}

#' @rdname fontset_variants
#' @export
fontset_styles <- function(fontset, variant) {
  props <- fontset_props(fontset)
  variant <- str_standardise(variant)
  str_prettify(names(props$files[[variant]]))
}

registered_fontsets <- new.env(parent = emptyenv())

#' Register a font to fontquiver
#'
#' @param fontset Name of the font set.
#' @param getter Constructor of \code{font} objects.
#' @param files Data structure containing font information.
#' @export
fontset_register <- function(fontset, getter, files) {
  props <- list(getter = getter, files = files)
  assign(fontset, props, envir = registered_fontsets)
}

#' Get list of all registered fonts
#'
#' @export
fontset_list <- function() {
  names(registered_fontsets)
}

fontset_props <- function(fontset) {
  fontset <- str_prettify(fontset)
  get(fontset, envir = registered_fontsets)
}


# Constructors -------------------------------------------------------

font_data <- function(base, family, face, weight) {
  structure(base,
    family = family,
    face = face,
    weight = weight,
    class = "font_data"
  )
}

other <- function(base, face = NA, weight = NA) {
  font_data(base, family = NA, face = face, weight = weight)
}

sans <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "sans", face = face, weight = weight)
}

serif <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "serif", face = face, weight = weight)
}

mono <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "mono", face = face, weight = weight)
}

symbol <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "symbol", face = face, weight = weight)
}
