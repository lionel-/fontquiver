#' Get elements of a fontset
#'
#' \code{fonts()} returns the list of all \code{font} objects for a
#' given fontset (a collection of fonts provided by fontquiver, such
#' as \code{Liberation} or \code{Bitstream Vera}). \code{font()}
#' returns one specific font. \code{font_symbol()} extracts the font
#' marked as symbol font, if it exists in the fontset.
#' @inheritParams fontset_info
#' @param variant Font variant, as per Fontconfig's nomenclature. Use
#'   \code{\link{fontset_variants}()} to find out which variants are
#'   available for a fontset.
#' @param style Font style, as per Fontconfig's nomenclature. Use
#'   \code{\link{fontset_styles}()} to find out which variants are
#'   available for a fontset.
#' @export
#' @examples
#' font("Bitstream Vera", "Sans", "Roman")
#'
#' f <- fonts("Liberation")
#' str(f, 1)
#'
#' font_symbol("Symbola")
fonts <- function(fontset) {
  info <- fontset_info(fontset)
  getter <- fontset_props(fontset)$getter

  fonts <- Map(getter, variant = info$variant, style = info$style)
  fonts_names <- flatten(Map(paste, info$variant, style = info$style, sep = "-"))
  names(fonts) <- vapply_chr(fonts_names, str_prettify)

  fonts
}

#' @rdname fonts
#' @export
font <- function(fontset, variant, style) {
  fontset_props(fontset)$getter(variant, style)
}

#' @rdname fonts
#' @export
font_symbol <- function(fontset = "Symbola") {
  families <- font_families(fontset)
  families$symbol$symbol
}

#' Font families and faces
#'
#' Returns families or faces as font files for a given font set. The
#' files are organised by R nomenclature of families (\code{"sans"},
#' \code{"serif"}, \code{"mono"}, and \code{"symbol"}) and faces
#' (\code{"plain"}, \code{"italic"}, \code{"bold"}, and
#' \code{"bolditalic"}). When a font does not have a combination,
#' \code{NA} is reported.
#'
#' Note that the fonts returned by \code{font_faces()} and
#' \code{\link{font_families}()} are constrained by R's nomenclature
#' of fonts and are thus a subset of those returned by
#' \code{\link{font_variants}()} and \code{\link{font_styles}()}.
#' @inheritParams fontset_info
#' @param family One of \code{"sans"}, \code{"serif"}, \code{"mono"}
#'   or \code{"symbol"}.
#' @export
#' @seealso \code{\link{font_variants}()}, \code{\link{fonts}()}
#' @examples
#' font_families("Bitstream Vera")$mono$bold
#' font_faces("Bitstream Vera", "mono")
font_families <- function(fontset) {
  info <- fontset_info(fontset)
  props <- fontset_props(fontset)
  getter <- props$getter

  filter_file <- function(...) {
    filtered <- filter_first(info, ..., ~!is.na(family), ~!is.na(face))
    if (nrow(filtered)) {
      do.call(getter, filtered[c("variant", "style")])
    } else {
      NULL
    }
  }

  fnames <- set_names(c("sans", "serif", "mono", "symbol"))
  families <- lapply(fnames, function(r_family) {
    family <- list(
      plain = filter_file(~family == r_family, ~face == "plain"),
      italic = filter_file(~family == r_family, ~face == "italic"),
      bold = filter_file(~family == r_family, ~face == "bold"),
      bolditalic = filter_file(~family == r_family, ~face == "bolditalic"),
      symbol = filter_file(~family == r_family, ~face == "symbol")
    )
    structure(family, class = "font_faces")
  })

  structure(families, class = "font_families")
}

#' @rdname font_families
#' @export
font_faces <- function(fontset, family = c("sans", "serif", "mono", "symbol")) {
  choices <- c("sans", "serif", "mono", "symbol")
  family <- match.arg(str_standardise(family), choices)
  families <- font_families(fontset)
  families[[family]]
}

#' Font variants
#'
#' \code{font_variants()} returns a tree of fonts organised as per
#' fontconfig's nomenclature. The first level contains variants of a
#' font and the second level contains font styles.
#'
#' See also \code{\link{font_families}()} and \code{\link{font_faces}()}
#' for similar collections organised according to R nomenclature of
#' fonts. Note that variants and styles are a super set of R families
#' and faces and may contain more fonts.
#'
#' @inheritParams font
#' @seealso \code{\link{font_families}()}, \code{\link{fonts}()}
#' @export
#' @examples
#' font_variants("Bitstream Vera")
#' font_variants("Bitstream Vera")$Sans$Oblique
#' font_styles("Bitstream Vera", "Sans")
font_variants <- function(fontset) {
  props <- fontset_props(fontset)
  names(props$files) <- str_prettify(names(props$files))

  variants <- Map(function(variant, variant_name) {
    styles <- set_names(str_prettify(names(variant)))
    styles <- lapply(styles, props$getter, variant = variant_name)
    structure(styles, class = "font_styles")
  }, props$files, names(props$files))

  structure(variants, class = "font_variants")
}

#' @rdname font_variants
#' @export
font_styles <- function(fontset, variant) {
  variant <- str_prettify(variant)
  variants <- font_variants(fontset)
  variants[[variant]]
}
