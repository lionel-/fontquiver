#' @references \url{https://fedorahosted.org/liberation-fonts/}
#' @import fontLiberation
#' @include fontset.R
fontset_liberation <- function(variant = "sans", style = "regular") {
  font_get("Liberation", variant, style, pkg = "fontLiberation")
}

fontset_liberation_files <- list(
  sans = list(
    regular = sans("LiberationSans-Regular", face = "plain", weight = 80),
    italic = sans("LiberationSans-Italic", face = "italic", weight = 80),
    bold = sans("LiberationSans-Bold", face = "bold", weight = 200),
    `bold-italic` = sans("LiberationSans-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `mono` = list(
    regular = mono("LiberationMono-Regular", face = "plain", weight = 80),
    oblique = mono("LiberationMono-Italic", face = "italic", weight = 80),
    bold = mono("LiberationMono-Bold", face = "bold", weight = 200),
    `bold-italic` = mono("LiberationMono-BoldItalic", face = "bolditalic", weight = 200)
  ),
  serif = list(
    regular = serif("LiberationSerif-Regular", face = "plain", weight = 80),
    italic = serif("LiberationSerif-Italic", face = "italic", weight = 80),
    bold = serif("LiberationSerif-Bold", face = "bold", weight = 200),
    `bold-italic` = serif("LiberationSerif-BoldItalic", face = "bolditalic", weight = 200)
  )
)

fontset_register("Liberation",
  getter = fontset_liberation,
  files = fontset_liberation_files
)
