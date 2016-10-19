#' @references \url{http://dejavu-fonts.org}
#' @include fontset.R
fontset_dejavu <- function(variant = "sans", style = "book") {
  if (system.file("fonts", package = "fontDejaVu") == "") {
    stop(call. = FALSE,
      "fontDejaVu must be installed from Github. Please run:\n",
      "  install.packages('devtools')\n",
      "  devtools::install_github('lionel-/fontDejaVu')"
    )
  }
  font_get("DejaVu", variant, style, pkg = "fontDejaVu")
}

fontset_dejavu_files <- list(
  sans = list(
    book = sans("ttf/DejaVuSans", face = "plain", weight = 80),
    oblique = sans("ttf/DejaVuSans-Oblique", face = "italic", weight = 80),
    bold = sans("ttf/DejaVuSans-Bold", face = "bold", weight = 200),
    `bold-oblique` = sans("ttf/DejaVuSans-BoldOblique", face = "bolditalic", weight = 200),
    `extra-light` = sans("ttf/DejaVuSans-ExtraLight", weight = 40)
  ),
  `sans-condensed` = list(
    book = other("ttf/DejaVuSansCondensed", face = "plain", weight = 80),
    oblique = other("ttf/DejaVuSansCondensed-Oblique", face = "italic", weight = 80),
    bold = other("ttf/DejaVuSansCondensed-Bold", face = "bold", weight = 200),
    `bold-oblique` = other("ttf/DejaVuSansCondensed-BoldOblique", face = "bolditalic", weight = 200)
  ),
  `sans-mono` = list(
    book = mono("ttf/DejaVuSansMono", face = "plain", weight = 80),
    oblique = mono("ttf/DejaVuSansMono-Oblique", face = "italic", weight = 80),
    bold = mono("ttf/DejaVuSansMono-Bold", face = "bold", weight = 200),
    `bold-oblique` = mono("ttf/DejaVuSansMono-BoldOblique", face = "bolditalic", weight = 200)
  ),
  serif = list(
    book = serif("ttf/DejaVuSerif", face = "plain", weight = 80),
    italic = serif("ttf/DejaVuSerif-Italic", face = "italic", weight = 80),
    bold = serif("ttf/DejaVuSerif-Bold", face = "bold", weight = 200),
    `bold-italic` = serif("ttf/DejaVuSerif-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `serif-condensed` = list(
    book = other("ttf/DejaVuSerifCondensed", face = "plain", weight = 80),
    italic = other("ttf/DejaVuSerifCondensed-Italic", face = "italic", weight = 80),
    bold = other("ttf/DejaVuSerifCondensed-Bold", face = "bold", weight = 200),
    `bold-italic` = other("ttf/DejaVuSerifCondensed-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `math-tex-gyre` = list(
    regular = other("ttf/DejaVuMathTexGyre", face = "plain", weight = 80)
  )
)

fontset_register("DejaVu",
  getter = fontset_dejavu,
  files = fontset_dejavu_files
)
