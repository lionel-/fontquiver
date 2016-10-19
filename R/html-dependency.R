#' Include font as CSS dependency
#'
#' @inheritParams splice_fonts
#' @export
#' @examples
#' # Create an htmlDependency object:
#' dep <- htmlFontDependency(font_families("Bitstream Vera"))
#'
#' # Use the fonts in your dependent css or html files. For example:
#' # body {
#' #  font-family: 'Bitstream Vera Sans Mono', courier;
#' # }
htmlFontDependency <- function(...) {
  if (!requireNamespace("htmltools")) {
    stop("htmltools is not installed", call. = FALSE)
  }
  fonts <- unique(splice_fonts(...))

  css_dir <- file.path(tempdir(), "fontquiver")
  if (!dir.exists(css_dir)) dir.create(css_dir)
  css_file <- tempfile("fontquiver-dep", css_dir, fileext = ".css")

  css <- lapply(fonts, function(font) {
    css_font_face(font$name, font$ttf, font$face, font$weight)
  })
  css <- flatten(css)
  writeLines(css, css_file, useBytes = TRUE)

  lapply(fonts, function(font) {
    file.copy(font$woff, file.path(css_dir, basename(font$woff)))
  })

  pkgs <- unique(vapply_chr(fonts, `[[`, "package"))
  vers <- unique(vapply_chr(fonts, `[[`, "version"))

  htmltools::htmlDependency(
    name = paste(pkgs, collapse = "-"),
    version = paste(vers, collapse = "-"),
    src = css_dir,
    stylesheet = basename(css_file)
  )
}

css_font_style <- function(face) {
  switch(face,
    italic = "italic",
    bolditalic = "italic",
    "normal"
  )
}

# Loosely adapted from
# https://lists.freedesktop.org/archives/fontconfig/2011-September/003645.html
css_font_weight <- function(w) {
  if (w <= 40) return(100)
  if (w <= 50) return(200)
  if (w <= 70) return(300)
  if (w <= 80) return(400)
  if (w <= 100) return(500)
  if (w <= 180) return(600)
  if (w <= 200) return(700)
  if (w <= 205) return(800)
  900
}

# https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face
css_font_face <- function(name, file, style, weight) {
  file <- str_trim_ext(basename(file))
  style <- css_font_style(style)
  weight <- css_font_weight(weight)
  gsub("%s", file, c(
    "@font-face {",
    sprintf("  font-family: '%s';", name),
    sprintf("  font-style: %s;", style),
    sprintf("  font-weight: %s;", weight),
    "  src:",
    "    url('%s.woff') format('woff');",
    "}\n"
  ))
}
