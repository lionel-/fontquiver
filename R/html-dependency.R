#' Include font as CSS dependency
#'
#' @inheritParams font_bitstream_vera
#' @param font_getter A fontquiver getter function such as
#'   \code{font_bistream_vera}.
#' @export
#' @examples
#' # Create an htmlDependency object:
#' dep <- htmlFontDependency(variant = "serif")
#'
#' # Check the name of the embedded font
#' dep$name
#'
#' # Use that name in your dependent css or html files. For example:
#' # body {
#' #  font-family: fontquiver-bitstream-vera-sans-roman, sans-serif;
#' # }
htmlFontDependency <- function(font_getter = font_bitstream_vera,
                               variant = NULL, style = NULL) {
  if (!requireNamespace("htmltools")) {
    stop("htmltools is not installed", call. = FALSE)
  }

  variant <- variant %||% formals(font_getter)$variant
  style <- style %||% formals(font_getter)$style
  font <- font_getter(variant, style, "ttf")
  name <- str_standardise(font$name)
  id <- paste("fontquiver", name, variant, style, sep = "-")

  base_file <- str_trim_ext(basename(font$file))
  css_dir <- file.path(tempdir(), "fontquiver")
  if (!dir.exists(css_dir)) {
    dir.create(css_dir)
  }
  css_file <- css_font_family(id, base_file, css_dir)

  base_path <- str_trim_ext(font$file)
  lapply(c("ttf", "svg", "woff", "eot"), function(ext) {
    from <- paste(base_path, ext, sep = ".")
    to <- file.path(css_dir, paste(base_file, ext, sep = "."))
    file.copy(from, to)
  })

  htmltools::htmlDependency(
    name = id,
    version = font$version,
    src = css_dir,
    stylesheet = basename(css_file)
  )
}

css_font_family <- function(id, file, css_dir) {
  lines <- gsub("%s", file, c(
    "@fontface {",
    sprintf("  font-family: '%s';", id),
    "  src: url('%s.eot');                                    /* IE9 Compat Modes */",
    "  src: url('%s.eot?#iefix') format('embedded-opentype'), /* IE6-IE8 */",
    "  url('%s.woff') format('woff'),                         /* Modern Browsers */",
    "  url('%s.ttf')  format('truetype'),                     /* Safari, Android, iOS */",
    "  url('%s.svg#svgFontName') format('svg');               /* Legacy iOS */",
    "}"
  ))

  css_file <- tempfile(id, css_dir, fileext = ".css")
  writeLines(lines, css_file, useBytes = TRUE)
  css_file
}
