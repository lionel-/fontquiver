
check_font_family <- function(face, style, files) {
  faces <- names(files)
  if (!face %in% faces) {
    stop(call. = FALSE,
      "Available faces:\n",
      paste(faces, collapse = ", ")
    )
  }

  if (!style %in% names(files[[face]])) {
    stop(call. = FALSE,
      "Available styles for face ", face, ":\n",
      paste(names(files[[face]]), collapse = ", ")
    )
  }
}

check_font_ext <- function(ext) {
  if (!ext %in% c("ttf", "woff")) {
    stop("`ext` must be either 'ttf' or 'woff'", call. = FALSE)
  }
}

check_font_exists <- function(font, package) {
  dir <- system.file("fonts", paste0(font, "-fonts"), package = package)
  dir.exists(dir)
}

font_file <- function(base, name, ext, package) {
  dir <- paste0(base, "-fonts")
  filename <- paste(name, ext, sep = ".")
  path <- file.path(dir, filename)

  file <- system.file("fonts", path, package = package)
  if (file == "") {
    stop("Internal error: cannot find font", filename)
  }
  file
}

font_version <- function(font, package) {
  file <- system.file("fonts", paste0(font, "-VERSION"), package = package)
  readChar(file, file.info(file)$size - 1)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

str_standardise <- function(name) {
  gsub(" ", "-", tolower(name))
}

str_trim_ext <- function(path) {
  sub("\\..+$", "", path)
}
