
font_data <- function(base, family, face, weight) {
  structure(base,
    family = family,
    face = face,
    weight = weight,
    class = "font_data"
  )
}

font_extra <- function(base, face = NA, weight = NA) {
  font_data(base, family = NA, face = face, weight = weight)
}

font_sans <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "sans", face = face, weight = weight)
}

font_serif <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "serif", face = face, weight = weight)
}

font_mono <- function(base, face = NA, weight = NA) {
  font_data(base = base, family = "mono", face = face, weight = weight)
}

font_files <- function(getter, ...) {
  getter <- structure(getter, class = "font_getter")
  structure(list(...), getter = getter)
}

print.font_getter <- function(x, ...) {
  cat("fontgetter\n")
}
str.font_getter <- function(object, ...) {
  cat(" fontgetter\n")
}

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

# Loosely adapted from
# https://lists.freedesktop.org/archives/fontconfig/2011-September/003645.html
fontconfig_to_css_weight <- function(w) {
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

# R Utils ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

str_standardise <- function(name) {
  gsub(" ", "-", tolower(name))
}

str_trim_ext <- function(path) {
  sub("\\..+$", "", path)
}

gather_tree <- function(x, depth) {
  if (is.character(depth)) {
    depth <- as.list(depth)
  } else if (is.numeric(depth)) {
    depth <- rep(list(NULL), depth)
  } else {
    stop("`depth` should be character or numeric", call. = FALSE)
  }

  if (length(depth) > 1) {
    x <- gather_tree_recurse(x, depth)
  }
  bind_rows(x, .id = depth[[1]])
}

gather_tree_recurse <- function(x, depth) {
  if (length(depth) > 1) {
    lapply(x, gather_tree_recurse, depth = depth[-1])
  } else {
    bind_rows(x, .id = depth[[1]])
  }
}

at_bottom <- function(.x, .f, ...) {
  if (is.list(.x)) {
    lapply(.x, at_bottom, .f, ...)
  } else {
    .f(.x, ...)
  }
}

spread_attributes <- function(x, id) {
  stopifnot(is.atomic(x))

  attrs <- attributes(unclass(x))
  stopifnot(all(lengths(attrs) == length(x)))

  attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)
  df <- cbind(unclass(x), attrs, stringsAsFactors = FALSE)
  names(df)[[1]] <- id

  df
}

# Simple reimplementations to avoid dplyr dependency -----------------

bind_rows <- function(x, .id = NULL) {
  df <- do.call("rbind", unname(x))

  if (!is.null(.id)) {
    lengths <- vapply(x, nrow, numeric(1))
    id <- rep(names(x), lengths)
    df <- cbind(id, df, stringsAsFactors = FALSE)
    names(df)[[1]] <- .id
  }

  df
}

filter <- function(df, ...) {
  fs <- list(...)
  stopifnot(all(vapply(fs, inherits, logical(1), "formula")))
  fs <- lapply(fs, `[[`, 2)

  bind_call <- function(lhs, rhs, fun = "&") {
    call(fun, lhs, rhs)
  }
  pattern <- Reduce(bind_call, fs[-1], fs[[1]])
  args <- list(df, pattern, substitute())

  # Assumes all formula envs are identical
  env <- as.environment(df)
  parent.env(env) <- environment(..1)
  do.call("[", args, envir = env)
}

filter_first <- function(df, ...) {
  filtered <- filter(df, ...)

  if (nrow(filtered) > 1) {
    filtered[1, ]
  } else {
    filtered
  }
}
