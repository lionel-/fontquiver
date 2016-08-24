
check_font_files <- function(files, font_getter) {
  Map(function(styles, face) {
    fonts <- lapply(names(styles), font_getter, face = face)
    not_found <- fonts == ""
    info <- paste(face, ": ", names(styles)[not_found], collapse = ", ", sep = "")
    expect_false(any(not_found), info)
  }, files, names(files))
}
