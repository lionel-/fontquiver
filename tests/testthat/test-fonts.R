
context("Font Files")

check_font_files <- function(files, font_getter) {
  Map(function(styles, variant) {
    fonts <- lapply(names(styles), font_getter, variant = variant)
    not_found <- fonts == ""
    info <- paste(variant, ": ", names(styles)[not_found], collapse = ", ", sep = "")
    expect_false(any(not_found), info)
  }, files, names(files))
}

test_that("Bitstream Vera files are found", {
  check_font_files(font_bitstream_vera_files, font_bitstream_vera)
})

test_that("DejaVu files are found", {
  if (check_font_exists("dejavu", "fontHeavy")) {
    check_font_files(font_dejavu_files, font_dejavu)
  }
})
