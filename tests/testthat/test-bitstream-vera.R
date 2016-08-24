
context("Font Files")

test_that("Bitstream Vera files are found", {
  check_font_files(bitstream_vera_files, font_bitstream_vera)
})
