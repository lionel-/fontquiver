
* Requirements

`webify` (https://github.com/ananthakumaran/webify)


* Procedure to install a new font `foo`

Create a `foo-VERSION` file in `inst/fonts/` containing the version of
the font to be released. This is used in the Makefile and in R tools
to figure out the font version.

Write a Makefile target for that font that:

  - extracts the fonts inside a folder suffixed with -fonts,
    e.g. `foo-fonts`.
  - applies webify to all ttf files to creates .woff and .svg versions.
  - creates a `foo-LICENCE` file in `inst/fonts/`.
