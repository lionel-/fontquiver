
# fontquiver

fontquiver installs a set of fonts with permissive licences. It is
useful for packages that needs controlled versions of fonts.


## Installation

Get the development version from github with:

```{r}
# install.packages("devtools")
devtools::install_github("lionel-/fontquiver")
```

## Usage

Get the path to an installed font `foo` with `font_foo()` helper:

```{r}
font_bitstream_vera()
font_bitstream_vera(face = "serif", style = "bold")
font_bitstream_vera(face = "sans-mono", ext = "woff")
```

The version number of the font is in the `version` attribute:

```{r}
font_path <- font_bitstream_vera()
attr(font_path, "version")
```
