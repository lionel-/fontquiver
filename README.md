
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

### Get installed font

Get the path to an installed font `foo` with the `font_foo()` getters:

```{r}
font_bitstream_vera()$file
font_bitstream_vera(variant = "serif", style = "bold")$file
font_bitstream_vera(variant = "sans-mono", ext = "woff")$file
```

The version number of the font is in the `version` field:

```{r}
font_bitstream_vera()$version
```


### fontconfig and R categories of fonts

The getters' arguments `variant` and `style` correspond to the font
properties returned by `fontconfig`. This nomenclature is larger than
R's standard classification of fonts. For example the DéjàVu set
contains a font whose variant and style are `serif-condensed` and
`extra-light` respectively.

The R categories of fonts are families (`sans`, `serif`, `mono`, and
`symbol`) and faces (`plain`, `bold`, `italic`, `bolditalic`).
`font_info_files()` provides an alternative way of obtaining a font
file using the R nomenclature:

```{r}
font_info_files("bitstream_vera")
font_info_files("dejavu")$serif$bold
```

You can also get a more general table of metadata about a given font
with `font_info()`.


### Install heavy fonts

Some fonts are too larged to be shipped with fontquiver by
default. You can install those from Github:

```{r}
devtools::install_github("lionel-/fontHeavy")
font_dejavu("math-tex-gyre")
```

### Add web dependency on an installed font

```{r}
# install.packages("htmltools")
html_dep <- htmlFontDependency(style = "bold")
```
