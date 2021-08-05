#'  List of palettes
#'
#' Use \code{\link{microshades_palette}} to construct palettes of desired length.
#'
#' @export
#'
#' @examples
#' microshades_palettes
#' microshades_palette("micro_gray")
#' microshades_palette("micro_brown")
#' microshades_palette("micro_green")
#' microshades_palette("micro_orange")
#' microshades_palette("micro_blue")
#' microshades_palette("micro_purple")
#'
microshades_palettes <- list(
  micro_gray = c("#d9d9d9","#bdbdbd", "#969696","#737373","#525252"),
  micro_brown = c("#D8C7BE", "#CAA995", "#B78560", "#9E5C00", "#7D3200"),
  micro_green = c("#c7e9c0", "#a1d99b","#74c476", "#41ab5d", "#238b45"),
  micro_orange = c("#feeda0","#fec44f","#fdae6b", "#fe9929","#ff7f00"),
  micro_blue = c("#eff3ff","#c6dbef", "#9ecae1", "#6baed6", "#4292c6"),
  micro_purple = c("#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3")
)

#'  List of CVD palettes
#'
#' Use \code{\link{microshades_palette}} to construct palettes of desired length.
#'
#' @export
#'
#' @examples
#' microshades_cvd_palettes
#' microshades_palette("micro_cvd_gray")
#' microshades_palette("micro_cvd_green")
#' microshades_palette("micro_cvd_orange")
#' microshades_palette("micro_cvd_blue")
#' microshades_palette("micro_cvd_turquoise")
#' microshades_palette("micro_cvd_purple")
#'
microshades_cvd_palettes <- list(
  micro_cvd_gray = c("#F5F5F5", "#D6D6D6", "#B7B7B7", "#8B8B8B","#616161"),
  micro_cvd_green = c("#DDFFA0",  "#BDEC6F",  "#97CE2F", "#6D9F06","#4E7705"),
  micro_cvd_orange = c("#FFD5AF",  "#FCB076","#F09163", "#C17754", "#9D654C"),
  micro_cvd_blue = c("#E7F4FF", "#BCE1FF", "#7DCCFF", "#56B4E9","#098BD9"),
  micro_cvd_turquoise = c("#A3E4D7", "#48C9B0",  "#43BA8F",  "#009E73", "#148F77"),
  micro_cvd_purple = c("#EFB6D6", "#E794C1", "#CC79A7", "#A1527F", "#7D3560")
)

#' Microshades sequential color shading palette accessor
#'
#' This function can be used to access the microshades palette desired.
#' Each palette contains a base color paired with lighter versions of the base color.
#'
#' Use this function to get the values needed to set the manual color values when applying to different plots
#'
#' @param n Number of colors desired. Palettes contain 5 shades
#'   If omitted, uses all colours.
#' @param lightest logical, if user requests n < 5 it will reutrn n lightest shades if TRUE.
#'   If FALSE, it will return n darkest shades
#' @param name Name of desired palette. Choices are:
#'   \code{micro_gray}, \code{micro_brown},  \code{micro_green},
#'   \code{micro_orange}, \code{micro_blue},  \code{micro_purple}
#'   \code{micro_cvd_gray}, \code{micro_cvd_brown},  \code{micro_cvd_green},
#'   \code{micro_cvd_orange}, \code{micro_cvd_blue},  \code{micro_cvd_purple}
#'
#' @return A microshades palette of colors.
#' @export
#' @keywords colors
#' @examples
#' microshades_palette("micro_cvd_green")
#' microshades_palette("micro_orange", 3, lightest = FALSE)
#'

microshades_palette <- function(name, n, lightest = TRUE) {

  if (is.null(microshades_palettes[[name]]))
  {
    pal <- microshades_cvd_palettes[[name]]
  }
  else
  {
    pal <- microshades_palettes[[name]]
  }

  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (n > length(pal)) {
    stop("Number of requested shades is greater than what palette can offer")
  }

  if (lightest)
  {
    out <- pal[1:n]
  }
  else
  {
    start <- 5 - n + 1
    out <- pal[start:5]
  }


  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics par image
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}

