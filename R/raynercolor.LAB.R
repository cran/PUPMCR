#' @title raynercolor.LAB
#' @description Generates results for color names from the CIELAB color space
#' @param fungi Image of fungi in transparent background
#' @param distance.method Metrics for color-matching ("euclidean", or "chisq")
#' @importFrom colordistance "loadImage"
#' @importFrom colordistance "plotPixels"
#' @importFrom colordistance "getLabHist"
#' @importFrom colordistance "getImageHist"
#' @importFrom readxl "read_excel"
#' @return Color names, 3D LAB plot, color histogram
#' @examples raynercolor.LAB(system.file("fungi.png", package = "PUPMCR"))
#' \dontrun{raynercolor.LAB("fungi_image_format")}
#' @author Niña Rose E. Zapanta
#' @author Jericho Ivan Pineda
#' @author Rhenz Hannah R. Santos
#' @author Lourdes V. Alvarez
#' @author Chester C. Deocaris
#' @references Rayner, R. (1970, ISBN:9780851980263). Mycological Colour Chart. UK: Commonwealth Mycological Insitute and British Mycological Society.
#' @references Conlan, X. A., Kalra. R., and Goel M. (2020) <doi:10.3389/fchem.2020.00369> Fungi as a Potential Source of Pigments: Harnessing Filamentous Fungi. Front. Chem, 8:369.
#' @references Cejpek, K. and Valisek, J. (2011) <doi:10.17221/524/2010-cjfs> Pigments of Higher Fungi: A review. Czech J. Food Sci., 29:87-102.
#' @export
#Color Names
raynercolor.LAB <- function(fungi, distance.method = "euclidean"){

  imgq <- colordistance::loadImage(fungi, lower = c(0, 1, 0), upper = c(0, 1, 0),
                                   CIELab = TRUE, ref.white = "D65", sample.size = 1000)
  colordistance::plotPixels(imgq, n = 20000, lower = c(0, 1, 0), upper = c(0, 1, 0),
                            color.space = "lab", ref.white = "D65", pch = 20, main = "default",
                            from = "sRGB", xlim = "default", ylim = "default", zlim = "default")
  py <- colordistance::getLabHist(imgq, bins = 2,
                                  sample.size = 10000, ref.white = "D65", bin.avg = TRUE,
                                  plotting = TRUE, lower = c(0, 1, 0), upper = c(0, 1, 0),
                                  a.bounds = c(-100, 100), b.bounds = c(-100, 100))
  py

  Table <- readxl::read_excel(path = system.file("LookupTableV2.xlsx",
                                                 package = "PUPMCR"))

  color<- c(Table$Color)
  L<- c(Table$L)
  a<- c(Table$A)
  b<- c(Table$'B...4')
  h<- c(Table$`Hue Group`)
  c<- c(Table$`Class of Compounds`)

  r_colors <- data.frame(color, L, a, b, h, c)

  if (distance.method == "euclidean") {
    py$ColorName <- sapply(
      seq_along(py$L),
      function(i) {
        r_colors$color[
          which.min(
            ((r_colors$L - py$L[i])^2) +
              ((r_colors$a - py$a[i])^2) +
              ((r_colors$b - py$b[i])^2)
          )
        ]
      }
    )

    py


  } else if (distance.method == "chisq") {
    py$ColorName <- sapply(
      seq_along(py$L),
      function(i) {
        r_colors$color[
          which.min(
            rowSums(
              cbind(
                ((r_colors$L - py$L[i])^2) / (r_colors$L + py$L[i]),
                ((r_colors$a - py$a[i])^2) / (r_colors$a + py$a[i]),
                ((r_colors$b - py$b[i])^2) / (r_colors$b + py$b[i])
              )
            )
          )
        ]
      }
    )

    py

  } else {
    stop("Invalid distance metric. Supported options: 'euclidean' or 'chisq'")
  }

  return(py)
}
