#' @title raynercolor.RGB
#' @description Generates results for color names from the RGB color space
#' @param fungi Image of fungi in transparent background
#' @param distance.method Metrics for color-matching ("euclidean", or "chisq")
#' @importFrom colordistance "loadImage"
#' @importFrom colordistance "plotPixels"
#' @importFrom colordistance "getLabHist"
#' @importFrom colordistance "getImageHist"
#' @importFrom readxl "read_excel"
#' @return Color names, 3D RGB plot, color histogram
#' @examples raynercolor.RGB(system.file("fungi.png", package = "PUPMCR"))
#' \dontrun{raynercolor.RGB("fungi_image_format")}
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
raynercolor.RGB <- function(fungi, distance.method = "euclidean"){

  imgq <- colordistance::loadImage(fungi, lower = c(0, 0.55, 0), upper = c(0.24, 1, 0.24),
                                   sample.size = 1000, hsv = FALSE, CIELab = FALSE, ref.white = NULL,
                                   alpha.channel = TRUE, alpha.message = FALSE)
  colordistance::plotPixels(imgq, n = 20000, lower = c(0, 0.55, 0), upper = c(0.24, 1, 0.24),
                            color.space = "rgb", ref.white = NULL, pch = 20, main = "default", from = "sRGB",
                            xlim = "default", ylim = "default", zlim = "default")
  py <- colordistance::getImageHist(imgq, bins = 2, bin.avg = TRUE, defaultClusters = NULL,
                                    lower = c(0, 0.55, 0), upper = c(0.24, 1, 0.24),
                                    as.vec = FALSE, alpha.channel = TRUE, norm.pix = FALSE,
                                    plotting = TRUE,  hsv = FALSE, title = "path", bounds = c(0, 1))
  py

  Table <- readxl::read_excel(path = system.file("LookupTableV2.xlsx",
                                                 package = "PUPMCR"))

  color<- c(Table$Color)
  r<- c(Table$R)
  g<- c(Table$G)
  b<- c(Table$'B...9')
  h<- c(Table$`Hue Group`)
  c<- c(Table$`Class of Compounds`)

  r_colors <- data.frame(color, r, g, b, h, c)

  if (distance.method == "euclidean") {
    py$ColorName <- sapply(
      seq_along(py$r),
      function(i) {
        r_colors$color[
          which.min(
            ((r_colors$r - py$r[i])^2) +
              ((r_colors$g - py$g[i])^2) +
              ((r_colors$b - py$b[i])^2)
          )
        ]
      }
    )

    py

  } else if (distance.method == "chisq") {
    py$ColorName <- sapply(
      seq_along(py$r),
      function(i) {
        r_colors$color[
          which.min(
            rowSums(
              cbind(
                ((r_colors$r - py$r[i])^2) / (r_colors$r + py$r[i]),
                ((r_colors$g - py$g[i])^2) / (r_colors$g + py$g[i]),
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
