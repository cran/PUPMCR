% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fungalpigments.RGB.R
\name{fungalpigments.RGB}
\alias{fungalpigments.RGB}
\title{fungalpigments.RGB}
\usage{
fungalpigments.RGB(fungi, distance.method = "euclidean")
}
\arguments{
\item{fungi}{Image of fungi in transparent background}

\item{distance.method}{Metrics for color-matching ("euclidean", or "chisq")}
}
\value{
Fungal pigments, 3D RGB plot, color histogram
}
\description{
Generates results for associated fungal pigments / class of compounds based on the RGB color matches
}
\examples{
fungalpigments.RGB(system.file("fungi.png", package = "PUPMCR"))
\dontrun{fungalpigments.RGB("fungi_image_format")}
}
\references{
Rayner, R. (1970, ISBN:9780851980263). Mycological Colour Chart. UK: Commonwealth Mycological Insitute and British Mycological Society.

Conlan, X. A., Kalra. R., and Goel M. (2020) <doi:10.3389/fchem.2020.00369> Fungi as a Potential Source of Pigments: Harnessing Filamentous Fungi. Front. Chem, 8:369.

Cejpek, K. and Valisek, J. (2011) <doi:10.17221/524/2010-cjfs> Pigments of Higher Fungi: A review. Czech J. Food Sci., 29:87-102.
}
\author{
Niña Rose E. Zapanta

Jericho Ivan Pineda

Rhenz Hannah R. Santos

Lourdes V. Alvarez

Chester C. Deocaris
}
