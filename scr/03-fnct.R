# lx Equality Measures ----------------------------------------------------

#' Average lx
Avglx <- function (lx, e0) {
  sum(lx^2) / e0
}

#' Inverse Gini Coefficient of lx
InvGinilx <- function (lx, e0) {
  1 / (1 - Avglx(lx, e0))
}

#' Logit of average lx
Logitlx <- function (lx, e0) {
  avglx <- Avglx(lx, e0)
  avglx / (1 - avglx)
}

#' Inverse Keyfitz' Entropy
InvKeyfEntr <- function (lx, e0) {
<<<<<<< HEAD:scr/03-fnct.R
  -e0/ sum(lx * log(lx))
=======
  -e0 / sum(lx * log(lx))
>>>>>>> 19d7e730132b00fa771ed20f2399a6d6cf61ffb0:R/03-fnct.R
}

#' MIRA
MIRA <- function (lx, x) {
  max(lx * x)
}

#' Outer Rectangularization
ORR <- function (e0, omega) {
  e0 / omega
}

#' MIRA
MIRA <- function (lx, x) {
  max(lx * x)
}

#' Inner Rectangularization
IRR <- function (lx, x, omega) {
  max(lx * x) / omega
}

#' Inverse Inter Quartile Range of lx
InvIQRlx <- function (lx, x) {
  upper_quartile <- max(x[lx >= 0.75])
  lower_quartile <- min(x[lx <= 0.25])
  1 / (lower_quartile - upper_quartile)
}

NormalizeRange <- function (x, scale = 1, na_rm = FALSE) {
  (x / max(x, na.rm = na_rm)) * scale
}

# Utility -----------------------------------------------------------------

#' Export Graphical Object as PDF
#'
#' @param .x Graphical object
#' @param .path Filesystem destination to save object.
#'   [string, length == 1]
#' @param .width Figure width in cm. [numeric, length == 1]
#' @param .height Figure height in cm. [numeric, length == 1]
#'
#' @return PDF output to disk.
ExportPDF <- function (.x, .path, .width, .height) {
  pdf(.path, width = 0.4*.width, height = 0.4*.height,
      useDingbats = FALSE) # avoid problems with missing fonts
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5,
                 width = unit(.width, "cm"),
                 height = unit(.height, "cm"))
  pushViewport(vp)

  print(.x, vp = vp)
  dev.off()
}
