#' Ris-file data from Functional Family Therapy (FFT) systematic review
#'
#' Bibliometric toy data from a systematic review regarding
#' Functional Family Therapy (FFT) for Young People in Treatment for
#' Non-opioid Drug Use (Filges et al., 2015). The data includes all 90 included and 180 excluded
#' randomly sampled references from the literature search of the
#' systematic review.
#'
#'
#' @format A tibble with 270 rows/studies and 6 variables/columns
#'
#' \tabular{lll}{
#'  \bold{author} \tab \code{character} \tab indicating the authors of the reference \cr
#'  \bold{eppi_id} \tab \code{character} \tab indicating a unique eppi-ID for each study \cr
#'  \bold{studyid} \tab \code{numeric} \tab indicating a unique study-ID for each study \cr
#'  \bold{title} \tab \code{character} \tab with the title of the study \cr
#'  \bold{abstract}  \tab \code{character} \tab with the study abstract \cr
#'  \bold{human_code} \tab \code{numeric} \tab indicating the human screening decision.
#'  1 = included, 0 = excluded. \cr
#' }
#'
#'
#' @references Filges, T., Andersen, D, & Jørgensen, A-M. K (2015).
#' Functional Family Therapy (FFT) for Young People in Treatment for Non-opioid Drug Use: A Systematic Review
#' \emph{Campbell Systematic Reviews}, \doi{10.4073/csr.2015.14}
#'

"filges2015_dat"
