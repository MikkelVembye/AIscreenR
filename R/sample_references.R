#' @title Random sample references
#'
#' @description
#' `sample_references`samples n rows from the dataset with titles and abstracts either with or without replacement.
#'  This function is supposed to support the construct of a test dataset,
#'  as suggested by [Vembye et al. (2024)](https://osf.io/preprints/osf/yrhzm).
#'
#' @references Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (2024)
#'   \emph{GPT API Models Can Function as Highly Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
#'   A Proof of Concept and Common Guidelines} \url{https://osf.io/preprints/osf/yrhzm}
#'
#' @param data Dataset containing the titles and abstracts wanted to be screened.
#' @param n A non-negative integer giving the number of rows to choose.
#' @param with_replacement Logical indicating if sampling should be done with of withour replacement.
#'    Default is `FALSE`.
#' @param prob_vec 'A vector of probability weights for obtaining the elements of the vector being sampled.'
#'    Default is a vector of 1/n.
#'
#' @return A dataset with n rows.
#'
#' @examples
#'
#' excl_test_dat <- filges2015_dat[1:200,] |> sample_references(100)
#'
#' @export


sample_references <- function(data, n, with_replacement = FALSE, prob_vec = rep(1/n, nrow(data))) {
  data[sample(NROW(data), size = n, replace = with_replacement, prob = prob_vec),]
}
