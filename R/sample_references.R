#' @title Random sample references
#'
#' @description
#' `sample_references`samples n rows from the dataset with titles and abstracts.
#' Implements the target set sampling algorithm from Hou & Tipton (2024) when `n` is
#' NULL and `relevant_col` is provided, providing a formal reliability guarantee.
#' Defined as the probability of achieving `c_target` recall is at least `1 - c_target^k`.
#'
#' The algorithm uses with replacement. This means that relevant records are returned to the
#' pool after selection, making draws independent without needing to know the total number of 
#' relevant records (L). See [Hou & Tipton (2024)](https://doi.org/10.1002/jrsm.1690) for details.
#'
#' @param data Dataset containing records with relevance labels.
#' @param n Integer. Number of rows to sample using simple random sampling. If
#'   provided, `relevant_col`, `c_target`, and `R_c` are ignored and simple random
#'   sampling is used directly. If NULL and `relevant_col` is provided, k (the
#'   target set size) is computed from `c_target` and `R_c`.
#' @param relevant_col Character string naming the binary relevance column (1 = relevant).
#'   Can also be a character vector of multiple column names, in which case a record
#'   is treated as relevant only if *all* of the named columns equal 1 (e.g. `human_code == 1`
#'   and `decision_binary == 1`). Only used when `n` is NULL; otherwise falls back to
#'   simple random sampling behaviour.
#' @param c_target Numeric in (0,1). Desired recall level (e.g. 0.95). Used to compute k.
#' @param R_c Numeric in (0,1). Desired reliability, i.e. probability of achieving
#'   `c_target` recall (e.g. 0.90). Used to compute k.
#' @param with_replacement Logical. Whether to sample with replacement. Default TRUE. 
#' Only used in simple random sampling.
#' @param id_col Character string naming the record ID column. Default `"record_id"`.
#' @param prob_vec Vector of probability weights. Only used in simple random sampling.
#'   Default is a uniform vector of 1/n.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#' @param message Logical. Whether to print a message about the target set size and reliability guarantee. Default is FALSE.
#'
#' @return When `n` is NULL and `relevant_col` is provided, a list with:
#'   \item{target_set}{Data frame of k target records}
#'   \item{target_ids}{Vector of record IDs in the target set}
#'   \item{k}{Target set size}
#'   \item{c_target}{Desired recall level}
#'   \item{R_c}{Desired reliability}
#'   \item{reliability_guarantee}{Lower bound on probability of achieving c_target recall: 1 - c_target^k}
#'
#'   Otherwise (i.e. whenever `n` is provided), returns a data frame of n rows
#'   (original, simple random sampling behaviour).
#'
#' @references
#' Hou, Z., & Tipton, E. (2024). Enhancing recall in automated record screening:
#'   A resampling algorithm. \emph{Research Synthesis Methods, 15}(3), 372-383.
#'   \doi{10.1002/jrsm.1690}
#'
#' Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (2025).
#'   Generative Pretrained Transformer Models Can Function as Highly Reliable Second
#'   Screeners of Titles and Abstracts in Systematic Reviews: A Proof of Concept and
#'   Common Guidelines. \emph{Psychological Methods}.
#'   \doi{10.1037/met0000769}
#'
#' @examples
#' \dontrun{
#' # Compute k from c_target and R_c:
#' target_studies <- sample_references(
#'   data = combined_data,
#'   relevant_col = "decision_binary",
#'   c_target = 0.95,
#'   R_c = 0.90
#' )
#'
#' # Relevant only if both the human coder and the model flagged the record:
#' target_studies <- sample_references(
#'   data = combined_data,
#'   relevant_col = c("human_code", "decision_binary"),
#'   c_target = 0.95,
#'   R_c = 0.90
#' )
#'
#' # Simple random sampling:
#' excl_test_dat <- filges2015_dat[1:200, ] |> sample_references(100)
#'}
#' @export

sample_references <- function(
    data,
    n = NULL,
    relevant_col = NULL,
    c_target = NULL,
    R_c = NULL,
    with_replacement = FALSE,
    id_col = "record_id",
    prob_vec = NULL,
    seed = 123,
    message = FALSE
) {

  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    warning("No seed provided. Results may not be reproducible.")
  }

  # ------------------------------------------------------------------
  # Sampling with replacement (Hou & Tipton)
  # ------------------------------------------------------------------
  if (is.null(n) && !is.null(relevant_col)) {

    if (!id_col %in% names(data)) {
      stop(
        "`id_col` = \"", id_col, "\" was not found in `data`.\n",
        "Please specify the name of the record ID column via the `id_col` argument."
      )
    }

    if (is.null(c_target) || is.null(R_c)) {
      stop("Provide `n`, or both `c_target` and `R_c` to compute k, when `relevant_col` is supplied.")
    }
    n <- ceiling(log(1 - R_c) / log(c_target))

    if (!all(relevant_col %in% names(data))) {
      stop(
        "`relevant_col` = ", paste(dQuote(setdiff(relevant_col, names(data)), q = FALSE), collapse = ", "),
        " was not found in `data`."
      )
    }

    k <- n
    # Make a logical vector indicating with TRUE if the record is relevant (1) in all of the specified columns
    is_relevant <- Reduce(`&`, lapply(relevant_col, function(col) data[[col]] == 1))
    n_relevant <- sum(is_relevant, na.rm = TRUE)

    if (n_relevant < k) {
      stop(paste0(
        "Only ", n_relevant, " relevant records available but k = ", k, " required.\n",
        "Consider reducing `c_target` or `R_c`."
      ))
    }

    relevant_data <- data[is_relevant & !is.na(is_relevant), ]

    # Draw with replacement from the relevant pool, one at a time, until k
    # distinct records are collected 
    drawn_idx <- integer(0) # Initialize an empty vector to store the indices of drawn records
    while (length(unique(drawn_idx)) < k) { # Continue drawing until we have k unique records
      drawn_idx <- c(drawn_idx, sample.int(n_relevant, size = 1)) # Draw one record at a time and append its index to drawn_idx.
    }
    target_idx <- unique(drawn_idx)[seq_len(k)] # Select the first k unique indices from the drawn indices
    target_data <- relevant_data[target_idx, ] # Subset the relevant data to get the target set

    guarantee <- if (!is.null(c_target)) 1 - c_target^k else NA_real_ # Compute the reliability guarantee based on c_target and k

    if (message && !is.null(c_target) && !is.null(R_c)) {
      message(paste0(
        "Target set of size k = ", k, " sampled with replacement.\n",
        "Reliability guarantee: P(recall >= ", c_target, ") >= ", round(guarantee, 4)
      ))
    }

    return(list(
      target_set = target_data,
      target_ids = target_data[[id_col]],
      k = k,
      c_target = c_target,
      R_c = R_c,
      reliability_guarantee = guarantee
    ))
  }

  # ------------------------------------------------------------------
  # Original
  # ------------------------------------------------------------------
  if (is.null(n)) stop("Provide `n` for simple random sampling.")
  if (is.null(prob_vec)) prob_vec <- rep(1 / nrow(data), nrow(data))

  data[sample(NROW(data), size = n, replace = with_replacement, prob = prob_vec), ]

}

