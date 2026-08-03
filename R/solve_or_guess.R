#' @title Fit the solve-or-guess model to compare an automated screener against human raters
#'
#' @description
#' Fits the solve-or-guess model (Rohe et al., 2026) to a set of raters. One
#' automated ("system") rater, such as an AI assistant,
#' and one or more human raters, who each labeled some or all of the same
#' items as Include/Exclude (or any other binary/categorical decision). The
#' model estimates each rater's solving probability i.e. how often the rater
#' actually recovers the true label rather than guessing. This is done without requiring
#' a gold-standard label set. The kappa-ratio of the system rater against
#' each reference human rater is the primary comparison: a bootstrap interval
#' that contains 1 means the two are statistically indistinguishable in
#' solving probability.
#'
#' @references
#' Rohe, K., Krauska, A. N., Collins, G., Higgins, J., & Pustejovsky, J. (2026).
#' The solve or guess model: Validating automated systems against
#' heterogeneous human raters. Working paper.
#'
#' @param evaluations Either (a) a `data.frame`/`tibble` with one row per item
#'   (study), containing at least one column with the AI decision and at
#'   least one column with a human decision - see `system_rater_id` and
#'   `reference_raters` below for how those columns are identified - or
#'   (b) a tabscreen result object of class `'gpt'`, `'gpt_tbl'`, or
#'   `'gpt_agg_tbl'` (i.e. the object returned by [tabscreen_gpt()] or one of
#'   the other tabscreen functions). When (b) is used, each model that was
#'   screened becomes its own AI rater (named after the model), and the
#'   result must reflect a single prompt/top_p configuration.
#' @param system_rater_id The name of the column in `evaluations` holding the
#'   automated/system rater's decisions (e.g. `"gpt-4o-mini"`), coded the same
#'   way as the human decision column(s).
#' @param reference_raters Character vector naming the column(s) in
#'   `evaluations` to compare the system rater against - typically one or more
#'   human raters, coded numerically as `1` (include) / `0` (exclude); other
#'   AI raters can also be included here. When `evaluations` is a tabscreen
#'   result object, these must be the human decision column(s) already
#'   present in that object.
#' @param item_id Name of the column in `evaluations` that identifies each
#'   item (study). Ignored when `evaluations` is a tabscreen result object
#'   (the object's study ID is used automatically). Default `"item_id"`.
#' @param B Number of nonparametric (item-level) bootstrap replicates used for
#'   confidence intervals. Default `1000`. The `B` model refits run in
#'   parallel whenever a non-sequential `future::plan()` is active (e.g.
#'   `future::plan(future::multisession)`).
#' @param conf_level Confidence level for the bootstrap intervals. Default `0.95`.
#' @param seed Optional integer seed, set before bootstrapping, for reproducibility.
#' @param verbose Logical; print progress messages while fitting/bootstrapping.
#'   Default `TRUE`.
#' @param progress Logical; show a progress bar for the `B` bootstrap
#'   refits. Default `TRUE`.
#'
#' @return An object of class `"sog"`, a list with elements:
#' \tabular{lll}{
#'  \bold{solving_probabilities} \tab \code{tibble} \tab \code{rater_id}, \code{p_hat}, and bootstrap \code{ci_lower}/\code{ci_upper} for each rater. \cr
#'  \bold{pairwise_kappa} \tab \code{tibble} \tab observed Cohen's kappa for every rater pair, restricted to items both raters in the pair actually rated (\code{n_overlap}). \cr
#'  \bold{kappa_ratios} \tab \code{tibble} \tab \eqn{p_system/p_reference}{p_{system}/p_{reference}} for each reference rater, with a bootstrap CI. \cr
#'  \bold{fit} \tab \code{list} \tab the raw solve-or-guess model fit (\code{rater_ability}, \code{item_estimates}, \code{guessing_distribution}, \code{class_distribution}). \cr
#'  \bold{evaluations} \tab \code{tibble} \tab the long-format (\code{item_id}, \code{rater_id}, \code{evaluation}) data actually used to fit the model, after any tabscreen-object conversion and dropping of unrated pairs. \cr
#'  \bold{system_rater_id} \tab \code{character} \tab the column name used as the system rater. \cr
#'  \bold{reference_raters} \tab \code{character} \tab the column name(s) used as the reference raters. \cr
#'  \bold{bootstrap_rater_ability} \tab \code{tibble} \tab every bootstrap replicate's rater ability estimates, tagged by \code{bootstrap_iter} - the raw draws behind \code{solving_probabilities}' and \code{kappa_ratios}' confidence intervals. \cr
#'  \bold{B} \tab \code{integer} \tab the number of bootstrap replicates used. \cr
#'  \bold{conf_level} \tab \code{numeric} \tab the confidence level used for the bootstrap intervals. \cr
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' evaluations <- data.frame(
#'   item_id  = paste0("study_", 1:50),
#'   human    = sample(c("Include", "Exclude"), 50, replace = TRUE),
#'   gpt      = sample(c("Include", "Exclude"), 50, replace = TRUE)
#' )
#'
#' fit <- solve_or_guess(
#'   evaluations,
#'   system_rater_id = "gpt",
#'   reference_raters = "human",
#'   B = 200
#' )
#'
#' # Using a tabscreen result object directly
#' res <- tabscreen_gpt(
#'   data = filges2015_dat,
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract
#' )
#'
#' fit <- solve_or_guess(
#'   res,
#'   system_rater_id = "gpt-4o-mini",
#'   reference_raters = c("human_code_1", "human_code_2"),
#'   B = 200
#' )
#' }
solve_or_guess <- function(evaluations,
                            system_rater_id,
                            reference_raters,
                            item_id = "item_id",
                            B = 1000,
                            conf_level = 0.95,
                            seed = NULL,
                            verbose = TRUE,
                            progress = TRUE) {

  if (!is.character(system_rater_id) || length(system_rater_id) != 1) {
    stop("`system_rater_id` must be a single column name.")
  }
  if (!is.character(reference_raters) || length(reference_raters) < 1) {
    stop("`reference_raters` must be a character vector naming one or more columns.")
  }
  if (system_rater_id %in% reference_raters) {
    stop("`system_rater_id` must not also appear in `reference_raters`.")
  }
  if (length(reference_raters) < 2) {
    warning("At least 2 reference raters (3 total) are recommended.")
  }

  if (is_gpt(evaluations) || is_gpt_tbl(evaluations) || is_gpt_agg_tbl(evaluations)) {
    evaluations <- .tabscreen_to_wide(evaluations, human_decision = reference_raters)
    item_id <- "item_id"
  }

  rater_cols <- c(system_rater_id, reference_raters)
  missing_cols <- setdiff(c(item_id, rater_cols), names(evaluations))
  if (length(missing_cols) > 0) {
    stop(sprintf("`evaluations` is missing column(s): %s", paste(missing_cols, collapse = ", ")))
  }

  # Reduce to just the columns needed, and standardize the item-id column
  # name before reshaping to the long format the fitting engine expects.
  wide <- evaluations |> select(all_of(c(item_id, rater_cols)))
  names(wide)[names(wide) == item_id] <- "item_id"
  wide$item_id <- as.character(wide$item_id)

  long_evaluations <-
    wide |>
    tidyr::pivot_longer(cols = all_of(rater_cols), names_to = "rater_id", values_to = "evaluation") |>
    filter(!is.na(evaluation))

  if (!is.null(seed)) set.seed(seed)

  alpha <- 1 - conf_level
  ci_probs <- c(alpha / 2, 1 - alpha / 2)

  fit <- .solve_or_guess_fast(long_evaluations, system_rater_id = system_rater_id, verbose = FALSE)

  # Estimated solving probabilities (p_hat) for each rater, from the fitted model
  abilities <-
    fit$rater_ability |>
    filter(parameter_id == "intercept") |>
    transmute(rater_id, p_hat = .logit_inv(estimate))

  # Bootstrap the model to get confidence intervals for each rater's solving probability
  if (verbose) message(sprintf("Bootstrapping (B = %d)...", B))
  boot <- .nonparametric_bootstrap_solve_or_guess(
    long_evaluations, system_rater_id = system_rater_id, B = B, progress = progress
  )

  # Extract the bootstrap estimates of solving probability (p) for each rater and each bootstrap iteration
  boot_p <-
    boot$bootstrap_rater_ability |>
    filter(parameter_id == "intercept") |>
    mutate(p = .logit_inv(estimate)) |>
    select(bootstrap_iter, rater_id, p)

  # Basic (pivotal) bootstrap CI: 2*p_hat - bootstrap quantile, which has
  # better coverage than the plain percentile interval when the bootstrap
  # distribution of p is skewed (Rohe et al., 2026).
  ci <-
    boot_p |>
    summarise(
      q_lower = unname(stats::quantile(p, ci_probs[1])),
      q_upper = unname(stats::quantile(p, ci_probs[2])),
      .by = rater_id
    )

  solving_probabilities <-
    abilities |>
    left_join(ci, by = "rater_id") |>
    mutate(ci_lower = 2 * p_hat - q_upper, ci_upper = 2 * p_hat - q_lower) |>
    select(rater_id, p_hat, ci_lower, ci_upper) |>
    arrange(desc(p_hat))

  # Compute Cohen's kappa for a single pair of raters, ignoring any items that either rater did not rate (NA)
  cohen_kappa_pair <- function(a, b) {
    keep <- !is.na(a) & !is.na(b)
    a <- a[keep]; b <- b[keep]
    lv <- sort(union(unique(a), unique(b)))
    m  <- table(factor(a, lv), factor(b, lv)) / length(a)
    po <- sum(diag(m)); pe <- sum(rowSums(m) * colSums(m))
    if (abs(1 - pe) < 1e-12) NA_real_ else (po - pe) / (1 - pe)
  }

  # Compute pairwise Cohen's kappa for every rater pair, restricted to items both raters actually rated
  rater_pairs <- utils::combn(rater_cols, 2, simplify = FALSE)
  pairwise_kappa <-
    purrr::map(rater_pairs, function(rp) {
      tibble::tibble(
        rater_a   = rp[1],
        rater_b   = rp[2],
        n_overlap = sum(!is.na(wide[[rp[1]]]) & !is.na(wide[[rp[2]]])),
        kappa     = cohen_kappa_pair(wide[[rp[1]]], wide[[rp[2]]])
      )
    }) |>
    purrr::list_rbind()

  boot_wide <- tidyr::pivot_wider(boot_p, names_from = rater_id, values_from = p)
  # Helper function to get the estimated solving probability for a given rater
  p_of <- function(r) abilities$p_hat[abilities$rater_id == r]

  # Compute the kappa-ratio of the system rater against each reference rater,
  # with a basic (pivotal) bootstrap CI computed on the log scale (safer than
  # the raw ratio scale when a reference rater's p_hat is close to zero) and
  # then exponentiated back.
  kappa_ratios <-
    purrr::map(reference_raters, function(hr) {
      ratio_hat <- p_of(system_rater_id) / p_of(hr)
      log_ratio_hat <- log(ratio_hat)
      log_r <- log(boot_wide[[system_rater_id]] / boot_wide[[hr]])
      tibble::tibble(
        comparison = paste0(system_rater_id, " / ", hr),
        ratio_hat  = ratio_hat,
        ci_lower   = exp(2 * log_ratio_hat - unname(stats::quantile(log_r, ci_probs[2]))),
        ci_upper   = exp(2 * log_ratio_hat - unname(stats::quantile(log_r, ci_probs[1])))
      )
    }) |>
    purrr::list_rbind()

  out <- list(
    solving_probabilities   = solving_probabilities,
    pairwise_kappa          = pairwise_kappa,
    kappa_ratios            = kappa_ratios,
    fit                     = fit,
    evaluations             = long_evaluations,
    system_rater_id         = system_rater_id,
    reference_raters        = reference_raters,
    bootstrap_rater_ability = boot$bootstrap_rater_ability,
    B                       = B,
    conf_level              = conf_level
  )
  class(out) <- "sog"
  return(out)
}