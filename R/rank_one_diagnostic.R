#' @title Rank-one diagnostic for a solve-or-guess fit
#'
#' @description
#' This function checks whether the solve-or-guess fit from
#' \code{\link{solve_or_guess}} is consistent with its core assumption: a
#' single solving probability per rater explains how often any two raters
#' agree. Under this assumption, each pair of raters' agreement rate should
#' be fully explained by a shared chance baseline plus the product of their
#' two individual solving probabilities, with nothing pair-specific left
#' over. Departure from that structure is measured by a residual sum of
#' squares (`T_obs`), computed over rater pairs that share at least one
#' rated item.
#'
#' The significance of `T_obs` is
#' assessed with a parametric bootstrap. Here we simulate fresh datasets from the
#' fitted model, refit the rank-one form, and recompute the residual each
#' time (`T_boot`). The p-value is the fraction of bootstrap replicates
#' whose residual is at least as large as the observed one; a small p-value
#' means the observed agreement pattern is not well explained by a single
#' solving probability per rater.
#'
#' @references
#' Rohe, K., Krauska, A. N., Collins, G., Higgins, J., & Pustejovsky, J. (2026).
#' The solve or guess model: Validating automated systems against
#' heterogeneous human raters. Working paper.
#'
#' @param x An object of class `"sog"`, as returned by \code{\link{solve_or_guess}}.
#' @param B Number of parametric bootstrap replicates. Default `1000`.
#' @param seed Optional integer seed, set before bootstrapping, for reproducibility.
#' @param verbose Logical; print progress messages, including the valid-pairs/
#'   degrees-of-freedom summary. Default `TRUE`.
#'
#' @return An object of class `"sog_rank_one"`, a list with elements:
#' \tabular{lll}{
#'  \bold{T_obs} \tab \code{numeric} \tab the observed rank-one residual sum of squares, computed from the real data. \cr
#'  \bold{T_boot} \tab \code{numeric} \tab length-`B` vector of the residual recomputed from each parametric bootstrap replicate - the simulated null distribution that `T_obs` is compared against. \cr
#'  \bold{B} \tab \code{integer} \tab the number of bootstrap replicates used. \cr
#'  \bold{n_valid_pairs} \tab \code{integer} \tab number of rater pairs that share at least one rated item, and so contribute to `T_obs`/`T_boot`. \cr
#'  \bold{n_params} \tab \code{integer} \tab number of free parameters in the rank-one form (one solving probability per rater, plus the shared chance constant). \cr
#'  \bold{df} \tab \code{integer} \tab `n_valid_pairs - n_params`. A non-positive value means there are no more valid pairs than parameters, but the bootstrap p-value is still valid regardless, since the rank-one equations are nonlinear. \cr
#'  \bold{p_value} \tab \code{numeric} \tab fraction of `T_boot` at least as large as `T_obs` - a small value means the observed agreement is not well explained by a single solving probability per rater. \cr
#'  \bold{raters} \tab \code{character} \tab rater identifiers in the order used to build the underlying agreement matrix (system rater first, then the rest). \cr
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- solve_or_guess(evaluations, system_rater_id = "gpt", B = 200)
#' rank_one_diagnostic(fit, B = 200)
#' }
rank_one_diagnostic <- function(x, B = 1000, seed = NULL, verbose = TRUE) {

  if (!inherits(x, "sog")) {
    stop("`x` must be an object of class 'sog', as returned by solve_or_guess().")
  }
  if (!is.null(seed)) set.seed(seed)

  # Rater order matches solve_or_guess(): system rater first, then the rest.
  evaluations <- x$evaluations
  RATERS <- c(x$system_rater_id, setdiff(unique(evaluations$rater_id), x$system_rater_id))
  m <- length(RATERS)

  # One column per rater. observed_mask records which (item, rater) cells were
  # actually rated, so the bootstrap can reproduce the same missingness pattern.
  wide <- tidyr::pivot_wider(evaluations, names_from = rater_id, values_from = evaluation) |>
    select(all_of(RATERS))
  observed_mask <- !is.na(as.matrix(wide))

  # Fraction of items where each pair of raters agree, ignoring items either
  # rater didn't rate.
  agreement_matrix <- function(W) {
    M <- matrix(1, m, m, dimnames = list(RATERS, RATERS))
    for (a in 1:m) for (b in 1:m) if (a != b) M[a, b] <- mean(W[[a]] == W[[b]], na.rm = TRUE)
    M
  }
  A_obs <- agreement_matrix(wide)

  # pairs with at least one overlapping item; NaN (no overlap) excluded
  valid_pairs <- upper.tri(A_obs) & !is.na(A_obs)
  n_params <- 1 + m
  n_valid  <- sum(valid_pairs)
  df <- n_valid - n_params

  if (verbose) {
    message(sprintf(
      "Valid rater pairs (with overlap): %d / %d possible. Parameters: %d. Degrees of freedom: %d.",
      n_valid, choose(m, 2), n_params, df
    ))
  }

  # Fits the chance constant c and each rater's solving probability p (on the
  # logit scale, so both stay in (0, 1)) by minimizing the squared distance
  # between an agreement matrix A and the rank-one prediction c + (1-c) p_a p_b.
  # Returns that minimized sum of squares
  fit_rank_one <- function(A, p_init, c_init, valid) {
    obj <- function(par) {
      c0 <- stats::plogis(par[1]); p <- stats::plogis(par[-1])
      pred <- c0 + (1 - c0) * outer(p, p)
      sum((A[valid] - pred[valid])^2)
    }
    clamp <- function(v) pmin(pmax(v, 1e-3), 1 - 1e-3)
    init <- c(stats::qlogis(clamp(c_init)), stats::qlogis(clamp(p_init)))
    stats::optim(init, obj, method = "BFGS")$value
  }

  # Each rater's fitted solving probability (the intercept term), converted
  # back off the logit scale - used as starting values for fit_rank_one() and
  # to simulate raters in the bootstrap below.
  fit <- x$fit
  p_hat <- setNames(
    vapply(RATERS, function(r) {
      stats::plogis(fit$rater_ability$estimate[
        fit$rater_ability$rater_id == r & fit$rater_ability$parameter_id == "intercept"
      ])
    }, numeric(1)),
    RATERS
  )

  # Class prior: the fitted probability of each possible true answer.
  ev_levels <- sort(unique(fit$guessing_distribution$evaluation))
  d <- length(ev_levels)
  tau <- vapply(ev_levels, function(e) {
    fit$class_distribution$prior_prob[fit$class_distribution$evaluation == e][1]
  }, numeric(1))
  tau <- tau / sum(tau)

  # Each rater's fitted guess distribution over answers, renormalized to sum to 1.
  Pi <- matrix(0, m, d, dimnames = list(RATERS, ev_levels))
  for (r in RATERS) {
    for (e in ev_levels) {
      Pi[r, e] <- fit$guessing_distribution$probability[
        fit$guessing_distribution$rater_id == r & fit$guessing_distribution$evaluation == e
      ]
    }
  }
  Pi <- Pi / rowSums(Pi)

  # Observed T: fit the rank-one form to the real agreement matrix.
  n <- nrow(wide)
  c_init <- sum(tau^2)
  T_obs <- fit_rank_one(A_obs, p_init = p_hat, c_init = c_init, valid = valid_pairs)

  # Simulate one dataset under the fitted model: draw a true answer per item,
  # then for each rater either "solve" it (copy the true answer) or "guess"
  # it from their own guess distribution. observed_mask reproduces the real
  # missingness pattern before recomputing agreement and refitting the
  # rank-one form, giving one draw of T under the null.
  sim_T <- function() {
    Y <- sample(d, n, replace = TRUE, prob = tau)
    R <- matrix(0L, n, m, dimnames = list(NULL, RATERS))
    for (a in 1:m) {
      solved <- stats::runif(n) < p_hat[a]
      R[, a] <- ifelse(solved, Y, sample(d, n, replace = TRUE, prob = Pi[a, ]))
    }
    R[!observed_mask] <- NA
    A <- matrix(1, m, m, dimnames = list(RATERS, RATERS))
    for (a in 1:m) for (b in 1:m) if (a != b) A[a, b] <- mean(R[, a] == R[, b], na.rm = TRUE)
    fit_rank_one(A, p_init = p_hat, c_init = c_init, valid = valid_pairs)
  }

  # Repeat B times to build the null distribution of T, then see how extreme
  # the observed value is relative to it.
  if (verbose) message(sprintf("Parametric bootstrap (B = %d)...", B))
  T_boot <- replicate(B, sim_T())
  p_value <- mean(T_boot >= T_obs)

  out <- list(
    T_obs         = T_obs,
    T_boot        = T_boot,
    B             = B,
    n_valid_pairs = n_valid,
    n_params      = n_params,
    df            = df,
    p_value       = p_value,
    raters        = RATERS
  )
  class(out) <- "sog_rank_one"
  return(out)
}
