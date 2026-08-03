############################
# Utility: logistic sigmoid
############################

.logit_inv <- function(x) {
  1 / (1 + exp(-x))
}

############################
# Logistic regression with fractional outcomes
############################

.logistic_fit <- function(Z, y, start = NULL, maxit = 100, tol = 1e-6) {
  # Inputs:
  #   Z:    n x f matrix of features
  #   y:    length-n vector of fractional outcomes, in [0,1]
  #   start: optional initial parameter vector (length f)
  #   maxit: maximum number of iterations
  #   tol:   convergence tolerance
  #
  # Output:
  #   A fitted parameter vector (beta) of length f
  #
  # Explanation of approach:
  #   We perform a Newton-Raphson iteration. On each iteration:
  #     1. Compute predictions p = logit_inv(Z %*% beta)
  #     2. Compute gradient: grad = t(Z) %*% (y - p)
  #     3. Compute Hessian: H = t(Z) %*% (Z * (p * (1 - p)))
  #     4. Update parameters: beta_new = beta + solve(H, grad)

  n <- nrow(Z)
  f <- ncol(Z)

  beta <- if (is.null(start)) rep(0, f) else start

  for (it in 1:maxit) {
    p <- .logit_inv(Z %*% beta)

    # Gradient
    grad <- t(Z) %*% (y - p)

    # Hessian
    W <- as.numeric(p * (1 - p))
    H <- t(Z) %*% (Z * W)
    epsilon <- stats::median(abs(H)) / 100
    H <- H + epsilon * diag(rep(1, nrow(H)))

    # Newton-Raphson
    step <- solve(H, grad)
    beta_new <- beta + step

    # Check convergence
    if (sqrt(sum((beta_new - beta)^2)) < tol) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }

  as.numeric(beta)
}

############################
# E-step
############################

.fast_E_step <- function(R0, R_h, Z,
                          piY, piG0, piGa,
                          beta0, betaList,
                          d, h) {
  # Given the current model (each rater's ability and guessing habits),
  # work out, for every item: how likely each possible true answer is, and
  # how likely it is that each rater actually knew the answer rather than
  # guessed it.
  #
  # Inputs:
  #   R0:  length-n vector of the system's (machine's) answers, coded 1..d,
  #        with 0 marking an item the system never rated
  #   R_h: n x h matrix of human answers, coded 1..d, with 0 marking an item
  #        that rater never rated
  #   Z:   n x f feature matrix (including an intercept column)
  #
  # A 0 entry contributes nothing to that item's candidate-answer scoring,
  # and nothing to that rater's solve/guess counts
  #
  # Output (a list):
  #   - S0:          length-n, estimated chance the system truly knew each item's answer
  #   - Sa:          n x h, estimated chance each human truly knew each item's answer
  #   - P_Y:         length-d, estimated number of items whose true answer is each label
  #   - countG0:     length-d, estimated number of times the system guessed each label
  #   - countGa:     h x d, estimated number of times each human guessed each label
  #   - posteriorYi: n x d, estimated probability of each true answer, per item
  #   - log_lik:     a single number scoring how well the model fits the data so far
  #                  (used to check convergence)

  n <- nrow(Z)

  # Convert each rater's ability parameters into an actual chance they know
  # the answer probability for every item.
  s0_vec <- .logit_inv(Z %*% beta0)
  sA_mat <- matrix(0, n, h)
  for (a in seq_len(h)) {
    sA_mat[, a] <- .logit_inv(Z %*% betaList[[a]])
  }

  # For every item, score how likely each candidate true answer is, given
  # everyone's actual answers. A rater matches a candidate answer either by
  # knowing it, or by guessing it a rater who disagrees with the candidate
  # answer must have guessed whatever they actually said.
  gammaMat <- matrix(0, nrow = n, ncol = d)
  log_lik <- 0

  for (i in 1:n) {
    numer <- numeric(d)
    for (y in 1:d) {
      val <- piY[y]

      r0i <- R0[i]
      if (r0i != 0) {
        if (r0i == y) {
          val <- val * (s0_vec[i] + (1 - s0_vec[i]) * piG0[y])
        } else {
          val <- val * ((1 - s0_vec[i]) * piG0[r0i])
        }
      }

      for (a in 1:h) {
        r_ai <- R_h[i, a]
        if (r_ai == 0) next
        if (r_ai == y) {
          val <- val * (sA_mat[i, a] + (1 - sA_mat[i, a]) * piGa[a, y])
        } else {
          val <- val * ((1 - sA_mat[i, a]) * piGa[a, r_ai])
        }
      }
      numer[y] <- val
    }

    # Turn the scores for this item into probabilities that sum to 1, and
    # add this item's contribution to the overall model fit score.
    denom <- sum(numer)
    if (denom < 1e-15) denom <- 1e-15

    gammaMat[i, ] <- numer / denom
    log_lik <- log_lik + log(denom)
  }

  # For each rater and item, work out the chance they truly knew the answer
  # (rather than guessed it and happened to match). This only applies to the
  # answer they actually gave, weighted by how likely that answer is to be
  # the true one.
  S0 <- rep(NA_real_, n) # chance the system knew the answer, per item (NA where not rated)
  Sa <- matrix(NA_real_, nrow = n, ncol = h) # chance each human knew the answer, per item (NA where not rated)

  for (i in 1:n) {
    r0i <- R0[i]
    if (r0i != 0) {
      denomSolve0 <- s0_vec[i] + (1 - s0_vec[i]) * piG0[r0i]
      if (denomSolve0 < 1e-15) denomSolve0 <- 1e-15
      alpha0i <- s0_vec[i] / denomSolve0
      S0[i] <- gammaMat[i, r0i] * alpha0i
    }

    for (a in 1:h) {
      r_ai <- R_h[i, a]
      if (r_ai == 0) next
      denomSolve_a <- sA_mat[i, a] + (1 - sA_mat[i, a]) * piGa[a, r_ai]
      if (denomSolve_a < 1e-15) denomSolve_a <- 1e-15
      alpha_ai <- sA_mat[i, a] / denomSolve_a
      Sa[i, a] <- gammaMat[i, r_ai] * alpha_ai
    }
  }

  # Sum up, per rater, how often they're estimated to have guessed each
  # label. For the label a rater actually gave, the guess share is
  # whatever's left after subtracting the truly knew it share; for every
  # other candidate true answer, giving that label would have to be a guess.
  countG0 <- numeric(d)
  countGa <- matrix(0, nrow = h, ncol = d)

  for (i in 1:n) {
    r0i <- R0[i]
    if (r0i != 0) {
      alpha0i <- S0[i] / (gammaMat[i, r0i] + 1e-15)
      guessCount0_term <- sum(gammaMat[i, ]) - gammaMat[i, r0i] * alpha0i
      countG0[r0i] <- countG0[r0i] + guessCount0_term
    }

    for (a in 1:h) {
      ra <- R_h[i, a]
      if (ra == 0) next
      alpha_ai <- Sa[i, a] / (gammaMat[i, ra] + 1e-15)
      guessCount_a_term <- sum(gammaMat[i, ]) - gammaMat[i, ra] * alpha_ai
      countGa[a, ra] <- countGa[a, ra] + guessCount_a_term
    }
  }

  # Total (across items) estimated probability mass for each true-answer label.
  P_Y <- colSums(gammaMat)

  list(
    S0 = S0,
    Sa = Sa,
    P_Y = P_Y,
    countG0 = countG0,
    countGa = countGa,
    posteriorYi = gammaMat,
    log_lik = log_lik
  )
}

############################
# Full EM routine: spectral-initialized, converges on observed-data
# log-likelihood (checked before each M-step).
############################

.EM_fit <- function(R0, R_h, Z, d, h,
                     max_iter = 100, tol = 1e-6, quiet = TRUE) {

  # Inputs:
  #   R0:    length-n vector of machine outputs in {1,...,d}
  #   R_h:   n x h matrix of human rater outputs in {1,...,d}
  #   Z:     n x f feature matrix (including intercept)
  #   d:     number of classes
  #   h:     number of human raters
  #   max_iter: maximum EM iterations
  #   tol:      convergence threshold
  #
  # Output:
  #   A list of fitted parameters:
  #     piY:      class prior (length-d)
  #     piG0:     guess distribution for machine (length-d)
  #     piGa:     guess distributions for each human rater (h x d)
  #     beta0:    machine logistic parameters (f-vector)
  #     betaList: list of length h, each logistic parameter vector for rater a
  #   Also returns the final E-step outputs in "final_E" for convenience

  n <- nrow(Z)

  # Initialize piY, piG0, piGa to uniform distributions
  piY <- rep(1/d, d)         # Uniform class prior
  piG0 <- rep(1/d, d)        # Uniform machine guess distribution
  piGa <- matrix(1/d, h, d)  # Uniform guess distributions for human raters

  # Initialize logistic parameters using Spectral Initialization
  ev <- .spectral_initializer(M = R0, H = R_h, R = h)  # Length h + 1
  # Machine:
  beta0 <- c(ev[1], rep(0, ncol(Z) - 1))
  # Humans:
  betaList <- lapply(1:h, function(a) c(ev[a + 1], rep(0, ncol(Z) - 1)))

  # Convergence is on the change in observed-data log-likelihood,
  # log P(R | theta), as computed by .fast_E_step. The check happens
  # before the M-step, so on convergence we return the parameters from
  # the previous iteration's M-step (which produced the current log_lik).
  prev_log_lik <- -Inf
  for (iter in 1:max_iter) {

    # ----------- E-step -----------
    estep <- .fast_E_step(R0, R_h, Z, piY, piG0, piGa, beta0, betaList, d, h)

    S0 <- estep$S0
    Sa <- estep$Sa
    P_Y <- estep$P_Y
    countG0 <- estep$countG0
    countGa <- estep$countGa
    cur_log_lik <- estep$log_lik

    # ----------- Convergence check (log-likelihood) -----------
    diff_ll <- cur_log_lik - prev_log_lik
    if (!quiet) cat("Iteration:", iter, "log-lik:", cur_log_lik, "change:", diff_ll, "\n")
    if (iter > 1 && abs(diff_ll) < tol) {
      if (!quiet) cat("Convergence achieved after", iter, "iterations.\n")
      break
    }
    prev_log_lik <- cur_log_lik

    # ----------- M-step -----------
    # (i) Update pi(Y)
    piY_new <- P_Y / sum(P_Y)

    # (ii) Update pi(G_0) and pi(G_a)
    piG0_new <- countG0 / sum(countG0)
    piGa_new <- piGa
    for (a in 1:h) {
      piGa_new[a, ] <- countGa[a, ] / sum(countGa[a, ])
    }

    # (iii) Update beta0 (machine logistic parameters) fit only on items
    # the system actually rated (R0 != 0)
    obs0 <- R0 != 0
    beta0_new <- .logistic_fit(Z[obs0, , drop = FALSE], S0[obs0], start = beta0)

    # (iv) Update beta_a for each human rater via logistic regression,
    # fit only on the items that rater actually rated (R_h[, a] != 0)
    betaList_new <- list()
    for (a in 1:h) {
      obs_a <- R_h[, a] != 0
      betaList_new[[a]] <- .logistic_fit(Z[obs_a, , drop = FALSE], Sa[obs_a, a], start = betaList[[a]])
    }

    # Accept parameter updates
    piY <- piY_new
    piG0 <- piG0_new
    piGa <- piGa_new
    beta0 <- beta0_new
    betaList <- betaList_new
  }

  # Perform a final E-step to gather final item-level posteriors
  final_estep <- .fast_E_step(R0, R_h, Z, piY, piG0, piGa, beta0, betaList, d, h)

  list(
    piY = piY,
    piG0 = piG0,
    piGa = piGa,
    beta0 = beta0,
    betaList = betaList,
    final_E = final_estep
  )
}

############################
# Spectral initializer: top eigenvector of the pairwise agreement matrix
# (machine as rater 1, humans 2..R+1) seeds each rater's intercept.
# (Smart starting guesses)
############################

.spectral_initializer <- function(M, H, R) {
  # Combine machine output (M) and human outputs (H) into one matrix of responses.
  # Machine is treated as the first "reviewer" (index 1),
  # Humans follow: indices 2,...,(R+1).

  Y <- cbind(M, H)
  n <- nrow(Y)
  if (is.null(n)) {
    # If H is a vector (in case R=1), ensure Y is still a matrix
    Y <- matrix(Y, ncol = R + 1)
    n <- nrow(Y)
  }

  # Compute the agreement matrix A of dimension (R+1) x (R+1)
  A <- .cohens_kappa_numerator(Y)

  # Compute the eigen-decomposition of A
  e <- eigen(A)

  # Find the eigenvector corresponding to the largest eigenvalue
  idx <- which.max(e$values)
  ev <- e$vectors[, idx]

  # If the largest value in ev is negative, flip the sign

  if (max(ev) < 0) {
    ev <- -ev
  }

  return(ev)
}

#############################
# Calculate the numerator of Cohen's kappa for all column pairs in a matrix
##############################

.cohens_kappa_numerator <- function(Y) {

# input: An n x m matrix with integer values (interpreted as categorical labels);
#        0 marks a rater who did not rate that item.
# output: An m x m matrix where element [i,j] contains the difference between observed and expected agreement for columns i and j, over items both raters actually rated

  m <- ncol(Y)

  result <- matrix(0, m, m)

  for (i in 1:m) {
    for (j in i:m) {
      col_i <- Y[, i]
      col_j <- Y[, j]
      keep <- col_i != 0 & col_j != 0
      col_i <- col_i[keep]
      col_j <- col_j[keep]
      nij <- length(col_i)

      if (nij == 0) {
        # These two raters never rated the same item - no basis for an
        # agreement estimate, so leave this entry at its neutral default (0).
        next
      }

      observed <- sum(col_i == col_j) / nij

      all_labels <- unique(c(col_i, col_j))

      expected <- 0
      for (label in all_labels) {
        p_i <- sum(col_i == label) / nij
        p_j <- sum(col_j == label) / nij
        expected <- expected + (p_i * p_j)
      }

      result[i, j] <- observed - expected
      if (i != j) result[j, i] <- result[i, j]
    }
  }

  return(result)
}

############################
# Wrapper Function
############################

.solve_or_guess_fast <- function(evaluations, # data frame with columns: item_id, rater_id, evaluation
                                  difficulty = NULL, # data frame with columns: item_id, feature_id, value
                                  system_rater_id = "system", # rater_id for the system (machine) rater
                                  max_iter = 100, # maximum number of EM iterations
                                  tol = 1e-6, # convergence tolerance for EM
                                  verbose = TRUE # whether to print progress messages
                                  ) { 

  # Step A: Validate and tidy the "evaluations" data
  required_cols_eval <- c("item_id", "rater_id", "evaluation")
  if (!all(required_cols_eval %in% names(evaluations))) {
    stop("`evaluations` must contain columns: item_id, rater_id, evaluation")
  }

  # Identify unique items and raters
  all_items <- unique(evaluations$item_id)
  all_raters <- unique(evaluations$rater_id)

  # Build a full grid so every item has a row for every rater. A rater who
  # never actually rated a given item gets no contribution to that item's
  # fit at all below (coded 0)
  full_grid <- expand.grid(item_id = all_items, rater_id = all_raters,
                            stringsAsFactors = FALSE)
  merged <- left_join(full_grid, evaluations, by = c("item_id", "rater_id"))

  num_missing <- sum(is.na(merged$evaluation))
  if (num_missing > 0) {
    warning(sprintf(
      "There are %d rater-item pairs with no evaluation; excluded from that rater's fit.",
      num_missing
    ))
  }

  evaluation_levels <- sort(unique(evaluations$evaluation))
  d <- length(evaluation_levels)

  # Code each evaluation as its position in evaluation_levels; 0 marks an
  # item that rater never rated.
  merged$evaluation_code <- match(merged$evaluation, evaluation_levels)
  merged$evaluation_code[is.na(merged$evaluation_code)] <- 0L

  # Identify system vs. human raters
  if (!(system_rater_id %in% all_raters)) {
    stop(sprintf("System rater_id '%s' not found in the data!", system_rater_id))
  }
  human_raters <- setdiff(all_raters, system_rater_id)
  rater_order <- c(system_rater_id, human_raters)

  # Order item_id and rater_id to form consistent row/column arrangement
  item_order <- sort(all_items)
  merged$item_id <- factor(merged$item_id, levels = item_order)
  merged$rater_id <- factor(merged$rater_id, levels = rater_order)
  merged <- merged[order(merged$item_id, merged$rater_id), ]

  n <- length(item_order)
  h <- length(human_raters)

  # Create a matrix of dimension (n, 1+h) for all rater evaluations (0 = not rated)
  big_mat <- matrix(0L, nrow = n, ncol = (h + 1))
  for (i_item in seq_len(n)) {
    # subset the rows for item i_item
    irows <- merged[merged$item_id == item_order[i_item], ]
    big_mat[i_item, ] <- irows$evaluation_code
  }
  R0 <- big_mat[, 1] # System
  R_h <- big_mat[, -1, drop = FALSE] # Humans

  # Step B: Build the feature matrix Z
  if (is.null(difficulty)) {
    # no features, just intercept
    Z <- matrix(1, nrow = n, ncol = 1)
    colnames(Z) <- "intercept"
  } else {
    required_cols_diff <- c("item_id", "feature_id", "value")
    if (!all(required_cols_diff %in% names(difficulty))) {
      stop("`difficulty` must contain columns: item_id, feature_id, value")
    }
    wide_diff <- tidyr::pivot_wider(difficulty,
                                     id_cols = "item_id",
                                     names_from = "feature_id",
                                     values_from = "value",
                                     values_fill = 0)
    wide_diff$item_id <- factor(wide_diff$item_id, levels = item_order)
    wide_diff <- wide_diff[order(wide_diff$item_id), ]
    feat_mat <- as.matrix(wide_diff[, setdiff(names(wide_diff), "item_id"), drop = FALSE])
    Z <- cbind(1, feat_mat)
    colnames(Z) <- c("intercept", colnames(feat_mat))
  }
  f <- ncol(Z)

  # Step C: Run the EM Fit
  fit <- .EM_fit(R0, R_h, Z, d, h, max_iter = max_iter, tol = tol, quiet = !verbose)
  # The final E-step results are in fit$final_E
  estep_f <- fit$final_E

  piY <- fit$piY
  piG0 <- fit$piG0
  piGa <- fit$piGa
  beta0 <- fit$beta0
  betaList <- fit$betaList

  # Step D: Build the four requested tidy data outputs

  # 1) rater_ability
  df_system <- data.frame(rater_id = rep(system_rater_id, f),
                           parameter_id = colnames(Z),
                           estimate = beta0,
                           stringsAsFactors = FALSE)
  df_human <- do.call(rbind, lapply(seq_len(h), function(a) {
    data.frame(rater_id = rep(human_raters[a], f),
               parameter_id = colnames(Z),
               estimate = betaList[[a]],
               stringsAsFactors = FALSE)
  }))
  rater_ability <- bind_rows(df_system, df_human)

  # 2) item_estimates
  s0_vec <- .logit_inv(Z %*% beta0)
  sA_mat <- matrix(0, n, h)
  for (a in 1:h) {
    sA_mat[, a] <- .logit_inv(Z %*% betaList[[a]])
  }
  S0_post <- estep_f$S0
  Sa_post <- estep_f$Sa

  all_rows <- list()
  idx <- 1
  for (i_item in seq_len(n)) {
    cur_item_id <- item_order[i_item]
    all_rows[[idx]] <- data.frame(item_id = cur_item_id,
                                   rater_id = system_rater_id,
                                   success_prob = s0_vec[i_item],
                                   expected_success = S0_post[i_item],
                                   stringsAsFactors = FALSE)
    idx <- idx + 1

    for (a in seq_len(h)) {
      all_rows[[idx]] <- data.frame(item_id = cur_item_id,
                                     rater_id = human_raters[a],
                                     success_prob = sA_mat[i_item, a],
                                     expected_success = Sa_post[i_item, a],
                                     stringsAsFactors = FALSE)
      idx <- idx + 1
    }
  }
  item_estimates <- bind_rows(all_rows)

  # 3) guessing_distribution
  guess_rows <- list()
  for (k in seq_len(d)) {
    guess_rows[[length(guess_rows) + 1]] <- data.frame(rater_id = system_rater_id,
                                                         evaluation = evaluation_levels[k],
                                                         probability = piG0[k],
                                                         stringsAsFactors = FALSE)
  }
  for (a in seq_len(h)) {
    for (k in seq_len(d)) {
      guess_rows[[length(guess_rows) + 1]] <- data.frame(rater_id = human_raters[a],
                                                           evaluation = evaluation_levels[k],
                                                           probability = piGa[a, k],
                                                           stringsAsFactors = FALSE)
    }
  }
  guessing_distribution <- bind_rows(guess_rows)

  # 4) class_distribution
  class_rows <- list()
  idx <- 1
  for (i_item in seq_len(n)) {
    cur_item_id <- item_order[i_item]
    for (k in seq_len(d)) {
      class_rows[[idx]] <- data.frame(item_id = cur_item_id,
                                       evaluation = evaluation_levels[k],
                                       prior_prob = piY[k],
                                       posterior_prob = estep_f$posteriorYi[i_item, k],
                                       stringsAsFactors = FALSE)
      idx <- idx + 1
    }
  }
  class_distribution <- bind_rows(class_rows)

  list(
    rater_ability = tibble::as_tibble(rater_ability),
    item_estimates = tibble::as_tibble(item_estimates),
    guessing_distribution = tibble::as_tibble(guessing_distribution),
    class_distribution = tibble::as_tibble(class_distribution)
  )
}

############################
# Nonparametric (item-level cluster) bootstrap: resample items with
# replacement, relabel duplicates so each draw is treated as a distinct
# item, refit .solve_or_guess_fast, and collect rater_ability across
# B replicates for percentile confidence intervals.
############################

.nonparametric_bootstrap_solve_or_guess <- function(
    evaluations, # data frame with columns: item_id, rater_id, evaluation
    difficulty = NULL, # data frame with columns: item_id, feature_id, value
    system_rater_id = "system", # rater_id for the system (machine) rater
    B = 100, # number of bootstrap replicates
    max_iter = 100, # maximum number of EM iterations
    tol = 1e-6, # convergence tolerance for EM
    progress = TRUE # whether to show a progress bar for the B replicates
) {

  # 1) Identify the set of distinct items We'll resample from these item_ids with replacement.
  unique_items <- unique(evaluations$item_id)
  n_items <- length(unique_items)

  # 2) Create a helper function that, given a vector of sampled items, constructs a new (bootstrap) dataset for that replicate.
  build_bootstrap_dataset <- function(sampled_items) {
    # 1) Start with an empty list for evaluations
    eval_list <- list()
    # 2) Similarly for difficulty
    diff_list <- list()

    # Each element of sampled_items might be repeated or unique
    # We'll assign a suffix .1, .2, ... for each repeated occurrence
    for (i in seq_along(sampled_items)) {
      orig_item <- sampled_items[i]
      # Assign a new item_id for this replicated version of the item
      new_item_id <- paste0(orig_item, ".", i)

      # 1) Filter the original evaluations for this item
      sub_eval <- filter(evaluations, item_id == orig_item)
      # Rename item_id to new_item_id
      if (nrow(sub_eval) > 0) sub_eval$item_id <- new_item_id

      # 2) Filter the original difficulty for this item (if difficulty is provided)
      if (!is.null(difficulty) && nrow(difficulty) > 0) {
        sub_diff <- filter(difficulty, item_id == orig_item)
        if (nrow(sub_diff) > 0) sub_diff$item_id <- new_item_id
      } else {
        sub_diff <- tibble::tibble(item_id = character(0),
                                    feature_id = character(0),
                                    value = numeric(0))
      }

      eval_list[[i]] <- sub_eval
      diff_list[[i]] <- sub_diff
    }

    # Combine all the replicated evaluations and difficulty into data frames
    eval_boot <- bind_rows(eval_list)
    diff_boot <- bind_rows(diff_list)
    if (nrow(diff_boot) == 0) diff_boot <- NULL

    # Return a list containing the bootstrap evaluations and difficulty
    list(evaluations = eval_boot, difficulty = diff_boot)
  }


  # ---------------------------------------------------------------------------
  # 3) MAIN BOOTSTRAP LOOP
  #    - For each of B replicates, we:
  #      (a) sample n_items from unique_items (with replacement)
  #      (b) build the new dataset
  #      (c) call .solve_or_guess_fast() on that dataset
  #      (d) store rater_ability results
  #    Runs in parallel whenever the caller has set a non-sequential
  #    future::plan() (e.g. future::plan(multisession)), same as tabscreen_gpt().
  # ---------------------------------------------------------------------------
  all_boot_estimates <- furrr::future_map(
    seq_len(B),
    function(b) {
      # (a) Sample item_ids (with replacement). We sample exactly n_items to match the original dataset size
      sampled_items <- sample(unique_items, size = n_items, replace = TRUE)

      # (b) Build the new dataset for these sampled items
      boot_data <- build_bootstrap_dataset(sampled_items)

      # (c) Re-fit solve_or_guess on this new dataset
      boot_fit <- .solve_or_guess_fast(
        evaluations = boot_data$evaluations,
        difficulty = boot_data$difficulty,
        system_rater_id = system_rater_id,
        max_iter = max_iter,
        tol = tol,
        verbose = FALSE
      )
      # (d) Return the rater_ability tibble, tagged with the replicate number
      mutate(boot_fit$rater_ability, bootstrap_iter = b)
    },
    .options = furrr::furrr_options(seed = TRUE),
    .progress = progress
  )

  # Combine all replicate results into a single data frame
  all_boot_estimates_df <- bind_rows(all_boot_estimates)

  list(
    n_boot = B,
    bootstrap_rater_ability = all_boot_estimates_df
  )
}

############################
# Convert a tabscreen result object ('gpt'/'gpt_tbl'/'gpt_agg_tbl') into the
# wide, one-row-per-item table expected by solve_or_guess(): an "item_id"
# column, one column per screened model (named after the model), and the
# human decision column(s)
############################

.tabscreen_to_wide <- function(x, # tabscreen result object
                                human_decision # Name(s) of column(s) containing human decisions
  ) {

  if (!is.character(human_decision) || length(human_decision) < 1) {
    stop("`reference_raters` must be a character vector naming one or more columns ",
         "with human decisions (coded 1 = include, 0 = exclude) when `evaluations` ",
         "is a tabscreen result object.")
  }

  # If the input is of class 'gpt',
  # Use either the aggregated data if available (multiple models/prompts/iterations used),
  # otherwise use answer_data.
  if (is_gpt(x)) {
    if ("answer_data_aggregated" %in% names(x) && !is.null(x$answer_data_aggregated)) {
      dat <- x$answer_data_aggregated
      decision_col <- "final_decision_gpt"
      topp_col <- "top_p"
      raw_scale <- FALSE
    } else {
      dat <- x$answer_data
      decision_col <- "decision_binary"
      topp_col <- "topp"
      raw_scale <- TRUE
    }
  } else if (is_gpt_agg_tbl(x)) {
    dat <- x
    decision_col <- "final_decision_gpt"
    topp_col <- "top_p"
    raw_scale <- FALSE
  } else if (is_gpt_tbl(x)) {
    dat <- x
    decision_col <- "decision_binary"
    topp_col <- "topp"
    raw_scale <- TRUE
  } else {
    stop("`evaluations` must be a data.frame/tibble with one row per item, or a ",
         "tabscreen result object of class 'gpt', 'gpt_tbl', or 'gpt_agg_tbl'.")
  }

  missing_human <- setdiff(human_decision, names(dat))
  if (length(missing_human) > 0) {
    stop(sprintf("`reference_raters` column(s) not found: %s", paste(missing_human, collapse = ", ")))
  }

  # Each screened model becomes its own column/rater, but the result must
  # reflect a single prompt/top_p configuration
  config_cols <- intersect(c("promptid", topp_col), names(dat))
  if (length(config_cols) > 0) {
    n_configs <- dat |> distinct(across(all_of(config_cols))) |> nrow()
    if (n_configs > 1) {
      stop("The supplied screening result contains more than one prompt/top_p configuration. ",
           "solve_or_guess() treats each model as its own rater but assumes a single underlying ",
           "screening question, so subset to one prompt/top_p first, e.g. ",
           "`dplyr::filter(result$answer_data_aggregated, promptid == 1)`.")
    }
  }

  dat[[decision_col]] <- if (raw_scale) {
    case_when(dat[[decision_col]] == 1 ~ "Include", dat[[decision_col]] == 0 ~ "Exclude", TRUE ~ NA_character_)
  } else {
    as.character(dat[[decision_col]])
  }

  dat$item_id <- as.character(dat$studyid)

  wide <-
    dat |>
    select(item_id, model, all_of(decision_col), all_of(human_decision)) |>
    tidyr::pivot_wider(
      id_cols = c(item_id, all_of(human_decision)),
      names_from = model,
      values_from = all_of(decision_col)
    ) |>
    mutate(across(all_of(human_decision), ~ case_when(.x == 1 ~ "Include", .x == 0 ~ "Exclude", TRUE ~ NA_character_)))

  wide
}
