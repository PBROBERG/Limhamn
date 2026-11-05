
#' Hight Order Universal Portfolio
#'
#' @param returns Matrix (T × m) of simple returns (price relatives)
#' @param eval_points Number of portfolios in the mixture (default: 10,000)
#' @param leverage Maximum leverage (default: 1.0).
#'   - `leverage = 1`: Standard simplex (no shorting).
#'   - `leverage < 1`: Shrinks toward uniform CRP.
#'   - `leverage > 1`: Allows negative weights (short positions).
#' @return List with:
#'   - `weights`: Matrix (T × m) of UP weights for each period.
#'   - `state`: Final internal state (W, S, leverage).

houp <- function(returns, order = 2, eval_points = 10000, leverage = 1.0, parameter = 0.5) {
  stopifnot(is.matrix(returns), nrow(returns) >= 1, ncol(returns) >= 1, order >= 1)
  T <- nrow(returns)
  m <- ncol(returns)
  eval_points <- as.integer(eval_points)

  # Store all intermediate UP weights
  all_up_weights <- list()
  current_returns <- returns

  for (k in 1:order) {
    result <- up_run_cover1991(current_returns, eval_points, leverage, parameter)
    all_up_weights[[k]] <- result$weights
    if (k < order) {
      up_returns <- rowSums(result$weights * current_returns)
      current_returns <- cbind(current_returns, up_returns)
    }
  }

  # Translate back to original m assets
  final_weights <- all_up_weights[[order]]
  for (k in order:2) {
    up_weight <- final_weights[, ncol(final_weights), drop = TRUE]
    direct_weights <- final_weights[, -ncol(final_weights), drop = FALSE]
    up_weights_prev <- all_up_weights[[k - 1]]
    final_weights <- direct_weights + outer(up_weight, rep(1, ncol(direct_weights)), "*") * up_weights_prev
  }

  # ✅ Force first-period weights to be uniform (Cover 1991)
  final_weights[1, ] <- rep(1/m, m)

  list(weights = final_weights, all_up_weights = all_up_weights)
}
