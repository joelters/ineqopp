#' Plot estimated fitted values and true fitted values (if available) per
#' cross-fitting fold
#'
#'
#'
#' @param fold_list_fvs list with a dataframe per CF fold, can be found as
#' output called FVsinfold of function IOp
#' @param sign_error_list list with a vector of sgn coincidences for each pair
#' for each fold, can be found as output called sgns of function IOp
#' @returns Faceted plot
#'
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#'
#' Terschuur, J. (2022). Debiased Machine Learning Inequality
#' of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
#'
#'
#' @export
plot_fvs_fvs0_folds <- function(data_list, sign_error_list) {
  combined_df <- purrr::pmap_dfr(
    list(data_list, sign_error_list, seq_along(data_list)),
    function(df, sign_vec, idx) {
      df_sorted <- dplyr::arrange(df, FVs0)
      rmse <- sqrt(mean((df_sorted$FVs - df_sorted$FVs0)^2, na.rm = TRUE))
      sign_error <- mean(sign_vec, na.rm = TRUE)

      tibble::tibble(
        index = seq_len(nrow(df_sorted)),
        FVs0 = df_sorted$FVs0,
        FVs = df_sorted$FVs,
        facet = paste0("Plot ", idx,
                       " — RMSE = ", round(rmse, 1),
                       " | Sign Err = ", round(sign_error, 3))
      )
    }
  )

  long_df <- tidyr::pivot_longer(
    combined_df,
    cols = c("FVs0", "FVs"),
    names_to = "variable",
    values_to = "value"
  )

  ggplot2::ggplot(long_df, ggplot2::aes(x = index, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ facet, scales = "free_y") +
    ggplot2::labs(
      x = "Index (Sorted by FVs0)",
      y = "Value",
      title = "FVs0 and FVs with RMSE and Sign Error"
    ) +
    ggplot2::theme_minimal()
}
