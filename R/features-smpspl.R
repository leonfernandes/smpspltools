#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @param .var The variable to compute features on.
#' @rdname features_smpspl
#' @exportS3Method fabletools::features
features.smpspl_grid <-
    function(.tbl, .var, features, ...) {
        if (!rlang::is_installed("fabletools")) {
            rlang::abort("Suggested package `fabletools` is not installed.")
        }
        if (!rlang::is_installed("smpspl")) {
            rlang::abort("Suggested package `smpspl` is not installed.")
        }
        if (!rlang::is_installed("purrr")) {
            rlang::abort("Suggested package `purrr` is not installed.")
        }
        if (!rlang::is_installed("dplyr")) {
            rlang::abort("Suggested package `dplyr` is not installed.")
        }
        if (!rlang::is_installed("tibble")) {
            rlang::abort("Suggested package `tibble` is not installed.")
        }
        .var <- rlang::enquo(.var)
        .tbl |>
            dplyr::mutate(
                .assessment = purrr::map(
                    !!.var,
                    ~ .x |>
                        dplyr::mutate(
                            .features = purrr::map(
                                .subresid,
                                function(.) {
                                    . |>
                                        fabletools::features(
                                            .var = .resid,
                                            features = features
                                        )
                                }
                            )
                        ) |>
                        dplyr::select(-.subresid)
                )
            ) |>
            tibble::new_tibble(class = "smpspl_nested_features")
    }
