#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @param .var The variable to compute features on.
#' @param .var_nest1 The variable `.var` is nested in.
#' @param .var_nest2 The variable `.var_nest1` is nested in. This is a column
#' of `.tbl`.
#' @importFrom fabletools features
#' @importFrom rlang `:=`
#' @rdname features_smpspl
#' @export
features.smpspl_grid <-
    function(.tbl, .var, .var_nest1, .var_nest2, features, ...) {
        .var <- rlang::enquo(.var)
        .var_nest1 <- rlang::enquo(.var_nest1)
        .var_nest2 <- rlang::enquo(.var_nest2)
        features_grid_impl(
            .tbl,
            .var,
            .var_nest1,
            .var_nest2,
            features,
            ...
        )
    }

#' Features for sample splitting
#'
#' @inheritParams fabletools::features_at
#' @param .vars The variables to compute features on.
#' @importFrom fabletools features_at
#' @rdname features_smpspl
#' @export
features_at.smpspl_grid <-
    function(.tbl, .vars, .var_nest1, .var_nest2, features, ...) {
        .vars <- rlang::enquos(.vars)
        .var_nest1 <- rlang::enquo(.var_nest1)
        .var_nest2 <- rlang::enquo(.var_nest2)
        features_grid_impl(
            .tbl,
            .vars,
            .var_nest1,
            .var_nest2,
            features,
            ...
        )
    }

features_grid_impl <-
    function(.tbl, .var, .var_nest1, .var_nest2, features, ...) {
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
        steps <-
            .tbl |>
            dplyr::pull(!!.var_nest2) |>
            purrr::map_int(vctrs::vec_size) |>
            sum()
        steps <- vctrs::vec_size(.tbl) * steps
        p <- progressr::progressor(steps = steps)
        if (rlang::is_quosure(.var)) {
            fn <-
                \(.x) {
                    p()
                    fabletools::features(
                    .x, .var = !!.var, features = features
                    )
                }
        } else if (rlang::is_quosures(.var)) {
           fn <-
                \(.x) {
                    p()
                    fabletools::features(
                    .x, .vars = !!.var, features = features
                    )
                }
        }
        .tbl |>
            dplyr::mutate(
                .nested_features = purrr::map(
                    !!.var_nest2,
                    ~ .x |>
                        dplyr::mutate(
                            .features = purrr::map(!!.var_nest1, fn),
                            .keep = "unused"
                        )
                ),
                .keep = "unused"
            ) |>
            tibble::new_tibble(class = "smpspl_grid_features")
    }