#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @param .var The variable to compute features on.
#' @rdname features_smpspl
#' @export
features.smpspl_boot <-
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
        if (!rlang::is_installed("tsibble")) {
            rlang::abort("Suggested package `tsibble` is not installed.")
        }
        boot_id <- rlang::sym("boot_id")
        .resid <- rlang::sym(".resid")
        .var <- NULL # rlang::enquo(.var) ## unused
        idx <- tsibble::index(.tbl)
        .tbl |>
            tsibble::as_tibble() |>
            dplyr::group_by(boot_id) |>
            dplyr::group_split() |>
            purrr::map(
                ~ fabletools::features(
                    tsibble::as_tsibble(.x, index = idx),
                    .resid,
                    features = features
                )
            ) |>
            purrr::list_rbind(names_to = "boot_id") |>
            dplyr::mutate(boot_id = factor(boot_id)) |>
            tibble::new_tibble(class = "smpspl_boot_features")
    }