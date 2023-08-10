#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @param .var The variable to compute features on.
#' @export
#' @return An `smpspl_boot_features` object.
#' @examples
#' \dontrun{
#' library(fable)
#' library(smpspl)
#' data <-
#'     tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'     ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'     smpspl_boot(data, 50, 100, 100)
#'
#' # Calculate lanyard features
#' my_acf <-
#'     function(x) {
#'         data.frame(t = x, e = 0) |>
#'             lanyard::acf_metric(t, e) |>
#'             generics::tidy()
#'     }
#'
#' o |>
#'     features(.resid, features = my_acf)
#' }
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
        idx <- tsibble::index(.tbl)
        data_list <-
            .tbl |>
            tsibble::as_tibble() |>
            dplyr::group_by(boot_id) |>
            dplyr::group_split()
        num_boots <- length(data_list)
        p <- progressr::progressor(steps = num_boots)
        data_list |>
            purrr::map(
                \(.x) {
                    p()
                    fabletools::features(
                        tsibble::as_tsibble(.x, index = idx),
                        !!rlang::enquo(.var),
                        features = features
                    )
                }
            ) |>
            purrr::list_rbind(names_to = "boot_id") |>
            dplyr::mutate(boot_id = factor(boot_id)) |>
            tibble::new_tibble(class = "smpspl_boot_features")
    }