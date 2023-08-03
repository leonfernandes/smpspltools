#' Sample Split Wrapper for a List of Models
#'
#' Convenient wrapper for iterating over a list of models. The `object`
#' parameter of `fun` should take a registered model to be fit to the data.
#' @param model_list list of models to be fit.
#' @param fun a [smpspl][smpspl::smpspl-package] function to be applied.
#' @param ... parameters other than "object" of `fun`.
#' @export
smpspl_list <-
    function(model_list, fun, ...) {
        model_id <- rlang::sym("model_id")
        dot_args <- list(...)
        my_fun <-
            function(.x) {
                do.call(fun, append(list(object = .x), dot_args))
            }
        ret <-
            model_list |>
            purrr::map(~ tibble::tibble(fun = my_fun(.x)))
        ret <-
            ret |>
            purrr::list_rbind(names_to = "model_id") |>
            dplyr::mutate(model_id = factor(model_id)) |>
            tibble::new_tibble(class = "smpspl_list")
        return(ret)
    }