#' Extracts available models
#'
#' Generic function to extract and return objects that have been registered with
#' this package. For now only `arima` and `garch` objects are registered. This
#' function translates functions of class `Arima`, `Arima_fit_impl`, `AR`,
#' `ARIMA` and `fGARCH` to the corresponding registered methods. This function
#' also extracts the fitted models from `model_fit`, `workflow` and `mdl_ts`
#' objects.
#' @param object An object.
#' @param ... unused.
#' @export
extract_model <- function(object, ...) UseMethod("extract_model")

#' @rdname extract_model
#' @export
extract_model.default <-
    function(object, ...) {
        rlang::abort(glue::glue("Model class {class(object)} is unregistered."))
    }

## arima ----
#' @rdname extract_model
#' @export
extract_model.arima <- function(object, ...) object

## Arima ----
#' @rdname extract_model
#' @export
extract_model.Arima <-
    function(object, ...) {
        ret <- object$model
        class(ret) <- "arima"
        ret
    }

## tidymodels ----
#' @rdname extract_model
#' @export
extract_model.model_fit <-
    function(object, ...) extract_model(hardhat::extract_fit_engine(object))

#' @rdname extract_model
#' @export
extract_model.workflow <-
    function(object, ...) extract_model(hardhat::extract_fit_engine(object))

## modeltime ----
#' @rdname extract_model
#' @export
extract_model.Arima_fit_impl <-
    # manually use .Arima method
    function(object, ...) extract_model.Arima(object$models$model_1, ...)

## fable ----
#' @rdname extract_model
#' @export
extract_model.mdl_ts <- function(object, ...) extract_model(object$fit, ...)

#' @rdname extract_model
#' @export
extract_model.ARIMA <- function(object, ...) extract_model(object$model, ...)

#' @rdname extract_model
#' @export
extract_model.AR <-
    function(object, ...) make_arima(phi = object$coef, theta = 0, delta = 0)

## garch ----
#' @rdname extract_model
#' @export
extract_model.garch <- function(object, ...) object

#' @rdname extract_model
#' @export
extract_model.fGARCH <- function(object, ...) {
    par <- fGarch::coef(object)
    par_list <-
        c(omega = "omega", alpha = "alpha", beta = "beta") |>
        lapply(function(.) subset_from_name(par, .))
    do.call(make_garch, par_list)
}