#' Extracts outcome column name
#'
#' Generic function to extract and return outcome variable name from various
#' classes of objects. Currently implemented for `formula`, `model_spec`,
#' `workflow`, `model_fit`, `Arima`, `Arima_fit_impl`, `mdl_defn` and `mdl_ts`
#' objects.
#' @param object an object to extract model from.
#' @param ... additional arguements such as `formula`.
#' @export
extract_outcome <- function(object, ...) UseMethod("extract_outcome")

#' @rdname extract_outcome
#' @export
extract_outcome.default <-
    function(object, ...) {
        if (inherits(object, "mdl_defn")) {
            return(extract_outcome(rlang::eval_tidy(object$formula)))
        }
        rlang::abort(
            glue::glue("Outcome of class {class(object)} cannot be determined.")
        )
    }

#' @rdname extract_outcome
#' @export
extract_outcome.formula <- function(object, ...) tune::outcome_names(object)

## tidymodels ----
#' @rdname extract_outcome
#' @export
extract_outcome.model_spec <-
    function(object, ...) extract_outcome(list(...)$formula)

#' @rdname extract_outcome
#' @export
extract_outcome.workflow <-
    function(object, ...) tune::outcome_names(object)

#' @rdname extract_outcome
#' @export
extract_outcome.model_fit <-
    function(object, ...) tune::outcome_names(object$preproc$terms)

## Arima ----
#' @rdname extract_outcome
#' @export
extract_outcome.Arima <- function(object, ...) object$series

## modeltime ----
#' @rdname extract_outcome
#' @export
extract_outcome.Arima_fit_impl <-
    function(object, ...) object$models$model_1$series

## fable ----
#' @rdname extract_outcome
#' @export
extract_outcome.mdl_ts <-
    function(object, ...) fabletools::response_vars(object)