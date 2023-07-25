#' Check if model is registered
#'
#' @param object an object.
#' @param ... unused.
#' @export
is_smpspl_reg <-
    function(object, ...) {
        (!inherits(object, "arima") && !inherits(object, "garch"))
    }