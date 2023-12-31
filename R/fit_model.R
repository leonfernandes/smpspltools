#' Wrapper to fit model
#'
#' Convenient wrapper function to fit models of class `model_spec`, `workflow`
#' or `model_dfn`.
#' @param .object an object
#' @param .data data to fit the model
#' @param ... additional arguments for fitting the model.
#' @export
fit_model <-
    function(.object, .data, ...) {
        UseMethod("fit_model")
    }

#' @rdname fit_model
#' @export
fit_model.default <-
    function(.object, .data, ...) {
        if (inherits(.object, "mdl_defn")) {
            return(fabletools::estimate(.model = .object, .data = .data, ...))
        }
        rlang::abort(
            glue::glue("No estimation method for class {class(.object)}.")
        )
    }

#' @rdname fit_model
#' @export
fit_model.model_spec <-
    function(.object, .data, ...) {
        dot_args <- list(...)
        if (!("formula" %in% names(dot_args))) {
            rlang::abort("`formula` not specified.")
        }
        generics::fit(
            object = .object,
            data = .data,
            ...
        )
    }

#' @rdname fit_model
#' @export
fit_model.workflow <-
    function(.object, .data, ...) {
        generics::fit(
            object = .object,
            data = .data,
            ...
        )
    }