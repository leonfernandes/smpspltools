#' GARCH Model
#'
#' Fits GARCH model using [fGarch][fGarch::fGarch-package]. Currently this is
#' fixed to be a GARCH(1, 1) model and the `formula` supplied is ignored.
#'
#' @param formula Model specification (see "Specials" section).
#' @param ... Further arguments for [`stats::arima()`]
#'
#' @section Specials:
#' The _specials_ define the orders of the GARCH model. Currently this is fixed
#' to be a GARCH(1, 1) model and the formula supplied is ignored.
#'
#' @export
GARCH <-
    function(formula, ...) {
        if (!rlang::is_installed("fabletools")) {
            rlang::abort("Suggested package `fabletools` is not installed.")
        }
        if (!rlang::is_installed("tsibble")) {
            rlang::abort("Suggested package `tsibble` is not installed.")
        }
        if (!rlang::is_installed("fGarch")) {
            rlang::abort("Suggested package `fGarch` is not installed.")
        }
        model_garch <- fabletools::new_model_class("garch",
            # The training method (more on this later)
            train = train_garch,
            # The formula specials (the next section)
            specials = specials_garch,
            check = function(.data) {
                if (!tsibble::is_regular(.data)) stop("Data must be regular")
            }
        )

        # Return a model definition which stores the user's model specification
        fabletools::new_model_definition(model_garch, {{ formula }}, ...)
    }

specials_garch <-
    fabletools::new_specials(
        pq = function(p = 1, q = 1) as.list(environment()),
        fabletools::common_xregs,
        xreg = function(...) {
            rlang::abort("Exogenous regressors aren't supported by `GARCH()`")
        },
        .required_specials = "pq"
    )

train_garch <-
    function(.data, specials, ...) {
        # Extract a vector of response data
        mv <- tsibble::measured_vars(.data)
        if (length(mv) > 1) stop("GARCH() is a univariate model.")
        y <- .data[[mv]]

        # Pull out inputs from the specials
        if (length(specials$garch) > 1) {
            rlang::warn(
                "Only one special for `garch()` is allowed,
                defaulting to the first usage."
            )
        }
        m <- specials$garch[[1]]

        # use valuue as column name and p = 1, q = 1
        fGarch::garchFit(
            formula = value ~ garch(1, 1),
            data = y,
            trace = FALSE,
            include.mean = FALSE,
            ...
        )
    }