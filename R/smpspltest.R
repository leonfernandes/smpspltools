#' Sum of squares
#'
#' Computes sum of squares of an object obtained from an application of
#' `fabletools::features`. This forms the chi-squared test statistic upto
#' scaling.
#' @param object an object.
#' @param .metric a character to filter the `metric` column.
#' @param .max_lag maximum lag
#' @export
#' @examples
#' \dontrun{
#' library(fable)
#' library(smpspl)
#' data <-
#'     tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#'
#' # Consider an AR(1) model
#' o <-
#'     ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'     smpspl_grid(data, 2, 2)
#'
#' # Calculate lanyard features
#' my_acf <-
#'     function(x) {
#'         data.frame(t = x, e = 0) |>
#'             lanyard::acf_metric(t, e) |>
#'             generics::tidy()
#'     }
#'
#' #Calculate test statistic
#' o |>
#'  features(.resid, .subresid, .assessment, features = my_acf) |>
#'  tidyr::unnest(.nested_features) |>
#'  tidyr::unnest(.features) |>
#'  dplyr::filter(analysis_idx == 50, assessment_idx == 100) |>
#'  smpspltest("autocorrelation", 30)
#'
#' #Compare with chi-square
#' stats::qchisq(0.95, df = 30) / 100
#' }
smpspltest <-
    function(object, .metric, .max_lag) {
        lag <- rlang::sym("lag")
        estimate <- rlang::sym("estimate")
        metric <- rlang::sym("metric")
        stopifnot(inherits(object, "tbl_df") == TRUE)
        ret <-
            object |>
            dplyr::filter(lag <= .max_lag, lag > 0, metric == .metric) |>
            dplyr::mutate(estimate = estimate^2) |>
            dplyr::pull(estimate) |>
            sum()
        return(ret)
    }