nested_nested_mutate <-
    function(.tbl, .var, .var_nest1, .var_nest2, .fun, .new_col) {
        .tbl |>
            dplyr::mutate(
                {{.var_nest2}} := purrr::map(
                    !!rlang::enquo(.var_nest2),
                    ~ dplyr::mutate(
                        .x,
                        {{.var_nest1}} := purrr::map(
                            !!rlang::enquo(.var_nest1),
                            \(y) dplyr::mutate(
                                y,
                                {{.new_col}} := .fun(
                                    !!rlang::enquo(.var)
                                )
                            )
                        )
                    )
                )
            )
    }