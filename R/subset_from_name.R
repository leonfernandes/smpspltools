subset_from_name <-
    function(object, str_to_detect) {
        idx <-
            object |>
            names() |>
            stringr::str_detect(str_to_detect) |>
            which()
        ret <- object[idx]
        if (!length(ret))
            ret <- 0
        ret
    }