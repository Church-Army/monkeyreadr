default_cols <- function(){
  c("Respondent ID", "Collector ID", "IP Address", "Email Address",
    "First Name", "Last Name", "Custom Data 1")
}

read_sm <- function(x, clean_names = TRUE, drop_surplus_cols = TRUE){

  ## determine cleaning function from clean_names -------------------
  stopifnot(length(clean_names) == 1)

  if(!is.function(clean_names)){
    name_cleaner <- ifelse(clean_names, janitor::make_clean_names, identity)
  } else name_cleaner <- clean_names

  ## read sm_data ---------------------------------------------------
  suppressMessages({
    sm_data <- vroom::vroom(x, show_col_types = FALSE)
    })

  missing_names <- stringr::str_detect(names(sm_data), "^\\.\\.\\.\\d+$")

  sm_data <- dplyr::rename_with(sm_data, name_cleaner, everything())

  ## Assign correct types (where known) ----------------------------------------

  default_cols <- name_cleaner(default_cols())

  sm_data <-
    dplyr::mutate(
      sm_data,
      dplyr::across(
        dplyr::any_of(default_cols), as.character)
      )

  sm_data <-
    dplyr::mutate(
      sm_data,
      dplyr::across(any_of(name_cleaner(c("Start Date", "End Date"))),
                    lubridate::mdy_hms
      )
    )

  ## Replace missing names w/ values from first row ----------------------------
  first_row <- unlist(sm_data[1, ])
  sm_data <- sm_data[-1, ]

  repaired_names <- name_cleaner(paste(first_row[missing_names], which(missing_names)))
  old_names <- names(sm_data)[missing_names]

  names(sm_data)[missing_names] <- repaired_names

  if(length(repaired_names) > 0){

    repaired_names_to_print <-
      paste(old_names, "->", repaired_names, sep = " ")

    rlang::inform(message = "Repaired names:",
                  class = "sm_name_repair",
                  body = repaired_names_to_print
                  )
  }


  ## Drop surplus columns ------------------------------------------------------
  if(drop_surplus_cols){

    all_na <- \(x) all(is.na(x))

    sm_data <- dplyr::select(sm_data, -(any_of(default_cols) & where(all_na)))

  }

  sm_data
}
