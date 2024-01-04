# check_ard_structure() works

    Code
      check_ard_structure(structure(dplyr::select(dplyr::mutate(ard_continuous(ADSL,
        variables = "AGE"), statistic = unlist(statistic)), -error), class = "data.frame"))
    Message
      Object is not of class <card>.
      The following columns are not present: "error".
      The following columns are expected to be list columns: "statistic".

