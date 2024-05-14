# check_ard_structure() works

    Code
      check_ard_structure(structure(dplyr::select(dplyr::mutate(ard_continuous(ADSL,
        variables = "AGE"), stat = unlist(stat)), -error), class = "data.frame"))
    Message
      Object is not of class <card>.
      The following columns are not present: "error".
      Expecting a row with `stat_name = 'method'`, but it is not present.
      The following columns are expected to be list columns: "stat".

