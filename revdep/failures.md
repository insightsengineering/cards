# gtsummary

<details>

* Version: 2.1.0
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2025-02-19 23:30:02 UTC
* Number of recursive dependencies: 206

Run `revdepcheck::revdep_details(, "gtsummary")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gtsummary-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tbl_strata_nested_stack
    > ### Title: Stratified Nested Stacking
    > ### Aliases: tbl_strata_nested_stack
    > 
    > ### ** Examples
    > 
    > # Example 1 ----------------------------------
    ...
      2.   ├─rlang::set_names(...)
      3.   └─gtsummary:::map(...)
      4.     └─base::lapply(.x, .f, ...)
      5.       └─gtsummary (local) FUN(X[[i]], ...)
      6.         ├─dplyr::select(...)
      7.         └─cards::rename_ard_columns(...)
      8.           └─cards:::check_class(x, "card")
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   R CMD check timed out
    

