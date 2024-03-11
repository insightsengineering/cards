# cards 0.1.0.9006

* Improved messaging in `check_pkg_installed()` that incorporates the calling function name in the case of an error. (#205)

* Styling from the {cli} package are now removed from errors and warnings when they are captured with `eval_capture_conditions()`. Styling is removed with `cli::ansi_strip()`. (#129)

* Updated `is_pkg_installed()` and `check_pkg_installed()` to allow checks for more than package at a time. The `get_min_version_required()` function has also been updated to return a tibble instead of a list with attributes. (#201)

# cards 0.1.0

* Initial release.
