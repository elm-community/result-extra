# CHANGELOG

## v2.4.0

* Add additinal `combine...` functions for mapping & pulling `Result` values
  out of lists and tuples.
* Add a `join` function for merging nested `Result` values.
* Add a `filter` function for validating an `Ok` value or returning an `Err`.


## v2.3.0

* Improve test coverage.
* Add toTask function to convert Results into Tasks.
* Add error function to convert Err values into Just values.
* Add partition function to split a list of Results into lists of Ok values &
  Err values.
* Add CHANGELOG file.
