#' Managing API keys
#'
#' @description
#' It is strongly recommended users of the BLS API use an API key. This key can
#' be stored as environment variable, `BLS_API_KEY`.
#'
#' - `bls_get_key()` will retrieve the key, if set, or it will return `NULL`
#'   if the key has not been set or has been unset.
#'
#' - `bls_set_key()` will set the key _for the current R session_. For
#'   persistence across sessions, set the environment variable.
#'   See the Persistence section for more information.
#'
#' - `bls_unset_key()` will unset the key _for the current R session_.
#'
#' - `bls_has_key()` returns `TRUE` if a key can be found. Otherwise it
#'   returns `FALSE`.
#'
#' @param key A valid BLS API key as a string. keys are typically 32 characters
#'  in length and a key with a different length will trigger a warning.
#'
#' @section Registering for and using an API key:
#'
#'   Registering for an API key is not required to use the BLS API, but it is
#'   recommended you register for an API key and use it. Requests without a key
#'   are limited to 10 years of data per request, 25 series per query, and 25
#'   queries per day. You can register for an API key at:
#'   <https://data.bls.gov/registrationEngine/>
#'
#' @section Persistence:
#'
#'   The preferred method to set the key is to set the `BLS_API_KEY`
#'   environment variable in an `.Renviron` file. The easiest way to do this is
#'   by calling `usethis::edit_r_environ()`. Don't forget to restart R after
#'   setting the key.
#'
#' @name bls-api-key
#'
#' @examples
#'
#' has_key <- bls_has_key()
#'
#' if(has_key){
#'   original_key <- bls_get_key()
#'   bls_unset_key()
#' }
#'
#' #no initial key
#' bls_has_key()
#'
#' # Set a session key
#' bls_set_key("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
#'
#' bls_has_key()
#'
#' # Get session key
#' bls_get_key()
#'
#' # Reset to original key
#' if(has_key) bls_set_key(original_key)
NULL

#' @export
#' @family blsR-utils
#' @rdname bls-api-key
bls_set_key <- function(key) {
  if (!rlang::is_string(key))
    rlang::abort("`key` must be a string.")
  if (nchar(key) != 32)
    rlang::warn("`key` does not have a length of 32 characters.")

  Sys.setenv(BLS_API_KEY = key)
}

#' @export
#' @family blsR-utils
#' @rdname bls-api-key
bls_unset_key <- function() {
  Sys.unsetenv("BLS_API_KEY")
}

#' @export
#' @family blsR-utils
#' @rdname bls-api-key
bls_get_key <- function() {
  key <- Sys.getenv("BLS_API_KEY", unset = NA)
  if(is.na(key)) return(NULL)

  key
}

#' @export
#' @family blsR-utils
#' @rdname bls-api-key
bls_has_key <- function() {
  !is.null(bls_get_key())
}
