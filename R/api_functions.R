#' Set API settings
#'
#' `me_set_api` is used to set the API settings consisting of the API URL, the API key and the verbose option.
#' `me_set_api` is a wrapper for the functions `me_set_api_url`, `me_set_api_key` and `me_set_api_verbose`.
#'
#' @param api_url API base endpoint (URL)
#' @param api_key API key
#' @param verbose Verbose setting for API call (default is FALSE)
#'
#' @examples
#' #me_set_api("http://mynadacatalog.myorganization.org/index.php/api/", "abc123", verbose = TRUE)
me_set_api <- function(api_url, api_key, verbose = FALSE) {
  me_set_api_url(api_url)
  me_set_api_key(api_key)
  me_set_api_verbose(verbose)
}

#' Set API key
#'
#' `me_set_api_key` is used to set the API key in the global environment variables.
#'
#' @param api_key API Key
#'
#' @export
me_set_api_key <- function(api_key) {
  pkg.globals$api_key <- api_key
}

#' Get API key
#'
#' `me_get_api_key` returns the API key from global environment variables.
#'
#' @return API key generated in NADA
#'
#' @export
me_get_api_key <- function()
{
  return(pkg.globals$api_key)
}

#' Set API URL
#'
#' `api_url` is used to set the API base endpoint URL in the global environment variables.
#'
#' @param api_url API base endpoint
#'
#' @export
me_set_api_url <- function(api_url) {
  pkg.globals$api_base_url <- api_url
}

#' Get API URL
#'
#' `me_get_api_key` returns the API base endpoint URL from global environment variables.
#'
#' @param endpoint Endpoint to be added to the base endpoint URL, e.g., ""
#'
#' @return API
#'
#' @export
me_get_api_url <- function(endpoint = NULL)
{
  url = pkg.globals$api_base_url
  
  if(!is.null(endpoint)){
    url = paste0(url,"/",endpoint)
  }
  
  return(url)
}

#' set_verbose
#'
#' Set API calls verbose options
#'
#' @param verbose Verbose output to TRUE or FALSE
#' @export
me_set_api_verbose <- function(verbose=FALSE) {
  pkg.globals$verbose <- verbose
}

#' me_get_verbose
#'
#' `me_get_api_verbose`  returns the API verbose parameter from the global environment variables.
#'
#' @return verbose parameter value
#' @export
me_get_api_verbose <- function()
{
  if (!is.logical(pkg.globals$verbose)){
    return (FALSE)
  }
  
  return (pkg.globals$verbose)
}