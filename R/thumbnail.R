#' Upload thumbnail for a project
#'
#' Upload thumbnail for a project
#'
#' @return NULL
#' @param idno (required) Project unique identifier
#' @param thumbnail \strong{(required)} Path to the thumbnail file
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#' upload_thumbnail (
#'   idno="project-idno-test",
#'   thumbnail = "/thumbnails/thumbnail-idno-test.png"
#' )
#'
#' @export
thumbnail_upload <- function(
    idno,
    thumbnail,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  options=list(
    file=upload_file(thumbnail)
  )
  
  # Create url
  endpoint <- paste0('files/',idno, '/thumbnail')
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=options, verbose(get_verbose()))
  
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"= metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}

#' Delete thumbnail for a project
#'
#' Delete thumbnail for a project
#'
#' @return NULL
#' @param idno (required) Project unique identifier
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#' delete_thumbnail (
#'   idno="survey-idno-test"
#' )
#'
#' @export
thumbnail_delete <- function(idno,
                                    api_key=NULL,
                                    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  options=list(
    thumbnail=''
  )
  

  # Create url
  endpoint <- paste0('files/delete_thumbnail/',idno)
  
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- POST(url,
                      add_headers("X-API-KEY" = api_key),
                      body=options,
                      content_type_json(),
                      encode="json",
                      accept_json(),
                      verbose(get_verbose()))
  
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"=metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}