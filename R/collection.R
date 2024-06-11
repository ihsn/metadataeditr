#' Get collections list
#'
#' Get collections list
#'
#' @return list
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' list_collections (
#' )
#'
#'
#'
#'
#' @export
list_collections <- function(
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  

  endpoint <- 'collections'
  
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
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
    "response"= metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}





#' Add projects to a collection
#'
#' Add projects to a collection
#'
#' @return NULL
#' @param collections \strong{(required)} List of Collection IDs
#' @param projects \strong{(required)} List of projects
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' collection_add_projects (
#'   collections=c(12,45,100),
#'   projects = c("unique-idno-for-project", "another-project-idno")
#' )
#'
#'
#'
#'
#' @export
collection_add_projects <- function(
    collections=c(),
    projects=c(),
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  options=list(
    collections=collections,
    id_format="idno",
    projects=projects
  )
  
  endpoint <- paste0('collections/add_projects')
  
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
    "response"=fromJSON(content(httpResponse,"text"))
  )
  
  return (output)
}


#' Remove projects from a collection
#'
#' Remove projects from a collection
#'
#' @return NULL
#' @param collection_id (required) Collection ID
#' @param projects \strong{(required)} List of projects
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' collection_remove_projects (
#'   collection_id=123,
#'   projects = c("unique-idno-for-project", "another-project-idno")
#' )
#'
#'
#'
#'
#' @export
collection_remove_projects <- function(
    collection_id,
    projects=c(),
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  options=list(
    collection_id=collection_id,
    id_format="idno",
    projects=projects
  )
  
  endpoint <- paste0('collections/remove_projects')
  
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
    "response"=fromJSON(content(httpResponse,"text")),
    "http_response"=httpResponse
  )
  
  return (output)
}

