#' List collections
#'
#' Alias for \code{\link{me_collection_list}} (same as \code{me_list_projects} naming).
#'
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return Same as \code{\link{me_collection_list}}
#' @export
me_list_collections <- function(api_key=NULL, api_base_url=NULL) {
  me_collection_list(api_key = api_key, api_base_url = api_base_url)
}





#' Add projects to a collection
#'
#' Add projects to a collection
#'
#' @return NULL
#' @param collections \strong{(required)} List of Collection IDs
#' @param projects \strong{(required)} List of projects
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @examples
#'
#'
#' me_collection_add_projects (
#'   collections=c(12,45,100),
#'   projects = c("unique-idno-for-project", "another-project-idno")
#' )
#'
#'
#'
#'
#' @export
me_collection_add_projects <- function(
    collections=c(),
    projects=c(),
    id_format="id",
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  options=list(
    collections=collections,
    id_format=id_format,
    projects=projects
  )
  
  endpoint <- paste0('collections/add_projects')
  
  if(is.null(api_base_url)){
    url=me_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(me_get_verbose()))
  
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
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @examples
#'
#'
#' me_collection_remove_projects (
#'   collection_id=123,
#'   projects = c("unique-idno-for-project", "another-project-idno")
#' )
#'
#'
#'
#'
#' @export
me_collection_remove_projects <- function(
    collections=c(),
    projects=c(),
    id_format="id",
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  options=list(
    collections=collections,
    id_format=id_format,
    projects=projects
  )
  
  endpoint <- paste0('collections/remove_projects')
  
  if(is.null(api_base_url)){
    url=me_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(me_get_verbose()))
  
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


# Internal helper for collection endpoint calls
me_collection_request <- function(method, endpoint, body=NULL, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  if (method == "GET") {
    httpResponse <- GET(url,
                        add_headers("X-API-KEY" = api_key),
                        content_type_json(),
                        encode = "json",
                        accept_json(),
                        verbose(me_get_verbose()))
  } else if (method == "POST") {
    if (is.null(body)) {
      httpResponse <- POST(url,
                           add_headers("X-API-KEY" = api_key),
                           accept_json(),
                           verbose(me_get_verbose()))
    } else {
      httpResponse <- POST(url,
                           add_headers("X-API-KEY" = api_key),
                           body = body,
                           content_type_json(),
                           encode = "json",
                           accept_json(),
                           verbose(me_get_verbose()))
    }
  } else if (method == "DELETE") {
    httpResponse <- DELETE(url,
                           add_headers("X-API-KEY" = api_key),
                           accept_json(),
                           verbose(me_get_verbose()))
  } else {
    stop("Unsupported method: ", method)
  }

  if (httpResponse$status_code >= 400) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' List collections
#'
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_list <- function(api_key=NULL, api_base_url=NULL) {
  me_collection_request("GET", "collections", api_key=api_key, api_base_url=api_base_url)
}


#' Create collection
#'
#' @param collection_data (required) Collection payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_create <- function(collection_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections", body=collection_data, api_key=api_key, api_base_url=api_base_url)
}


#' Update collection
#'
#' @param collection_id (required) Collection ID
#' @param collection_data (required) Collection payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_update <- function(collection_id, collection_data, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("collections/update/", collection_id)
  me_collection_request("POST", endpoint, body=collection_data, api_key=api_key, api_base_url=api_base_url)
}


#' Copy collection
#'
#' @param copy_data (required) Copy payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_copy <- function(copy_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/copy", body=copy_data, api_key=api_key, api_base_url=api_base_url)
}


#' Move collection
#'
#' @param move_data (required) Move payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_move <- function(move_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/move", body=move_data, api_key=api_key, api_base_url=api_base_url)
}


#' Get collection permissions
#'
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_permissions <- function(api_key=NULL, api_base_url=NULL) {
  me_collection_request("GET", "collections/permissions", api_key=api_key, api_base_url=api_base_url)
}


#' Get collection by ID
#'
#' @param collection_id (required) Collection ID
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_get <- function(collection_id, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("collections/", collection_id)
  me_collection_request("GET", endpoint, api_key=api_key, api_base_url=api_base_url)
}


#' Delete collection by ID
#'
#' @param collection_id (required) Collection ID
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_delete <- function(collection_id, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("collections/", collection_id)
  me_collection_request("DELETE", endpoint, api_key=api_key, api_base_url=api_base_url)
}


#' Add projects to collections
#'
#' @param projects_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_projects <- function(projects_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/projects", body=projects_data, api_key=api_key, api_base_url=api_base_url)
}


#' Get project access entries by collection
#'
#' @param collection_id (required) Collection ID
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_project_access_get <- function(collection_id, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("collections/user_project_access/", collection_id)
  me_collection_request("GET", endpoint, api_key=api_key, api_base_url=api_base_url)
}


#' Add user project access
#'
#' @param access_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_project_access_add <- function(access_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/user_project_access", body=access_data, api_key=api_key, api_base_url=api_base_url)
}


#' Remove user project access
#'
#' @param access_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_project_access_remove <- function(access_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/remove_user_project_access", body=access_data, api_key=api_key, api_base_url=api_base_url)
}


#' Create collection template
#'
#' @param template_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_template <- function(template_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/template", body=template_data, api_key=api_key, api_base_url=api_base_url)
}


#' Get collection ACL entries
#'
#' @param collection_id (required) Collection ID
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_acl_get <- function(collection_id, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("collections/user_acl/", collection_id)
  me_collection_request("GET", endpoint, api_key=api_key, api_base_url=api_base_url)
}


#' Add collection ACL entry
#'
#' @param acl_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_acl_add <- function(acl_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/user_acl", body=acl_data, api_key=api_key, api_base_url=api_base_url)
}


#' Update collection ACL entry
#'
#' @param acl_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_acl_update <- function(acl_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/user_acl_update", body=acl_data, api_key=api_key, api_base_url=api_base_url)
}


#' Remove collection ACL entry
#'
#' @param acl_data (required) Payload as a named list
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_acl_remove <- function(acl_data, api_key=NULL, api_base_url=NULL) {
  me_collection_request("POST", "collections/user_acl_remove", body=acl_data, api_key=api_key, api_base_url=api_base_url)
}


#' Check user ACL for collection
#'
#' @param collection_id (required) Collection ID
#' @param user_id (required) User ID
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_user_acl_check <- function(collection_id, user_id, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("collections/user_acl_check/", collection_id, "/", user_id)
  me_collection_request("GET", endpoint, api_key=api_key, api_base_url=api_base_url)
}


#' List collections assigned to a project
#'
#' @param project_id (required) Project ID or IDNO
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_collection_by_project <- function(project_id, api_key=NULL, api_base_url=NULL) {
  endpoint <- paste0("editor/collections/", project_id)
  me_collection_request("GET", endpoint, api_key=api_key, api_base_url=api_base_url)
}

