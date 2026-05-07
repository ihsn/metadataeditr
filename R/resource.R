#' Editor - list External Resources
#'
#' List external resources for a project
#'
#' @return List of external resources
#' @param idno Project IDNo
#' @export
me_resource_list <- function(idno, api_key=NULL, api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  endpoint=paste0('resources/',idno)
  url=me_get_api_url(endpoint)
  
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(me_get_verbose()))
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"=me_metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}


#' Editor upload external resource file
#'
#' Upload an external resource file
#'
#' @return NULL
#' @param idno Project IDNo
#' @param file External resource file to be uploaded
#' @param resource_type Resource type - documentation, data
#' @export
# resources_upload <- function(
#     idno,
#     resource_id,
#     file,
#     resource_type='documentation',
#     api_key=NULL,
#     api_base_url=NULL){
#   
#   
#   valid_resource_types=c("documentation","data")
#   
#   if (!resource_type %in% valid_resource_types){
#     stop(paste("Supported resource types are:",valid_resource_types))
#   }
#   
#   endpoint=paste0('files/',idno,'/', resource_type)
#   
#   if(is.null(api_key)){
#     api_key=me_get_api_key();
#   }
#   
#   url=me_get_api_url(endpoint)
#   
#   options=list(
#     "file"=upload_file(file)
#   )
#   
#   httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(me_get_verbose()))
#   output=NULL
#   
#   if(httpResponse$status_code!=200){
#     warning(content(httpResponse, "text"))
#   }
#   
#   output=list(
#     "status_code"=httpResponse$status_code,
#     "response"= me_metadataedit_http_response_json(httpResponse)
#   )
#   
#   return (output)
# }



#' Editor create new resource
#'
#' Create a new resource
#'
#' @return NULL
#' @param idno \strong{(required)} Project IDNO
#' @param dctype Resource document type
#' @param title Resource title
#' @param dcformat Resource file format
#' @param author Author name
#' @param dcdate Date using YYYY-MM-DD format
#' @param country Country name
#' @param language Language or Language code
#' @param contributor Contributor name
#' @param publisher Publisher name
#' @param rights Rights
#' @param description Resource detailed description
#' @param abstract  Resource abstract
#' @param toc Table of contents
#' @param file_path File path for uploading
#' @param overwrite Optional logical. When a resource already uses the same stored
#'   filename for this project, pass \code{TRUE} to replace that resource row,
#'   or \code{FALSE} to keep default validation (duplicate filename error).
#'   Omit to omit the field (same as \code{FALSE}). Must be a single non-missing
#'   \code{TRUE} or \code{FALSE} when supplied.
#'
#' @export
me_resource_add <- function(
    idno,
    dctype,
    title,
    subtitle=NULL,
    dcformat=NULL,
    author=NULL,
    dcdate=NULL,
    country=NULL,
    language=NULL,
    contributor=NULL,
    publisher=NULL,
    rights=NULL,
    description=NULL,
    abstract=NULL,
    toc=NULL,
    subjects=NULL,
    file_path=NULL,
    overwrite=NULL,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  options=list(
    idno=idno,
    dctype=dctype,
    dcformat=dcformat,
    title=title,
    subtitle=subtitle,
    author=author,
    dcdate=dcdate,
    country=country,
    language=language,
    contributor=contributor,
    publisher=publisher,
    rights=rights,
    description=description,
    abstract=abstract,
    toc=toc,
    subjects=subjects
  )

  if (!is.null(overwrite)) {
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
      stop("overwrite must be a single TRUE or FALSE", call. = FALSE)
    }
    options$overwrite <- overwrite
  }
  
  if (!is.null(file_path) && file.exists(file_path)){
    options$file=upload_file(file_path)
  }
  else if(!is.null(file_path) && me_is_valid_url(file_path)){
    options[['filename']]=file_path
  }
  
  url=me_get_api_url(paste0('resources/',idno))
  print(url)
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       accept_json(),
                       verbose(me_get_verbose()))
  
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"=me_metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}


#' Editor update resource
#'
#' Update resource
#'
#' @return NULL
#' @param idno \strong{(required)} Project IDNO
#' @param resource_id \strong{(required)} Resource ID
#' @param dctype Resource document type
#' @param title Resource title
#' @param dcformat Resource file format
#' @param author Author name
#' @param dcdate Date using YYYY-MM-DD format
#' @param country Country name
#' @param language Language or Language code
#' @param contributor Contributor name
#' @param publisher Publisher name
#' @param rights Rights
#' @param description Resource detailed description
#' @param abstract  Resource abstract
#' @param toc Table of contents
#' @param file_path File path for uploading
#'
#'
#'
#'
#' @export
me_resource_update <- function(
    idno,
    resource_id,
    dctype=NULL,
    title=NULL,
    subtitle=NULL,
    dcformat=NULL,
    author=NULL,
    dcdate=NULL,
    country=NULL,
    language=NULL,
    contributor=NULL,
    publisher=NULL,
    rights=NULL,
    description=NULL,
    abstract=NULL,
    toc=NULL,
    subjects=NULL,
    file_path=NULL,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  options=list(
    idno=idno,
    dctype=dctype,
    dcformat=dcformat,
    title=title,
    subtitle=subtitle,
    author=author,
    dcdate=dcdate,
    country=country,
    language=language,
    contributor=contributor,
    publisher=publisher,
    rights=rights,
    description=description,
    abstract=abstract,
    toc=toc,
    subjects=subjects
  )
  
  if (!is.null(file_path) && file.exists(file_path)){
    options$file=upload_file(file_path)
  }
  else if(!is.null(file_path) && me_is_valid_url(file_path)){
    options[['filename']]=file_path
  }
  
  url=me_get_api_url(paste0('resources/',idno, "/", resource_id))
  print(url)
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       accept_json(),
                       verbose(me_get_verbose()))
  
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"=me_metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}



#' Editor - delete External Resources
#'
#' Delete external resources for a project
#'
#' @return list
#' @param idno Project IDNo
#' @param resource_id Resource ID
#' @export
me_resource_delete <- function(idno, resource_id, api_key=NULL, api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  endpoint=paste0('resources/delete/',idno,'/',resource_id)
  url=me_get_api_url(endpoint)
  
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(me_get_verbose()))
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"= me_metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}



#' Editor import RDF
#'
#' Import an RDF file
#'
#' @return NULL
#' @param idno Project IDNo
#' @param rdf_file RDF file path
#' @param skip_uploads TRUE/FALSE - If TRUE, won't upload files
#' @param overwrite yes/no - Overwrite existing resources
#' @export
me_resource_import_rdf <- function(
    idno,
    rdf_file,
    api_key=NULL,
    api_base_url=NULL
){
  
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }
  
  url=me_get_api_url(paste0('resources/import/',idno))
  
  options=list(
    "file"=upload_file(rdf_file)
  )
  
  print(url)
  
  
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(me_get_verbose()))
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"= me_metadataedit_http_response_json(httpResponse)
  )
  
  return (output)
}


#' Generate JSON for resources
#'
#' Calls the resources JSON generation endpoint for a project.
#'
#' @param idno Project IDNo
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#' @return list
#' @export
me_resource_write_json <- function(idno, api_key=NULL, api_base_url=NULL){
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }

  endpoint <- paste0("resources/write_json", idno)
  if(is.null(api_base_url)){
    url=me_get_api_url(endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(me_get_verbose()))

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  list(
    "status_code"=httpResponse$status_code,
    "response"= me_metadataedit_http_response_json(httpResponse)
  )
}


#' Generate RDF for resources
#'
#' Calls the resources RDF generation endpoint for a project.
#'
#' @param idno Project IDNo
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#' @return list
#' @export
me_resource_write_rdf <- function(idno, api_key=NULL, api_base_url=NULL){
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }

  endpoint <- paste0("resources/write_rdf", idno)
  if(is.null(api_base_url)){
    url=me_get_api_url(endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(me_get_verbose()))

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  list(
    "status_code"=httpResponse$status_code,
    "response"= me_metadataedit_http_response_json(httpResponse)
  )
}


#' Download RDF for resources
#'
#' Downloads RDF output for a project's resources.
#'
#' @param idno Project IDNo
#' @param as_text Return body as text (TRUE) or raw bytes (FALSE). Default TRUE.
#' @param api_key API key (optional if API key is set using me_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using me_set_api_url)
#' @return list
#' @export
me_resource_rdf <- function(idno, as_text=TRUE, api_key=NULL, api_base_url=NULL){
  if(is.null(api_key)){
    api_key=me_get_api_key();
  }

  endpoint <- paste0("resources/rdf", idno)
  if(is.null(api_base_url)){
    url=me_get_api_url(endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), verbose(me_get_verbose()))

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  body <- if (as_text) content(httpResponse, as = "text", encoding = "UTF-8") else content(httpResponse, as = "raw")

  list(
    "status_code"=httpResponse$status_code,
    "content_type"=headers(httpResponse)[["content-type"]],
    "response"= body
  )
}