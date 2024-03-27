#' Get paginated projects list
#'
#' Get paginated projects list
#'
#' @return NULL
#' @param type (required) Type of project - survey, geospatial, table, document, timeseries
#' @param keywords Search keywords
#' @param filter_type Filter by project type e.g. c("timeseries")
#' @param filter_collection Filter by collections e.g. c(1,2,3)
#' @param limit Limit number of search entries. Default is 100
#' @param offset Starting row number for pagination
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' list_projects (
#'   keywords="some keywords or project IDNO",
#'   filter_type = c("survey","timeseries"),
#'   filter_collection = c(1,2)
#'   metadata = list()
#' )
#'
#'
#'
#'
#' @export
list_projects <- function(
    keywords=NULL,
    filter_type=c(),
    filter_collection=c(),
    limit=100,
    offset=0,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  search_params=NULL
  
  if (!is.null(keywords)){
    search_params=paste0("keywords=",URLencode(keywords))
  }
  
  if (!is.null(filter_type)){
    type_str=paste0("type=",paste(as.character(filter_type), collapse=","))
    if (is.null(search_params)){
      search_params=type_str
    }else{
      search_params=paste(search_params,type_str,sep="&" )
    }
  }
  
  if (!is.null(filter_collection)){
    collection_str=paste0("collection=",paste(as.character(filter_collection), collapse=","))
    if (is.null(search_params)){
      search_params=collection_str
    }else{
      search_params=paste(search_params,collection_str,sep="&" )
    }
  }
  
  #search_params="keywords=test"
  #offset=0
  #limit=10
  
  #set offset and limit
  if (is.null(search_params)){
    search_params=paste0("offset=",offset,"&limit=",limit)
  }else{
    search_params=paste0(search_params,"&offset=",offset,"&limit=",limit)
  }
  
  print (search_params)
  
  endpoint <- paste0('editor/?',search_params)
  
  print(endpoint)
  
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




#' Create new project
#'
#' Create a new project
#'
#' @return NULL
#' @param type (required) Type of project - survey, geospatial, table, document, timeseries
#' @param idno \strong{(required)} Project unique IDNO
#' @param metadata \strong{(required)} Metadata list depending on the type of study
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' create (
#'   type="survey",
#'   idno = "unique-idno-for-project",
#'   metadata = list()
#' )
#'
#'
#'
#'
#' @export
create_project <- function(
    type,
    idno,
    metadata,
    thumbnail=NULL,
    overwrite=FALSE,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  

  # Create url
  endpoint <- paste0('editor/create/',type)
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=metadata,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(get_verbose()))
  
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  thumbnail_result=NULL
  
  #upload thumbnail
  if(!is.null(thumbnail) && file.exists(thumbnail)) {
    thumbnail_result=thumbnail_upload(idno=idno,thumbnail = thumbnail)
  }
  
  #set default thumbnail
  if(!is.null(thumbnail) && thumbnail == 'default'){
    thumbnail_result= thumbnail_delete(idno=idno)
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"= metadataedit_http_response_json(httpResponse),
    "thumbnail"=thumbnail_result
  )
  
  return (output)
}


#' Update project
#'
#' Update project
#'
#' @return NULL
#' @param type (required) Type of project - survey, geospatial, table, document, timeseries
#' @param idno \strong{(required)} Project unique IDNO
#' @param metadata \strong{(required)} Metadata list depending on the type of study
#' @param partial_update Update only partial metadata (TRUE/FALSE)
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' update_project (
#'   type="survey",
#'   idno = "unique-idno-for-project",
#'   metadata = list()
#' )
#'
#'
#'
#'
#' @export
update_project <- function(
    type,
    idno,
    metadata,
    partial_update=FALSE,
    thumbnail=NULL,
    overwrite=FALSE,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  if (partial_update==TRUE){
    metadata$partial_update=TRUE
  }
  
  # Create url
  endpoint <- paste0('editor/update/',type,"/",idno)
  
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=metadata,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(get_verbose()))
  
  output=NULL
  
  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
  
  thumbnail_result=NULL
  
  #upload thumbnail
  if(!is.null(thumbnail) && file.exists(thumbnail)) {
    thumbnail_result=thumbnail_upload(idno=idno,thumbnail = thumbnail)
  }
  
  #set default thumbnail
  if(!is.null(thumbnail) && thumbnail == 'default'){
    thumbnail_result= thumbnail_delete(idno=idno)
  }
  
  output=list(
    "status_code"=httpResponse$status_code,
    "response"= metadataedit_http_response_json(httpResponse),
    "thumbnail"=thumbnail_result
  )
  
  return (output)
}




#' Find a project by IDNO
#'
#' Find a project by IDNO
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#' find_by_idno (
#'   idno="survey-idno-test"
#' )
#'
#' @export
project_by_idno <- function(
    idno,
    api_key=NULL,
    api_base_url=NULL){
  
  if(is.null(api_key)){
    api_key=get_api_key();
  }
  
  endpoint <- paste0('editor/',idno)
  
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }
  
  print(url)
  
  httpResponse <- GET(url, 
                      add_headers("X-API-KEY" = api_key),
                      verbose(get_verbose())
                      )
                      
  
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
