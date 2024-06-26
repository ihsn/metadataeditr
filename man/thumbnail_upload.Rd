% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thumbnail.R
\name{thumbnail_upload}
\alias{thumbnail_upload}
\title{Upload thumbnail for a project}
\usage{
thumbnail_upload(idno, thumbnail, api_key = NULL, api_base_url = NULL)
}
\arguments{
\item{idno}{(required) Project unique identifier}

\item{thumbnail}{\strong{(required)} Path to the thumbnail file}

\item{api_key}{API key (optional if API key is set using set_api_key)}

\item{api_base_url}{API base endpoint (optional if API base endpoint is set using set_api_url)}
}
\description{
Upload thumbnail for a project
}
\examples{

upload_thumbnail (
  idno="project-idno-test",
  thumbnail = "/thumbnails/thumbnail-idno-test.png"
)

}
