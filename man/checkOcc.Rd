% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkOcc_function.R
\encoding{UTF-8}
\name{checkOcc}
\alias{checkOcc}
\title{checkOcc: check if occurrences fall inside of an specific are}
\usage{
checkOcc (occ, crs, poly, distOcc = NULL, SpOcc = FALSE)
}
\arguments{
\item{occ}{Occurrences records of the species (coordinates in decimal degrees).
'It might correspond to
\itemize{
  \item path for a folder with the species occurrences files (.csv format)
  \item a list of 'data.frames' with the occurrences from multiple species (see data
examples)
  \item 'spOcc' object corresponding to a list of 'SpatialPoints' from
multiple species (see \code{\link[habitaR]{readOcc}} to obtain such object).
}
NOTE: If path is provided, each .csv file should correspond to only one species
and the file must be named with the corresponding species names. The files must
have 3 columns identified as "species" (species names or other identification of taxa),
"long" (longitude), "lat" (latitude). Longitude must be in a column before the
latitude column.}

\item{crs}{The Coordinate Reference System (CRS) specifing the projection and
datum of dataset. Could be a CRS object or a character string.}

\item{distOcc}{A value corresponding to the minimum distance to
consider two coordinates as not duplicate. Values up to this distance will
correspond to duplicates and removed. Units of this value must be in km. Default
is zero (i.e. only exactly coincindent coordinates will be removed).
For more details,see \code{\link[sp:remove.duplicates]{remove.duplicates}}.
Optional and only used if 'occ' is a path for .csv files or a list of 'data.frame'.
If 'occ' correspond to 'SpatialPoints', this argument ('distOcc') should be ignored
(i.e. NULL).}

\item{poly}{A polygon (ESRI shapefile from a 'SpatialPolygonsDataFrame' class)
of the specific area to be checked.}

\item{SpOcc}{(logical) Whether the output should also return the 'SpatialPoints'
of species filtered based on the poly boundaries. Default is \code{FALSE}.}
}
\value{
By default, \code{checkOcc} returns a data.frame of species and its
respective occurrences matching with the provided polygon. If SpOcc is \code{TRUE},
\code{checkdOcc} returns a list with two elements:
\itemize{
  \item A data.frame with the species and total occurrences filtered by the polygon.
  \item A list of 'SpatialPoints' from each species that match with polygon, but
  without clean the records to remove those falling outside the polygon checked.
  See details.
}
}
\description{
From 'SpatialPoints' of multiple species, gives the species and its
occurrences that are inside of a polygon (ESRI shapefile format) representing
a region of interest (e.g. study area, continent, country or other regions with
specific boundaries).
}
\details{
The function filter the initial dataset of species (\emph{occ})
based on a given region (\emph{poly}). NOTE: if \emph{SpOcc} is \code{TRUE}, the
'SpatialPoints' returned encompass all set of occurrences and not a subset
that match with the specified region (\emph{poly}). Therefore, the 'SpatialPoints'
are \emph{NOT} cleaned to remove the records falling outside the polygon checked and to
restrict the 'SpatialPoints' only to occurrences recorded inside of polygon,
but just filtered to reduce the species from initial dataset based in the area
of \emph{poly}.
}
\examples{

### List of 'data.frame' as Input ###

check1 <- checkOcc(occ_plants, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84
 +towgs84=0,0,0", poly=poly, distOcc = NULL, SpOcc = TRUE)

### spOcc or list of 'SpatialPoints' as Input ###

check2 <- checkOcc(spOcc_plants, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84
 +towgs84=0,0,0", poly=poly, distOcc = 0.1, SpOcc = TRUE)

}
\author{
Thaís Dória & Daniel Gonçalves-Souza
}
