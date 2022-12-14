% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout-packcircles.R
\name{layout_packcircles}
\alias{layout_packcircles}
\title{Packcircle Layout}
\usage{
layout_packcircles(g, degree = NULL, ...)
}
\arguments{
\item{g}{igraph object.}

\item{degree}{if NULL (default), will use \code{igraph::degree()} to
calculate degree.}

\item{...}{other parameters passing to
\code{packcircles::circleProgressiveLayout()} function.}
}
\value{
a two-columns matrix.
}
\description{
arranges nodes by degree.
}
\seealso{
Other layout: 
\code{\link{layout_within_circle}()}
}
\author{
Hou Yun
}
\concept{layout}
