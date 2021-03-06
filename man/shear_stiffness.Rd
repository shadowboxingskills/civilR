% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{shear_stiffness}
\alias{shear_stiffness}
\title{Calculate the shear stiffness for K-shape lacing}
\usage{
shear_stiffness(n = 2, Ad, Lch, E, h0)
}
\arguments{
\item{n}{Number of planes of lacing, default [\eqn{n=2}]}

\item{Ad}{Section area of diagonal (lacing), [\eqn{cm^2}]}

\item{Lch}{Length of chord of betwen restrains (lace points) [\eqn{m}]}

\item{E}{Young modulus [\eqn{GPa} or \eqn{GN/m^2}]}

\item{h0}{Distance between centroids of chords [\eqn{m}]}
}
\value{
\eqn{S_v} Shear stiffness for K-shape lacing [\eqn{kN}]
}
\description{
Calculate the shear stiffness for K-shape lacing [\eqn{kN}].
The expression of shear stiffness is: \deqn{ S_v = \frac{ n \, E \, A_d \, L_{ch} \, {h_0}^2 }{ d^3 } }
}
