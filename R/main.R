usethis::use_package("devtools")
usethis::use_package("roxygen2")
usethis::use_package("readxl")

require(devtools)
require(roxygen2)
# usethis::use_vignette("civilR")
# roxygen2::roxygenise()
# devtools::build_manual(path="./doc")
# system("R CMD Rd2pdf .")


#' Calculate the critical length along major \eqn{y} axis
#'
#' Calculate the critical length along major \eqn{y} axis, \eqn{L_{cry}} [\eqn{m}]
#'
#' @param L Total length of member [\eqn{m}]
#' @param Lkp Length to king post [\eqn{m}]
#' @param Lsp Length from splays [\eqn{m}]
#'
#' @export
#'
#' @return \eqn{L_{cry}} Critical length along major \eqn{y} axis [\eqn{m}]
#'
critical_length_major_axis_y <- function (L, Lkp, Lsp) {
  Lkp <- L / 2
  Lcry <- Lkp - Lsp
  return(Lcry)
}


#' Calculate the critical length along major \eqn{z} axis
#'
#' Calculate the critical length along major \eqn{z} axis, \eqn{L_{crz}} [\eqn{m}]
#'
#' @param L Total length of member [\eqn{m}]
#' @param Lkp Length to king post [\eqn{m}]
#' @param Lsp Length from splays [\eqn{m}]
#'
#' @export
#'
#' @return \eqn{L_{crz}} Critical length along major \eqn{z} axis [\eqn{m}]
#'
critical_length_minor_axis_z <- function (L, Lkp, Lsp) {
  Lcrz <- 0 # TBC
  return( Lcrz )
}


#' Calculate the temperature load
#'
#' Calculate Temperature Load as a function of a surface changes of temperature, TL [\eqn{kN}].
#' Usually used for calculation of Axial Compression Force for the top level member. \deqn{TL = \alpha_T \, \delta_T \, k_T  \, E \, A}
#'
#' @param alpha_T Thermal coefficient of expansion [\eqn{degC}]
#' @param delta_T Change in temperature from the Installation temperature [\eqn{degC}]
#' @param k_T Coefficient Of temperature effect [dimensionless]
#' @param E Young's Modulus of Elasticity [\eqn{GPa}]
#' @param A Sectional area from table for given member size [\eqn{cm^2}]
#'
#' @export
#'
#' @return TL Temperature load [\eqn{kN}]
#'
temperature_load <- function(alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, A) {
  TL <- alpha_T * delta_T * k_T * E * A
  return(TL)
}


#' Extract dimensions from reference table
#'
#' Function that looks into the Blue Book \url{https://www.steelforlifebluebook.co.uk/} for dimensions and properties.
#'
#' @param h Member height [\eqn{mm}]
#' @param b Member width [\eqn{mm}]
#' @param m Member mass [\eqn{kg/m}]
#' @param member_type Member type, 'UB' or 'UC'
#'
#' @export
#'
#' @return
#' \itemize{
#'   \item \eqn{A} Area of section [\eqn{cm^2}]
#'   \item \eqn{tw} Thickness of web [\eqn{mm}]
#'   \item \eqn{tf} Thickness of flange [\eqn{mm}]
#'   \item \eqn{Iyy} Second moment of area axis \eqn{y-y} [\eqn{{cm}^4}]
#'   \item \eqn{sh} Depth of section [\eqn{mm}]
#'   \item \eqn{sb} Width of section [\eqn{mm}]
#' }
#'
extract_member_dimensions <- function(h, b, m, member_type) {
  require(readxl)

  if ( member_type == "UB" ) {
    dimensions_table_name <- "./tables/UB-Dimensions_properties.xlsx"
    nmax <- 107
  } else {
    dimensions_table_name <- "./tables/UC-Dimensions_properties.xlsx"
    nmax <- 46
  }

  t <- readxl::read_xlsx( dimensions_table_name,
                          col_types = c("text", "text", "skip", rep("numeric", 27)),
                          skip = 9,
                          n_max = nmax )

  # generate clean h / b / m columns
  t$h <- unlist(strsplit(t$..1, " x "))[c(T, F)]
  t$b <- unlist(strsplit(t$..1, " x "))[c(F, T)]
  t$m <- unlist(strsplit(t$..2, "x "))[c(F, T)]

  # remove 2 first columns
  drops <- c("..1", "..2")
  t <- t[ , !(names(t) %in% drops)]

  cnames <- c("Mass per meter kg/m",	"Depth of section h mm",	"Width of section b mm",	"Thickness web tw mm", "Thickness flange tf mm",		"Root radius r mm",	"Depth between fillets d mm",	"cw/tw ratio for local buckling", "cf/tf ratio for local buckling",
              "End clearance C mm",	"Notch N mm", "Notch n mm",		"Surface area per meter m2",	"Surface area per mT m2",	"Second moment of area Axis y-y cm4",	"Second moment of area Axis z-z cm4",	"Radius of gyration Axis y-y cm",	"Radius of gyration Axis z-z cm",
              "Elastic modulus Axis y-y cm3",	"Elastic modulus Axis z-z cm3",	"Plastic modulus Axis y-y cm3",	"Plastic modulus Axis z-z cm3", "Buckling parameter U",	"Torsional index X",
              "Warping constant Iw dm6",	"Torsional constant IT cm4",	"Area of section A cm2", "h", "b", "m")
  colnames(t) <- cnames

  A <- t$`Area of section A cm2`[(t$h == h) & (t$b == b) & (t$m == m)]
  tw <- t$`Thickness web tw mm`[(t$h == h) & (t$b == b) & (t$m == m)]
  tf <- t$`Thickness flange tf mm`[(t$h == h) & (t$b == b) & (t$m == m)]
  Iyy <- t$`Second moment of area Axis y-y cm4`[(t$h == h) & (t$b == b) & (t$m == m)]
  sh <- t$`Depth of section h mm`[(t$h == h) & (t$b == b) & (t$m == m)]
  sb <- t$`Width of section b mm`[(t$h == h) & (t$b == b) & (t$m == m)]

  l <- list("A" = A, "tw" = tw, "tf" = tf, "Iyy" = Iyy, "sh" = sh, "sb" = sb)

  return(l)
}


#' Convert the member size individual dimensions to a standard string
#'
#' Generate a combined string from given three individual elements, separated by "x".
#'
#' @param h Member height [\eqn{mm}]
#' @param b Member width [\eqn{mm}]
#' @param m Member mass [\eqn{kg/m}]
#'
#' @export
#'
#' @return String of the member dimensions
#'
convert_member_dimensions_to_string <- function(h, b, m) {
  return( paste( h, b, m, sep=' x ' ) )
}


#' Convert individual member dimensions to a string
#'
#' Convert individual member dimensions to a string.
#'
#' @param s String of the member dimensions
#'
#' @export
#'
#' @return
#' \itemize{
#'   \item \eqn{h} Member height [\eqn{mm}]
#'   \item \eqn{b} Member width [\eqn{mm}]
#'   \item \eqn{m} Member weight [\eqn{kg/m}]
#' }
#'
convert_member_dimensions_string_to_elements <- function(s) {
  v <- as.numeric( unlist(strsplit(s, " x ")) )
  h <- v[1]
  b <- v[2]
  m <- v[3]
  return( list("h" = h, "b" = b, "m" = m) )
}


#' Calculate the imperfection factor \eqn{\alpha_{yy}} for rolled section [dimensionless]
#'
#' Calculate the imperfection factor \eqn{\alpha_{yy}} for rolled section [dimensionless].
#'
#' @param h Member height [\eqn{mm}]
#' @param b Member width [\eqn{mm}]
#' @param tf thickness of the flange [\eqn{mm}]
#'
#' @export
#'
#' @return \eqn{\alpha_{yy}} Imperfection factor for \eqn{y-y} axis [dimensionless]
#'
imperfection_factor_yy <- function(h, b, tf) {
  if ( h/b > 1.2 ) {
    if ( tf <= 40 ) {
      alpha = 0.21
    } else {
      alpha = 0.34
    }
  } else {
    if ( tf <= 100 ) {
      alpha = 0.34
    } else {
      alpha = 0.76
    }
  }
  return(alpha)
}


#' Calculate the imperfection factor \eqn{\alpha_{zz}} for rolled section
#'
#' Calculate the imperfection factor \eqn{\alpha_{zz}} for rolled section.
#'
#' @param h Member height [\eqn{mm}]
#' @param b Member width [\eqn{mm}]
#' @param tf thickness of the flange [\eqn{mm}]
#'
#' @export
#'
#' @return \eqn{\alpha_{zz}} Imperfection factor for \eqn{z-z} axis [dimensionless]
#'
imperfection_factor_zz <- function(h, b, tf) {
  if ( h/b > 1.2 ) {
    if ( tf <= 40 ) {
      alpha = 0.34
    } else {
      alpha = 0.49
    }
  } else {
    if ( tf <= 100 ) {
      alpha = 0.49
    } else {
      alpha = 0.76
    }
  }
  return(alpha)
}


#' Calculate the yield strength
#'
#' Calculate the yield strength, \eqn{f_y} [\eqn{N/{mm}^2}]
#'
#' @param tw Thickness of the web [\eqn{mm}]
#' @param tf Thickness of the flange [\eqn{mm}]
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#'
#' @export
#'
#' @examples
#' yield_strength(tw=47.6, tf=77, steel_grade="S355")
#'
#' @return \eqn{f_y} Yield strength [\eqn{N/{mm}^2}]
#'
yield_strength <- function(tw, tf, steel_grade) {
  t <- max( tw, tf ) # t maximum thickness [mm]

  if ( steel_grade == "S275" ) {
    if ( t <= 16 ) {
      fy <- 275
    } else if ( 16 < t & t <= 40	) {
      fy <- 265
    } else if ( 40 < t & t <= 63 ) {
      fy <- 255
    } else if ( 63 < t & t <= 80	) {
      fy <- 245
    } else if ( 80 < t & t <= 100 ) {
      fy <- 235
    } else if ( 100 < t & t <= 150	) {
      fy <- 225
    } else if ( 150 < t & t <= 200 ) {
      fy <- 215
    } else if ( 200 < t & t <= 250	) {
      fy <- 205
    } else {
      fy <- NA
      print("Error. t >= 250")
    }
  } else {
    # steel_grade = "S355"
    if ( t <= 16 ) {
      fy <- 355
    } else if ( 16 < t & t <= 40	) {
      fy <- 345
    } else if ( 40 < t & t <= 63 ) {
      fy <- 335
    } else if ( 63 < t & t <= 80	) {
      fy <- 325
    } else if ( 80 < t & t <= 100 ) {
      fy <- 315
    } else if ( 100 < t & t <= 150	) {
      fy <- 295
    } else if ( 150 < t & t <= 200 ) {
      fy <- 285
    } else if ( 200 < t & t <= 250	) {
      fy <- 275
    } else {
      fy <- NA
      print("Error. t >= 250")
    }
  }

  # mm		S275	S355
  # t ≤ 16		275	355
  # 16 < t ≤ 40		265	345
  # 40 < t ≤ 63		255	335
  # 63 < t < 80		245	325
  # 80 < t < 100		235	315
  # 100 < t < 150		225	295
  # 150 < t < 200		215	285
  # 200 < t < 250		205	275

  return(fy)
}


#' Calculate the effective length of member
#'
#' Calculate the effective length of member, \eqn{L_e} [\eqn{mm}].
#'
#' @param k Effective lengh coefficient [dimensionless]
#' @param L Length of strut between restraints [\eqn{mm}]
#'
#' @export
#'
#' @return \eqn{L_e} Effective length of strut [\eqn{mm}]
#'
effective_length_of_member <- function(k, L) {
  return( k * L )
}


#' Calculate the effective second moment of area
#'
#' Compute the effective second moment of area [\eqn{{mm}^4}].
#' \eqn{I_{eff}} is a function of the distance between the centroids of the chords and the section area of a chord, calculated as \eqn{ I_{eff} = 0.5 \, {h_0}^2 \, A }.
#'
#' @param h0 Distance between centroids of chords [\eqn{m}]
#' @param A Cross-section area of strut [\eqn{{cm}^2}]
#'
#' @export
#'
#' @return \eqn{I_{eff}} Effective second moment of area [\eqn{{mm}^4}]
#'
effective_second_moment_of_area <- function(h0, A) {
  return( 0.5 * h0^2 * A * 100 )
}


#' Calculate the plastic resistance of the cross-section to compression
#'
#' Calculate the plastic resistance of the cross-section to compression [\eqn{kN}], based on cross-section area \eqn{A} and yield strength \eqn{f_y}.
#'
#' @param A Cross-section area of the strut [\eqn{{cm}^2}]
#' @param fy Yield strength [\eqn{kN/{mm}^2}]
#'
#' @export
#'
#' @return \eqn{N_{pl,R_d}} Plastic resistance of the cross-section to compression [\eqn{kN}]
#'
plastic_resistance_of_cross_section_to_compression <- function(A, fy) {
  return( fy * A * 100 )
}


#' Calculate the Euler buckling load
#'
#' Calculate the Euler buckling load [\eqn{kN}] \deqn{ N_{cr,ch}=\frac{\pi^2 \, E \, I}{{L_e}^2} }
#'
#' @param Le Effective length of strut [\eqn{mm}]
#' @param E Young modulus [\eqn{GPa} or \eqn{GN/m^2}]
#' @param I - check 1: \eqn{I_{yy}}, second moment of area Axis \eqn{y-y} [\eqn{{cm}^4}]. Check 2: \eqn{I_{eff}}, Effective second moment of area [\eqn{{mm}^4}]. Check 3: \eqn{I_{eff}} or \eqn{I_{zz}} [\eqn{{mm}^4}]
#'
#' @export
#'
#' @return \eqn{N_{cr}} Euler buckling load [\eqn{kN}]
#'
Euler_buckling_load <- function(Le, E, I) {
  Ncr <- ( pi^2 * E * I ) / Le^2
  return( Ncr )
}


#' Calculate the relative slenderness
#'
#' Calculate the relative slenderness [dimensionless] \deqn{ \bar{\lambda}=\sqrt{ \frac{N_{pl,R_d}}{N_{cr}} } }
#'
#' @param N_pl_Rd Plastic resistance of the cross-section to compression [\eqn{kN}]
#' @param Ncr Euler buckling load [\eqn{kN}]
#'
#' @export
#'
#' @return \eqn{\bar{\lambda}} Relative slenderness [dimentionless]
#'
relative_slenderness <- function(N_pl_Rd, Ncr) {
  return( sqrt( N_pl_Rd / Ncr ) )
}


#' Calculate the slenderness reduction factor
#'
#' Calculate the slenderness reduction factor \eqn{X} [dimentionless] for the general case.
#' \deqn{ \Phi = 0.5 \left[ 1 + \alpha \left( \bar{\lambda}-0.2 \right) + {\bar{\lambda}}^2 \right] }
#' \deqn{ X = \frac{1}{\Phi + \sqrt{ \Phi^2 - {\bar{\lambda}}^2} } }
#'
#' @param alpha Check #1: imperfection factor \eqn{\alpha_{yy}} for rolled section [dimentionless]. Check 2 & 3: imperfection factor \eqn{\alpha_{zz}} for rolled section [dimentionless]
#' @param lambda_bar, Relative slenderness \eqn{\bar{\lambda}} [dimentionless]
#'
#' @export
#'
#' @return \eqn{X} Slenderness reduction factor [dimentionless]
#'
slenderness_reduction_factor <- function(alpha, lambda_bar) {
  Phi <- 0.5 * ( 1 + alpha * (lambda_bar - 0.2) + lambda_bar^2 )
  X <- 1 / ( Phi + sqrt( Phi^2 - lambda_bar^2 )  )
  return(X)
}


#' Calculate the overall buckling resistance of the memeber about the axis
#'
#' General case to compute the overall buckling resistance of the member, \eqn{N_{b,R_d}} [\eqn{kN}], about the axis, calculated as: \deqn{ N_{b,R_d}=X \, N_{pl,R_d} }
#'
#' @param X Slenderness reduction factor [dimentionless]
#' @param N_pl_Rd Plastic resistance of the cross-section to compression [\eqn{kN}]
#'
#' @export
#'
#' @return \eqn{N_{b,R_d}} Overall buckling resistance of the struts about the axis [\eqn{kN}]
#'
overall_buckling_resistance_about_axis <- function(X, N_pl_Rd) {
  return( X * N_pl_Rd )
}


#' Calculate the shear stiffness for K-shape lacing
#'
#' Calculate the shear stiffness for K-shape lacing [\eqn{kN}].
#' The expression of shear stiffness is: \deqn{ S_v = \frac{ n \, E \, A_d \, L_{ch} \, {h_0}^2 }{ d^3 } }
#'
#' @param n Number of planes of lacing, default [\eqn{n=2}]
#' @param Ad Section area of diagonal (lacing), [\eqn{cm^2}]
#' @param Lch Length of chord of betwen restrains (lace points) [\eqn{m}]
#' @param E Young modulus [\eqn{GPa} or \eqn{GN/m^2}]
#' @param h0 Distance between centroids of chords [\eqn{m}]
#'
#' @export
#'
#' @return \eqn{S_v} Shear stiffness for K-shape lacing [\eqn{kN}]
#'
shear_stiffness <- function(n=2, Ad, Lch, E, h0) {
  d <- sqrt( h0^2 + Lch^2 )  # length of the diagonal
  Sv <- ( n * E * Ad * Lch * h0^2 ) / d^3
  return(Sv)
}


#' Calculate the second order bending moment
#'
#' Compute the second order bending moment, \eqn{M_{E_d}} [\eqn{kN.m}].
#' The maximum bending moment, including the bow imperfection and the second order effects, calculated as:
#' \deqn{ M_{E_d} = \frac{ N_{E_d} \, e_0 \, + \, {M_{E_d}}^I }{ 1 - \frac{N_{E_d}}{N_{cr,Y}} - \frac{N_{E_d}}{S_v} } }
#'
#' @param L Length of strut between restraints [\eqn{mm}]
#' @param Ned axial_compression_force [\eqn{kN}]
#' @param Sv Shear stiffness for K-shape lacing [\eqn{kN}]
#' @param Ncr Euler buckling load from check #2 global zz [\eqn{kN}]
#'
#' @export
#'
#' @return \eqn{M_{E_d}} Second order moment [\eqn{kN.m}]
#'
second_order_bending_moment <- function(L, Ned, Sv, Ncr) {
  e0 <- L / 500 # e0, initial bow imperfection [mm]
  e0 <- min(e0, 30) # cap e0 at 30mm
  MEd_1 <- 0 # first order moment
  MEd <- ( Ned * e0 + MEd_1 ) / ( 1 - (Ned / Ncr) - (Ned / Sv))
  return(MEd)
}


#' Generate calculated NEd
#'
#' Generate calculated \eqn{N_{E_d}}, \eqn{N_{{E_d}_c}} [\eqn{kN}].
#'
#' @param N_b_Rd Overall buckling resistance of the struts about the axis [\eqn{kN}]
#' @param Ieff Effective second moment of area [\eqn{{mm}^4}]
#' @param MEd Second order moment [\eqn{kN.m}]
#' @param h0 Distance between centroids of chords [\eqn{m}]
#' @param A Cross-section area of strut [\eqn{{cm}^2}]
#'
#' @export
#'
#' @return \eqn{N_{{E_d}_c}} Calculated \eqn{N_{E_d}} [\eqn{kN}]
#'
calculated_NEd <- function(N_b_Rd, Ieff, MEd, h0, A) {
  return( 2 * ( N_b_Rd - (MEd*h0*A)/(2*Ieff) ) )
}


#' Calculate the maximum shear force in the lacing
#'
#' Calculate the maximum shear force in the lacing, \eqn{V_{E_d}} [\eqn{kN}] (for a laced strut subject to a compressive axial force only)
#' \deqn{ V_{E_d}= \pi \, \frac{M_{E_d}}{L} }
#'
#' @param MEd Second order moment [\eqn{kN.m}]
#' @param L Length of strut between restraints [\eqn{m}]
#'
#' @export
#'
#' @return \eqn{V_{E_d}} Maximum shear force in the lacing [\eqn{kN}] (for a laced strut subject to a compressive axial force only)
#'
maximum_shear_force_in_the_lacing <- function(MEd, L) {
  return( pi * MEd / L )
}


#' Calculate the axial compression force
#'
#' Compute Axial Compression Force, \eqn{N_{ed}} [\eqn{kN}], for member without including Temperature effect.
#' Used as trial for the top level strut where temperature changes could not be neglected.
#' As well can be used to calculate final \eqn{N_{ed}} for struts from low levels of excavation, where temperature effect could be neglected.
#'
#' First of all function check which combination govern in ULS (Ultimate Limit State) without including Temperature load, TL [\eqn{kN}].
#' Then include TL calculations for Load Combinations applying partial factors based on the Table A1.2(B), EN1990-2002, p53
#' Compare maximum from ULS and ALS to define which mistake could govern.
#'
#' @param DL Dead load / self-weight of member [\eqn{kN/m}]
#' @param LL Live load / imposed load [\eqn{kN/m}]
#' @param L Total length of member [\eqn{m}]
#' @param AF Axial compression force of member per meter [\eqn{kN/m}]
#' @param theta Angle to wall [\eqn{deg}]
#' @param spacing spacing [\eqn{m}]
#' @param Lcry critical length major axis [\eqn{m}]
#' @param Lcrz critical length minor axis [\eqn{m}]
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#' @param member_type member_type, categorical: 'UC' or 'UB'
#' @param alpha_T Thermal coef. of expansion [\eqn{degC}]
#' @param delta_T Change in temperature from the Installation temperature [\eqn{degC}]
#' @param k_T Coefficient Of Temperature Effect [dimensionless]
#' @param E Young's Modulus of Elasticity [\eqn{GPa}]
#' @param IL Accidental Impact Load [\eqn{kN/m}]
#'
#' @export
#'
#' @return \eqn{N_{ed}} Axial compression force [\eqn{kN}]
#'
axial_compression_force <- function(
  isTopLevel=T,
  DL=1,
  LL=1,
  L,
  AF,
  theta=90,
  spacing=6,
  Lcry=12.7,
  Lcrz=12.7,
  steel_grade='S355',
  member_type='UB',
  alpha_T=0.000012,
  delta_T=10,
  k_T=0.8,
  E=210,
  IL=50
) {
  # axial_compression_force( DL=1, LL=1, L=12.7, AF=582, theta=90, spacing=7,
  # Lcry=12.7, Lcrz=12.7, steel_grade='S355', member_type='UB', alpha_T=0.000012,
  # delta_T=10, k_T=0.8, E=210, IL=50 )

  # DL=1; LL=1; L=12.7; AF=582; theta=90; spacing=7
  # Lcry=12.7; Lcrz=12.7; steel_grade='S355'; member_type='UB'
  # alpha_T=0.000012; delta_T=10; k_T=0.8; E=210; IL=50

  # SF, axial force / strut force (kN/m)
  # SF <- f(AF, theta)
  SF <- ( AF * spacing ) / sin( theta * pi / 180 )

  if ( isTopLevel ) { # top level only
    # ULS - load combination for strut design
    lc1 <- 1.35 * DL + 1.35 * SF + 1.5 * LL
    lc2 <- 1.35 * DL + 1.35 * SF + 1.05 *LL
    lc3 <- 1.35 * DL + 1.0 * SF + 1.5 * LL
    lc4 <- 1.35 * DL + 1.0 * SF + 1.05 * LL

    # ALS
    lc5 <- 1.0 * DL + 1.0 * SF + 0.7 * LL + 1.0 * IL
    lc6 <- 1.0 * DL + 1.0 * SF + 0.6 * LL + 1.0 * IL
    # lc7 <- 1.0 * DL + 1.0 * SF + 0.7* LL
    # lc8 <- 1.0 * DL + 1.0 * SF + 0.6 * LL

    Ned_no_TL <- max( lc1, lc2, lc3, lc4, lc5, lc6 )

    # find trial member size
    mb_no_TL <- trial_member_size(Lcry, Lcrz, Ned_no_TL, steel_grade, member_type)

    # extract member area from reference table
    l <- convert_member_dimensions_string_to_elements(mb_no_TL)
    A_no_TL <- extract_member_dimensions(l$h, l$b , l$m, member_type)$A

    # calculate temperature load
    TL <- temperature_load(alpha_T, delta_T, k_T, E, A_no_TL)

    # ULS - load combination for strut design with TL
    lc1 <- 1.35 * DL + 1.35 * SF + 1.5 * LL + 0.9 * TL
    lc2 <- 1.35 * DL + 1.35 * SF + 1.05 *LL + 1.5 * TL
    lc3 <- 1.35 * DL + 1.0 * SF + 1.5 * LL + 0.9 * TL
    lc4 <- 1.35 * DL + 1.0 * SF + 1.05 * LL + 1.5 * TL

    # ALS - load combination for strut design with TL
    lc5 <- 1.0 * DL + 1.0 * SF + 0.7 * LL + 1.0 * IL
    lc6 <- 1.0 * DL + 1.0 * SF + 0.6 * LL + 0.5 * TL + 1.0 * IL
    # lc7 <- 1.0 * DL + 1.0 * SF + 0.7* LL
    # lc8 <- 1.0 * DL + 1.0 * SF + 0.6 * LL + 0.5 * TL

    # OFS

    # axial_compression_force (ULS) Ned_ULS (kN)
    Ned_TL <- round( max( lc1, lc2, lc3, lc4, lc5, lc6 ) )

  } else { # all levels except top one

    # ULS - load combination for strut design without TL
    lc1 <- 1.35 * DL + 1.35 * SF + 1.5 * LL
    lc2 <- 1.35 * DL + 1.35 * SF + 1.05 *LL
    lc3 <- 1.35 * DL + 1.0 * SF + 1.5 * LL
    lc4 <- 1.35 * DL + 1.0 * SF + 1.05 * LL

    # ALS without TL
    lc5 <- 1.0 * DL + 1.0 * SF + 0.7 * LL + 1.0 * IL
    lc6 <- 1.0 * DL + 1.0 * SF + 0.6 * LL + 1.0 * IL
    # lc7 <- 1.0 * DL + 1.0 * SF + 0.7* LL
    # lc8 <- 1.0 * DL + 1.0 * SF + 0.6 * LL

    # OFS

    Ned_no_TL <- round( max( lc1, lc2, lc3, lc4, lc5, lc6 ) )
  }

  if ( isTopLevel ) {
    return(Ned_TL)
  } else {
    return(Ned_no_TL)
  }
}


#' Determine member size
#'
#' Find optimized designation [ height (mm) x width (mm) x mass (kg/m) ] (also called member size) for given Axial Compression Force and critical length for major and minor axis.
#' Searching into the tables based on the 'Compression' tables of the Blue Book \url{https://www.steelforlifebluebook.co.uk/}
#'
#' @param Lcry critical length major axis [\eqn{m}]
#' @param Lcrz critical length minor axis [\eqn{m}]
#' @param Ned Axial compression force [\eqn{kN}]
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#' @param member_type member_type, categorical: 'UC' or 'UB'
#'
#' @export
#'
#' @return Member size [ height (mm) x width (mm) x mass (kg/m) ]
#'
trial_member_size <- function(Lcry, Lcrz, Ned, steel_grade, member_type) {
  require(readxl)

  # Lcry=13.2; Lcrz=1.9; Ned=9250; steel_grade="S275"; member_type="UC"

  if ( steel_grade == "S355" ) {
    if ( member_type == "UC" ) {
      axial_compression_table_name <- "./tables/s355/UC/UC-compression-S355.xlsx"
      nmax <- 138
    } else if ( member_type == "UB" ) {
      axial_compression_table_name <- "./tables/s355/UB/UB-Axial compression-S355.xlsx"
      nmax <- 321
    } else {
      print("member type unknown. please enter valid one.")
    }
  } else if ( steel_grade == "S275" ) {
    if ( member_type == "UC" ) {
      axial_compression_table_name <- "./tables/s275/UC/UC-compression-S275.xlsx"
      nmax <- 138
    } else if ( member_type == "UB" ) {
      axial_compression_table_name <- "./tables/s275/UB/UB_Axial compression-S275.xlsx"
      nmax <- 321
    } else {
      print("member type unknown. please enter valid one.")
    }
  } else {
    print("steel grade unknown. please enter valid one.")
  }

  #axial_compression_table_name <- "./s355/UB/UB-Axial compression-S355.xlsx"
  t <- readxl::read_xlsx( axial_compression_table_name,
                          n_max = nmax,
                          skip = 5 )

  # delete all "Nb,T,Rd" rows
  t <- subset(t, Axis != "Nb,T,Rd")

  # duplicate h x b x m labels
  t$..1[is.na(t$..1)] <- t$..1[!is.na(t$..1)]
  t$..2[is.na(t$..2)] <- t$..2[!is.na(t$..2)]

  # generate clean h / b / m columns
  t$h <- unlist(strsplit(t$..1, " x "))[c(T, F)]
  t$b <- unlist(strsplit(t$..1, " x "))[c(F, T)]
  t$m <- unlist(strsplit(t$..2, "x "))[c(F, T)]

  # remove 3 first columns
  drops <- c("..1", "..2", "..3")
  t <- t[ , !(names(t) %in% drops)]

  # function to select closest critical length
  closest_critical_length_index <- function(Lcr, member_type) {
    col_discrete_values <- colnames(t)
    drops <- c("Axis", "h", "b", "m")
    critical_length_vector <- as.numeric( col_discrete_values[!(col_discrete_values %in% drops)] )

    return( which.min(abs(critical_length_vector-Lcr)) + 1 )
  }

  critical_length_Lcry_colname <- closest_critical_length_index(Lcry, member_type)
  # critical_length_Lcrz_colname <- closest_critical_length_index(Lcrz, member_type)

  # critical_length_Lcry_colname <- as.character(format(critical_length_Lcry, nsmall = 1))
  # critical_length_Lcrz_colname <- as.character(format(critical_length_Lcrz, nsmall = 1))

  # Lcry
  t_Nb_y <- subset( t, (Axis == "Nb,y,Rd") )
  x_Nb_y <- t_Nb_y[, critical_length_Lcry_colname]

  colnames(x_Nb_y) <- "Nb_y"
  x_Nb_y <- as.numeric( x_Nb_y$"Nb_y" )

  i <- which(abs(x_Nb_y-Ned)==min(abs(x_Nb_y-Ned)))
  max(x_Nb_y[i])

  # height h(mm) x width b(mm) x mass m(kg/m) for Lcry
  h_Lcry <- as.numeric( t_Nb_y[i, "h"] )
  b_Lcry <- as.numeric( t_Nb_y[i, "b"] )
  m_Lcry <- as.numeric( t_Nb_y[i, "m"] )

  trial_member_size_Lcry <- paste( h_Lcry, b_Lcry, m_Lcry, sep=' x ' )

  # # Lcrz
  # t_Nb_z <- subset( t, (Axis == "Nb,z,Rd") )
  # x_Nb_z <- t_Nb_z[, critical_length_Lcrz_colname]
  #
  # colnames(x_Nb_z) <- "Nb_z"
  # x_Nb_z <- as.numeric( x_Nb_z$"Nb_z" )
  #
  # i <- which(abs(x_Nb_z-Ned)==min(abs(x_Nb_z-Ned)))
  # max(x_Nb_z[i])
  #
  # # height h(mm) x width b(mm) x mass m(kg/m) for Lcry
  # h_Lcrz <- as.numeric( t_Nb_z[i, "h"] )
  # b_Lcrz <- as.numeric( t_Nb_z[i, "b"] )
  # m_Lcrz <- as.numeric( t_Nb_z[i, "m"] )
  #
  # trial_member_size_Lcrz <- paste( h_Lcrz, b_Lcrz, m_Lcrz, sep=' x ' )

  trial_member_size <- trial_member_size_Lcry
  return(trial_member_size)
}


#' Perform check #1, calculating the overall buckling resistance of member about major \eqn{y-y} axis
#'
#' Calculate the overall buckling resistance of member about \eqn{y-y} axis, based on EC3 Approach. \deqn{L_e=kL} [\eqn{mm}]
#' where \eqn{L} is the critical length for buckling about major axis \eqn{y-y}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [\eqn{kN}] \deqn{N_{pl,R_d}= 2(A \, fy)}
#'   \item The Euler buckling load [\eqn{kN}] \deqn{N_{cr,X}=\frac{\pi^2 \, E \, I}{{L_e}^2}}
#'   \item Relative slenderness [dimensionless] \deqn{ \bar{\lambda_X} = \sqrt{ \frac{N_{pl,R_d}}{N_{cr,X}} } }
#'   \item Calculate \eqn{\Phi_X} parameter for slenderness reduction factor \deqn{ \Phi_X = 0.5 \left[ 1 + \alpha \left( \bar{\lambda_X}-0.2 \right) + {\bar{\lambda_X}}^2 \right] }
#'   \item Slenderness reduction factor [dimensionless] \deqn{ X_x = \frac{1}{ \Phi_X+\sqrt{{\Phi_X}^2-{\bar{\lambda_X}}^2} } }
#'   \item Output overall buckling resistance of the struts about \eqn{y-y} axis [\eqn{kN}] \deqn{ N_{b,R_d,X}=X_X \, N_{pl,R_d} }
#' }
#' The partial factors \eqn{\gamma_M} that are applied to resistance of members to instability:	\eqn{\gamma_{M_1} = 1}
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type, categorical: 'UC' or 'UB'
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#' @param k Coefficient [dimensionless]
#' @param L Total length of member [\eqn{m}]
#' @param E Young's Modulus of Elasticity [\eqn{GPa}]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,X}} Overall buckling resistance of struts about major y-y axis [\eqn{kN}]
#'
check_overall_buckling_resistance_about_yy_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {

  # trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=1; L=12.7; E=210

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- extract_member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- yield_strength(l$tw, l$tf, steel_grade)

  # Le, effective length of strut (mm)
  Le <- effective_length_of_member(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, l$Iyy) / 1000

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd * 2, Ncr)

  # imperfection_factor_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_yy(s$h, s$b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_X <- round( overall_buckling_resistance_about_axis(X, N_pl_Rd) )

  return(N_b_Rd_X)
}


#' Perform check #2, calculating the overall buckling resistance of struts about major \eqn{z-z} axis
#'
#' Calculate the overall buckling resistance of member about \eqn{z-z} axis, based on EC3 Approach. \deqn{L_e = k \, L} [\eqn{mm}]
#' where \eqn{L} is the critical length for buckling about major axis \eqn{z-z}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [\eqn{kN}] \deqn{N_{pl,R_d}= 2(A \, fy)}
#'   \item The Euler buckling load [\eqn{kN}] \deqn{N_{cr,Y}=\frac{\pi^2 \, E \, I}{{L_e}^2}}
#'   \item Relative slenderness [dimensionless] \deqn{ \bar{\lambda_Y} = \sqrt{ \frac{N_{pl,R_d}}{N_{cr,Y}} } }
#'   \item Calculate \eqn{\Phi_Y} parameter for slenderness reduction factor \deqn{ \Phi_Y = 0.5 \left[ 1 + \alpha \left( \bar{\lambda_Y}-0.2 \right) + {\bar{\lambda_Y}}^2 \right] }
#'   \item Slenderness reduction factor [dimensionless] \deqn{ X_Y = \frac{1}{ \Phi_Y+\sqrt{{\Phi_Y}^2-{\bar{\lambda_Y}}^2} } }
#'   \item Output overall buckling resistance of the struts about \eqn{z-z} axis [\eqn{kN}] \deqn{ N_{b,R_d,Y}=X_Y \, N_{pl,R_d} }
#' }
#' The partial factors \eqn{\gamma_M} that are applied to resistance of members to instability:	\eqn{\gamma_{M_1} = 1}
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type, categorical: 'UC' or 'UB'
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#' @param k Coefficient [dimensionless]
#' @param L Total length of member [\eqn{m}]
#' @param E Young's Modulus of Elasticity [\eqn{GPa}]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,Y}} Overall buckling resistance of struts about z-z axis [\eqn{kN}]
#'
check_overall_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {
  # 2: \alpha_zz, I=Ieff [mm4], Le=kL[\eqn{m}], Npl,Rk=Npl,Rd*2 [\eqn{kN}]
  # \lambda= \lambda_Y,  X=X_y, \Phi=\Phi_y
  #
  # OUTPUT 2 : N_{b,Rd}=N_{b,Rd,y} [\eqn{kN}]

  # trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- extract_member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- yield_strength(l$tw, l$tf, steel_grade)

  # Le, effective length of strut (mm)
  Le <- effective_length_of_member(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, l$Iyy) / 1000

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd * 2, Ncr)

  # imperfection_factor_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_yy(s$h, s$b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_Y <- overall_buckling_resistance_about_axis(X, N_pl_Rd)

  return(N_b_Rd_Y)
}


#' Perform check #3, calculating the local buckling resistance of struts about minor \eqn{z-z} axis
#'
#' Calculate the local buckling resistance of member about minor \eqn{z-z} axis, based on EC3 Approach. \deqn{L_e=kL} [\eqn{mm}]
#' where \eqn{L} is the critical length for buckling about minor axis \eqn{z-z}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [\eqn{kN}] \deqn{N_{pl,R_d,ch}= 2(A  \, fy)}
#'   \item The Euler buckling load [\eqn{kN}] \deqn{N_{cr,ch}=\frac{\pi^2 \,  E \,  I}{{L_e}^2}}
#'   \item Relative slenderness [dimensionless] \deqn{ \bar{\lambda_{ch}} = \sqrt{ \frac{N_{pl,R_d,ch}}{N_{cr,ch}} } }
#'   \item Calculate \eqn{\Phi_{ch}} parameter for slenderness reduction factor \deqn{ \Phi_{ch} = 0.5   \left[ 1 + \alpha \left( \bar{\lambda_{ch}}-0.2 \right) + {\bar{\lambda_{ch}}}^2 \right] }
#'   \item Slenderness reduction factor [dimensionless] \deqn{ X_{ch} = \frac{1}{ \Phi_{ch}+\sqrt{{\Phi_{ch}}^2-{\bar{\lambda_{ch}}}^2} } }
#'   \item Output overall buckling resistance of the struts about \eqn{z-z} minor axis [\eqn{kN}] \deqn{ N_{b,R_d,ch}=X_{ch} \, N_{pl,R_d,ch} }
#' }
#' The partial factors \eqn{\gamma_M} that are applied to resistance of members to instability:	\eqn{\gamma_{M_1} = 1}
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type, categorical: 'UC' or 'UB'
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#' @param k Coefficient [dimensionless]
#' @param L Total length of member [\eqn{m}]
#' @param E Young's Modulus of Elasticity [\eqn{GPa}]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,X}} Local buckling resistance of struts about \eqn{z-z} axis [\eqn{kN}]
#'
check_local_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {
  # 3: \alpha_zz, I=Ieff[mm4], Le=Lch [\eqn{m}], Npl,Rch=Npl,Rd [\eqn{kN}]
  #
  # OUTPUT 3 :
  #   take min OUTPUT 1, OUTPUT 2, OUTPUT 3

  # trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- extract_member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- yield_strength(l$tw, l$tf, steel_grade)

  # Le, effective length of strut (mm)
  Le <- effective_length_of_member(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, l$Iyy) / 1000

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd * 2, Ncr)

  # imperfection_factor_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_yy(s$h, s$b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_ch <- overall_buckling_resistance_about_axis(X, N_pl_Rd)

  return(N_b_Rd_ch)
}


#' Maximum compressive axial force in the chords
#'
#' Determine maximum compressive axial force in the chords at mid-length of the strut, \eqn{N_{ch,E_d}} [\eqn{kN}]
#'
#' Calculation steps are as follows:
#' \enumerate{
#'   \item Effective length \eqn{L_e = k \, L} [\eqn{mm}], where \eqn{L} is the length between two vertical supports of laced struts.
#'   \item Effective second moment of area \deqn{I_{eff}=0.5 \, h_0 \, A_{ch}}  [\eqn{{mm}^4}]. \eqn{I_{eff}}, where \eqn{h_0} [\eqn{cm}] is the distance between centroids of the chords and \eqn{A_{ch}}, [\eqn{{cm}^4}] is the cross-sectional area of one chord.
#'   \item Shear stiffness for K-shape lacing \eqn{S_v} [\eqn{kN}], where \eqn{d} is the lenth of diagonals \eqn{d = sqrt{ {h_0}^2 + L_{ch} }} [\eqn{mm}].
#'   \item Calculate the Euler buckling load \eqn{N_{cr,ch}} [\eqn{kN}] \deqn{ N_{cr,ch}=\frac{\pi^2 \, E \, I} {{L_e}^2} }, where \eqn{L_e} is the effective length between two vertical supports.
#'   \item Compute the second order bending moment \eqn{{M_{E_d}}^{II}} [\eqn{kN.m}].
#'   \item Output maximum compressive axial force in the chords \eqn{N_{ch,E_d}} [\eqn{kN}] \deqn{ N_{ch,E_d} = \frac{N_{E_d}}{2} + \frac{M_{E_d} \, h_0 \, A}{ 2 \, I_{eff}} }
#' }
#'
#' @param k Coefficient of length as function of wall rigidity [dimensionless]
#' @param L Length between two restrains [\eqn{m}]
#' @param n Number of lacing planes, default [\eqn{n=2}]
#' @param Ad Section area of diagonal (lacing), [\eqn{cm^2}]
#' @param Lch Length of chord of betwen restrains (lace points) [\eqn{m}]
#' @param E Young modulus [\eqn{GPa} or \eqn{GN/m^2}]
#' @param h0 Distance between centroids of chords [\eqn{m}]
#' @param Ned Axial compression Force [\eqn{kN}]
#' @param Ieff Effective second moment of area [\eqn{{mm}^4}]
#' @param Sv Shear stiffness for K-shape lacing [\eqn{kN}]
#'
#' @export
#'
#' @return \eqn{N_{ch,E_d}} Maximum compressive axial force in the chords [\eqn{kN}]
#'
max_compressive_axial_force_in_chords <- function(k, L, A, n, Ad, Lch, E, h0, Ned) {

  # Le, effective length of strut (mm)
  Le <- effective_length_of_member(k, L)

  # Ieff, effective second moment of area
  Ieff <- effective_second_moment_of_area(h0, A)

  # Shear stiffness for K-shape lacing
  Sv <- shear_stiffness(n, Ad, Lch, E, h0)

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, Ieff) / 1000

  # The second order bending moment
  MEd <- second_order_bending_moment(L, Ned, Sv, Ncr)

  # Output maximum compressive axial force in the chords
  N_ch_Ed <- round( (0.5 * Ned) + (MEd * h0 * A) / (2*Ieff) )

  return(N_ch_Ed)
}


#' Run the high-level main flow
#'
#' Run the high-level main flow.
#'
#' @export
#'
#' @return Final results
#'
main <- function() {
  # user inputs
  DL <- 1
  LL <- 1
  L <- 12.7
  AF <- 582
  theta <- 90
  spacing <- 7
  Lcry <- 12.7
  Lcrz <- 12.7
  steel_grade <- 'S355'
  member_type <- 'UB'
  alpha_T <- 0.000012
  delta_T <- 10
  k_T <- 0.8
  E <- 210
  IL <- 50
  k <- 1
  isTopLevel <- T

  # calculate Ned (kN)
  Ned <- axial_compression_force(isTopLevel, DL, LL, L, AF, theta, spacing, Lcry, Lcrz, steel_grade, member_type, alpha_T, delta_T, k_T, E, IL)

  # calculate max compressive axial force in chords
  N_ch_Ed <- max_compressive_axial_force_in_chords(k, L, A=44.3, n=2, Ad=12.47, Lch=1, E, h0=8, Ned)

  # determine member size
  member_size <- trial_member_size(Lcry, Lcrz, Ned, steel_grade, member_type)

  # apply 1st check
  check1 <- check_overall_buckling_resistance_about_yy_axis(member_size, member_type, steel_grade, k, L, E)

  # display results
  print( paste0('Ned = ', Ned, ' kN') )
  print( paste0('N_ch_Ed = ', N_ch_Ed, ' kN') )
  print( paste0('Selected trial member size: ', member_size) )
  print( paste0('check #1 = ', check1) )
}

