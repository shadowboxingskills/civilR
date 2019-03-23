usethis::use_package("devtools")
usethis::use_package("roxygen2")
usethis::use_package("readxl")
usethis::use_package("writexl")
usethis::use_package("dplyr")

# usethis::use_vignette("civilR")
# roxygen2::roxygenise()
# devtools::build_manual(path="./doc")
# system("R CMD Rd2pdf .")

#' Calculate the temperature load
#'
#' Calculate Temperature Load as a function of a surface changes of temperature, TL [\eqn{kN}].
#' Usually used for calculation of Axial Compression Force for the top level member. \deqn{TL = \alpha_T \, \delta_T \, k_T  \, E \, A}
#'
#' @param alpha_T Thermal coefficient of expansion [\eqn{degC}]
#' @param delta_T Change in temperature from the Installation temperature [\eqn{degC}]
#' @param k_T Coefficient Of temperature effect [dimensionless]
#' @param E Young's Modulus of Elasticity [\eqn{GPa} or \eqn{GN/m2}]
#' @param A Sectional area from table for given member size [\eqn{cm^2}]
#'
#' @export
#'
#' @return TL Temperature load [\eqn{kN}]
#'
temperature_load <- function(alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, A=94.4) {
  A_m2 <- A * 1e-4 # convert A from cm2 to m2
  E_kPa <- E * 1e6 # convert E from GPa to kPa
  TL <- alpha_T * delta_T * k_T * E_kPa * A_m2
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
#'   \item \eqn{Izz} Second moment of area axis \eqn{z-z} [\eqn{{cm}^4}]
#' }
#'
extract_member_dimensions <- function(h, b, m, member_type) {
  require(readxl)

  # h=533; b=165; m=66; member_type='UB'

  if ( member_type == "UB" ) {
    dimensions_table_name <- "./tables/UB-Dimensions_properties.xlsx"
    nmax <- 107
  } else {
    dimensions_table_name <- "./tables/UC-Dimensions_properties.xlsx"
    nmax <- 46
  }

  t <- readxl::read_excel( dimensions_table_name,
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
  Izz <- t$`Second moment of area Axis z-z cm4`[(t$h == h) & (t$b == b) & (t$m == m)]

  l <- list("A" = A, "tw" = tw, "tf" = tf, "Iyy" = Iyy, "sh" = sh, "sb" = sb, "Izz" = Izz)

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
#' Calculate the effective length of member, \eqn{L_e} [\eqn{m}].
#'
#' @param k Effective lengh coefficient [dimensionless]
#' @param L Length of strut between restraints [\eqn{m}]
#'
#' @export
#'
#' @return \eqn{L_e} Effective length of strut [\eqn{m}]
#'
effective_length_of_member <- function(k, L) {
  return( k * L )
}


#' Calculate the effective second moment of area
#'
#' Compute the effective second moment of area [\eqn{{mm}^4}].
#' \eqn{I_{eff}} is a function of the distance between the centroids of the chords and the section area of a chord, calculated as \eqn{ I_{eff} = 0.5 \, {h_0}^2 \, A }.
#'
#' @param h0 Distance between centroids of chords [\eqn{mm}]
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
#' Calculate the plastic resistance of the cross-section to compression [\eqn{N}], based on cross-section area \eqn{A} and yield strength \eqn{f_y}.
#'
#' @param A Cross-section area of the strut [\eqn{{cm}^2}]
#' @param fy Yield strength [\eqn{kN/{mm}^2}]
#'
#' @export
#'
#' @return \eqn{N_{pl,R_d}} Plastic resistance of the cross-section to compression [\eqn{N}]
#'
plastic_resistance_of_cross_section_to_compression <- function(A, fy) {
  return( fy * A * 100 )
}


#' Calculate the Euler buckling load
#'
#' Calculate the Euler buckling load [\eqn{kN}] \deqn{ N_{cr,ch}=\frac{\pi^2 \, E \, I}{{L_e}^2} }
#'
#' @param Le Effective length of strut [\eqn{mm}]
#' @param E Young modulus [\eqn{MPa} or \eqn{MN/m^2}]
#' @param I - check 1: \eqn{I_{yy}}, second moment of area Axis \eqn{y-y} [\eqn{{cm}^4}]. Check 2: \eqn{I_{eff}}, Effective second moment of area [\eqn{{mm}^4}]. Check 3: \eqn{I_{eff}} or \eqn{I_{zz}} [\eqn{{mm}^4}]
#'
#' @export
#'
#' @return \eqn{N_{cr}} Euler buckling load [\eqn{kN}]
#'
Euler_buckling_load <- function(Le, E, I) {
  Ncr <- ( pi^2 * E * I * 1e4 ) / Le^2 # [N]
  Ncr <- Ncr * 1e-3 # [kN]
  return(Ncr)
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
#' @param isTopLevel Is member located at top level? [boolean]
#' @param DL Dead load / self-weight of member [\eqn{kN/m}]
#' @param LL Live load / imposed load [\eqn{kN/m}]
#' @param L Total length of member [\eqn{m}]
#' @param P Axial compression force of member per meter [\eqn{kN/m}]
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
#' @param gamma Partial factor for action [dimensionless], as per EN 1990:2002 standard
#'
#' @export
#'
#' @return \eqn{N_{ed}} Axial compression force [\eqn{kN}], \eqn{TL} Temperature Load [\eqn{kN}]
#'
axial_compression_force <- function( isTopLevel=T, DL=1, LL=1, L=12.5, P=247, theta=90, spacing=6,
                                     Lcry=12.7, Lcrz=1.0, steel_grade='S355', member_type='UB',
                                     alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, IL=50, gamma=1.35 )
  {
  # DL=1; LL=1; L=12.5; P=247; theta=90; spacing=6
  # Lcry=12.5; Lcrz=1.6; steel_grade='S355'; member_type='UB'
  # alpha_T=0.000012; delta_T=10; k_T=0.8; E=210; IL=50

  # SF, axial force / strut force (kN)
  SF <- ( P * spacing * gamma ) / sin( theta * pi / 180 ) # Axial compressional force, Ned [kN]

  e <- 30 / 1000 # [m] = 30mm, eccentricity (e = 10% of d > 30mm)

  Ned_no_TL <- SF * (1 + e) # correct for eccentricity

  Ned_no_TL <- round( Ned_no_TL / 2 ) # divide by 2 (force distributed between 2 struts)


  if ( isTopLevel ) { # top level only
    # find trial member size
    mb_no_TL <- trial_member_size(Lcry, Lcrz, Ned_no_TL, steel_grade, member_type)

    # extract member area from reference table
    l <- convert_member_dimensions_string_to_elements(mb_no_TL)
    A_no_TL <- extract_member_dimensions(l$h, l$b , l$m, member_type)$A # Area in cm2

    # calculate temperature load
    TL <- round( temperature_load(alpha_T, delta_T, k_T, E, A_no_TL) )

    # leading temperature partial factor correction
    TL <- TL * 1.5

    Ned_TL <- Ned_no_TL + TL

    Ned_TL <- round( Ned_TL / 2 ) # divide by 2 (force distributed between 2 struts)
  }

  if ( isTopLevel ) {
    Ned_output <- Ned_TL
    TL_output <- round(TL)
  } else {
    Ned_output <- Ned_no_TL
    TL_output <- NA
  }

  return( list("Ned" = Ned_output, "TL" = TL_output) )
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
  require(dplyr)

  # Lcry=12.7; Lcrz=1; Ned=3638; steel_grade="S355"; member_type="UB"

  # select file name
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

  # function to select closest critical length
  closest_critical_length_index <- function(t, Lcr, member_type) {
    col_discrete_values <- colnames(t)
    drops <- c("Axis", "h", "b", "m")
    critical_length_vector <- as.numeric( col_discrete_values[!(col_discrete_values %in% drops)] )

    return( which.min(abs(critical_length_vector-Lcr)) + 1 )
  }

  file_name <- select_file_name(steel_grade, member_type)
  t <- readxl::read_excel( path = file_name,
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

  critical_length_Lcry_colname <- closest_critical_length_index(t, Lcry, member_type)
  critical_length_Lcrz_colname <- closest_critical_length_index(t, Lcrz, member_type)

  determine_h_b_m_Ned_z_for_closest_Ned_y <- function(t, Ned) {
    # Lcry
    t_Nb_y <- subset( t, (Axis == "Nb,y,Rd") )
    x_Nb_y <- t_Nb_y[, critical_length_Lcry_colname]

    colnames(x_Nb_y) <- "Nb_y"
    x_Nb_y <- as.numeric( x_Nb_y$"Nb_y" )

    i <- which(abs(x_Nb_y-Ned)==min(abs(x_Nb_y-Ned)))
    i <- i[1]

    Ned_y <- x_Nb_y[i]

    # height h(mm) x width b(mm) x mass m(kg/m) for Lcry
    h <- as.numeric( t_Nb_y[i, "h"] )
    b <- as.numeric( t_Nb_y[i, "b"] )
    m <- as.numeric( t_Nb_y[i, "m"] )

    trial_member_size <- paste(h, b, m, sep=' x ')


    # Lcrz
    t_Nb_z <- subset( t, (Axis == "Nb,z,Rd") )
    x_Nb_z <- t_Nb_z[, critical_length_Lcrz_colname]

    colnames(x_Nb_z) <- "Nb_z"
    x_Nb_z <- as.numeric( x_Nb_z$"Nb_z" )

    Ned_z <- x_Nb_z[i]

    return( list("h" = h, "b" = b, "m" = m, "trial_member_size"=trial_member_size, "Ned_y"=Ned_y, "Ned_z"=Ned_z) )
  }

  # critical_length_Lcry_colname <- as.character(format(critical_length_Lcry_colname, nsmall = 1))
  # critical_length_Lcrz_colname <- as.character(format(critical_length_Lcrz_colname, nsmall = 1))

  # col_y <- colnames(t)[critical_length_Lcry_colname]
  # col_z <- colnames(t)[critical_length_Lcrz_colname]


  t$hbm <- civilR::convert_member_dimensions_to_string(t$h, t$b, t$m)
  t_trimmed_z <- subset( t, Axis == "Nb,z,Rd" )

  l <- determine_h_b_m_Ned_z_for_closest_Ned_y(t, Ned)

  while ( ((l$Ned_y < Ned) | (l$Ned_z < Ned)) & (dim(t)[1] >= 2) )
  {
    t_trimmed_y <- subset( t, ( sapply(t[,critical_length_Lcry_colname], as.numeric) > Ned ) & ( Axis == "Nb,y,Rd" ) )
    t_b <- rbind(t_trimmed_y, t_trimmed_z)
    t_b <- dplyr::group_by(t_b, hbm)
    t_b <- dplyr::filter(t_b, n()>1)

    t <- t_b

    if ( dim(t)[1] >= 2 ) {
      l <- determine_h_b_m_Ned_z_for_closest_Ned_y(t, Ned)
    }

    # print(l$Ned_y)
    # print(l$Ned_z)
    # print(l$trial_member_size)
  }

  # l$Ned_y
  # l$Ned_z
  if ( dim(t)[1] >= 2 ) {
    return(l$trial_member_size)
  } else {
    return("undetermined")
  }

}


#' Perform check #1, calculating the overall buckling resistance of member about major \eqn{y-y} axis
#'
#' Calculate the overall buckling resistance of member about \eqn{y-y} axis, based on EC3 Approach. \deqn{L_e=kL} [\eqn{mm}]
#' where \eqn{L} is the critical length for buckling about major axis \eqn{y-y}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [\eqn{kN}] \deqn{N_{pl,R_d}= 2(A \, fy)}
#'   \item The Euler buckling load [\eqn{kN}] \deqn{N_{cr,X}=\frac{\pi^2 \, E \, I_{yy}}{{L_e}^2}}
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
#' @return \eqn{N_{b,Rd,X}} Overall buckling resistance of struts about major y-y axis [\eqn{kN}], \eqn{N_{b,R_d,X}}, \eqn{f_y}, \eqn{N_{pl,R_d}}, \eqn{N_{cr,X}}, \eqn{{\bar{\lambda_X}}}, \eqn{\alpha_{yy}}, \eqn{X}
#'
check_overall_buckling_resistance_about_yy_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {

  # trial_member_size="533 x 165 x 66"; member_type="UB"; steel_grade="S355"; k=1; L=12.5; E=210

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- extract_member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- yield_strength(l$tw, l$tf, steel_grade)

  # Le, effective length of strut [m]
  Le <- effective_length_of_member(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression [kN]
  N_pl_Rd <- 2 * plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load [kN]
  Ncr <- Euler_buckling_load(Le * 1000, E * 1000, l$Iyy)

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd, Ncr)

  # imperfection_factor_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_yy(s$h, s$b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_X <- round( overall_buckling_resistance_about_axis(X, N_pl_Rd) )

  return(
    list("N_b_Rd_X" = round(N_b_Rd_X),
         "fy" = round(fy),
         "N_pl_Rd" = round(N_pl_Rd),
         "Ncr" = round(Ncr),
         "lambda_bar" = round(lambda_bar, 2),
         "alpha_yy" = round(alpha_yy, 2),
         "X" = round(X, 2)
         )
    )
}


#' Perform check #2, calculating the overall buckling resistance of struts about major \eqn{z-z} axis
#'
#' Calculate the overall buckling resistance of member about \eqn{z-z} axis, based on EC3 Approach. \deqn{L_e = k \, L} [\eqn{mm}]
#' where \eqn{L} is the critical length for buckling about major axis \eqn{z-z}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [\eqn{kN}] \deqn{N_{pl,R_d}= 2(A \, fy)}
#'   \item The Euler buckling load [\eqn{kN}] \deqn{N_{cr,Y}=\frac{\pi^2 \, E \, I_{eff}}{{L_e}^2}}
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
#' @param h0 Distance between centroids of chords [\eqn{mm}]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,Y}} Overall buckling resistance of struts about z-z axis [\eqn{kN}], \eqn{N_{b,R_d,Y}}, \eqn{f_y}, \eqn{N_{pl,R_d}}, \eqn{I_{eff}}, \eqn{N_{cr,Y}}, \eqn{{\bar{\lambda_Y}}}, \eqn{\alpha_{yy}}, \eqn{X}
#'
check_overall_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E, h0) {

  # trial_member_size="533 x 165 x 66"; member_type="UB"; steel_grade="S355"; k=1; L=12.5; E=210; h0=1000

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- extract_member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- yield_strength(l$tw, l$tf, steel_grade)

  # Le, effective length of strut [m]
  Le <- effective_length_of_member(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- 2 * plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ieff, effective second moment of area [mm4 --> cm4]
  Ieff <- effective_second_moment_of_area(h0, l$A) / 1e4

  # Ncr, Euler buckling load [kN]
  Ncr <- Euler_buckling_load(Le * 1000, E * 1000, Ieff)

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd, Ncr)

  # imperfection_factor_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_yy(s$h, s$b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_Y <- round( overall_buckling_resistance_about_axis(X, N_pl_Rd) )

  return(
    list("N_b_Rd_Y" = round(N_b_Rd_Y),
         "fy" = round(fy),
         "N_pl_Rd" = round(N_pl_Rd),
         "Ieff" = round(Ieff),
         "Ncr" = round(Ncr),
         "lambda_bar" = round(lambda_bar, 2),
         "alpha_yy" = round(alpha_yy, 2),
         "X" = round(X, 2)
    )
  )
}


#' Perform check #3, calculating the local buckling resistance of struts about minor \eqn{z-z} axis
#'
#' Calculate the local buckling resistance of member about minor \eqn{z-z} axis, based on EC3 Approach. \deqn{L_e=kL_{ch}} [\eqn{mm}]
#' where \eqn{L} is the critical length for buckling about minor axis \eqn{z-z}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [\eqn{kN}] \deqn{N_{pl,R_d,ch}= 2(A  \, fy)}
#'   \item The Euler buckling load [\eqn{kN}] \deqn{N_{cr,ch}=\frac{\pi^2 \,  E \,  I_{zz}}{{L_e}^2}}
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
#' @param Lch Length of chord [\eqn{mm}]
#' @param E Young's Modulus of Elasticity [\eqn{GPa}]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,X}} Local buckling resistance of struts about \eqn{z-z} axis [\eqn{kN}], \eqn{f_y}, \eqn{N_{pl,R_d}}, \eqn{N_{cr}}, \eqn{{\bar{\lambda}}}, \eqn{\alpha_{yy}}, \eqn{X}
#'
check_local_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, Lch, E) {

  # trial_member_size="533 x 165 x 66"; member_type="UB"; steel_grade="S355"; k=1; Lch=1; E=210
  # trial_member_size="686 x 254 x 125"; member_type="UB"; steel_grade="S355"; k=1; Lch=1; E=210

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- civilR::extract_member_dimensions(s$h, s$b, s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- yield_strength(l$tw, l$tf, steel_grade)

  # Le, effective length of strut [m]
  Le <- effective_length_of_member(k, Lch / 1000)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load [kN]
  Ncr <- Euler_buckling_load(Le * 1000, E * 1000, l$Izz)

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd, Ncr)

  # imperfection_factor_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_yy(s$h, s$b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd_ch, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_ch <- round( overall_buckling_resistance_about_axis(X, N_pl_Rd) )

  return(
    list("N_b_Rd_ch" = round(N_b_Rd_ch),
         "fy" = round(fy),
         "N_pl_Rd" = round(N_pl_Rd),
         "Ncr" = round(Ncr),
         "lambda_bar" = round(lambda_bar, 2),
         "alpha_yy" = round(alpha_yy, 2),
         "X" = round(X, 2)
    )
  )
}


#' Maximum compressive axial force in the chords
#'
#' Determine maximum compressive axial force in the chords at mid-length of the strut, \eqn{N_{ch,E_d}} [\eqn{kN}]
#'
#' Calculation steps are as follows:
#' \enumerate{
#'   \item Effective length \eqn{L_e = k \, L} [\eqn{mm}], where \eqn{L} is the length between two vertical supports of laced struts.
#'   \item Effective second moment of area \deqn{I_{eff}=0.5 \, h_0 \, A  [\eqn{{mm}^4}]. \eqn{I_{eff}}, where \eqn{h_0} [\eqn{cm}] is the distance between centroids of the chords and \eqn{A_{ch}}, [\eqn{{cm}^4}] is the cross-sectional area of one chord.
#'   \item Shear stiffness for K-shape lacing \eqn{S_v} [\eqn{kN}], where \eqn{d} is the lenth of diagonals \eqn{d = sqrt{ {h_0}^2 + L_{ch} }} [\eqn{mm}].
#'   \item Calculate the Euler buckling load \eqn{N_{cr,ch}} [\eqn{kN}] \deqn{ N_{cr,ch}=\frac{\pi^2 \, E \, I} {{L_e}^2} }, where \eqn{L_e} is the effective length between two vertical supports.
#'   \item Compute the second order bending moment \eqn{{M_{E_d}}^{II}} [\eqn{kN.m}].
#'   \item Output maximum compressive axial force in the chords \eqn{N_{ch,E_d}} [\eqn{kN}] \deqn{ N_{ch,E_d} = \frac{N_{E_d}}{2} + \frac{M_{E_d} \, h_0 \, A}{ 2 \, I_{eff}} }
#' }
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type, categorical: 'UC' or 'UB'
#' @param steel_grade steel_grade [\eqn{N/{mm}^2}], categorical: 'S355' or 'S275'
#' @param k Coefficient of length as function of wall rigidity [dimensionless]
#' @param L Length between two restraints [\eqn{m}]
#' @param n Number of lacing planes, default [\eqn{n=2}]
#' @param Ad Section area of diagonal (lacing), [\eqn{cm^2}]
#' @param Lch Length of chord of betwen restrains (lace points) [\eqn{m}]
#' @param E Young modulus [\eqn{GPa} or \eqn{GN/m^2}]
#' @param h0 Distance between centroids of chords [\eqn{m}]
#' @param Ned Axial compression Force [\eqn{kN}]
#'
#' @export
#'
#' @return \eqn{N_{ch,E_d}} Maximum compressive axial force in the chords [\eqn{kN}], \eqn{S_v}, \eqn{N_{cr,ch}}, \eqn{{M_{E_d}}}
#'
max_compressive_axial_force_in_chords <- function(trial_member_size, member_type, steel_grade, k, L, n, Ad, Lch, E, h0, Ned) {

  s <- convert_member_dimensions_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- extract_member_dimensions(s$h, s$b , s$m, member_type)

  # Le, effective length of strut [m]
  Le <- effective_length_of_member(k, L)

  # Ieff, effective second moment of area [mm4 --> cm4]
  Ieff <- effective_second_moment_of_area(h0, l$A) / 1e4

  # n=2; Ad=1552; Lch=1; E=210000; h0=1000

  # Shear stiffness for K-shape lacing
  Sv <- shear_stiffness(n, Ad, Lch, E * 1000, h0)

  # Ncr, Euler buckling load [kN]
  Ncr <- Euler_buckling_load(Le * 1000, E * 1000, Ieff)

  # The second order bending moment
  MEd <- second_order_bending_moment(L, Ned, Sv, Ncr)

  # Output maximum compressive axial force in the chords
  N_ch_Ed <- round( (0.5 * Ned) + (MEd * h0 * l$A) / (2*Ieff) )

  return(
    list("N_ch_Ed" = round(N_ch_Ed),
         "Sv" = round(Sv),
         "Ncr" = round(Ncr),
         "MEd" = round(lambda_bar)
         )
    )
}


#' Read input table from given Excel file
#'
#' Read input table from given Excel file.
#'
#' @param file_name Path and file name of the input table
#'
#' @export
#'
#' @return Input table
#'
read_input_table <- function(file_name="tables/input/trial1_kotik.xlsx") {
  require(readxl)

  t <- readxl::read_excel(path = file_name,
                          col_types = c("text", rep("numeric", 11), "text", rep("numeric", 4), "text", "text", "numeric", "numeric") )

  names(t) <- c( "Strut.name", "L.m", "k", "s.m", "Lcry.m", "Lcrz.m", "theta.deg", "Lch.mm",
                 "h0.mm", "n", "Ad.mm2", "E.GPa", "top.level.y.n", "DL.kN.m", "LL.kN.m",
                 "P.kN.m", "IL.kN.m", "steel.grade", "member.type", "alpha_T", "k_T" )
  t$steel.grade <- paste0( 'S', t$steel.grade )
  t$top.level.y.n <- (t$top.level.y.n == "y")

  t$delta_T <- 10
  t$gamma <- 1.35

  return(t)
}


#' Process input table
#'
#' Process input table.
#'
#' @export
#'
#' @return Processed table, adding computed outputs:
#'
process_input_table <- function() {
  t <- read_input_table()

  # t_INPUT <- t
  # View(t_INPUT)

  t$TL <- NA
  t$Ned_2 <- 0
  t$Ned <- 0
  t$selected_member_size <- "undetermined"

  # Check 1
  t$fy_1 <- NA
  t$N_pl_Rd_1 <- NA
  t$Ncr_1 <- NA
  t$lambda_bar_1 <- NA
  t$alpha_yy_1 <- NA
  t$X_1 <- NA
  t$N_b_Rd_X <- NA

  # Check 2
  t$fy_2 <- NA
  t$N_pl_Rd_2 <- NA
  t$Ieff_2 <- NA
  t$Ncr_2 <- NA
  t$lambda_bar_2 <- NA
  t$alpha_yy_2 <- NA
  t$X_2 <- NA
  t$N_b_Rd_Y <- NA

  # Check 3
  t$fy_3 <- NA
  t$N_pl_Rd_3 <- NA
  t$Ncr_3 <- NA
  t$lambda_bar_3 <- NA
  t$alpha_yy_3 <- NA
  t$X_3 <- NA
  t$N_b_Rd_ch <- NA

  # Check 4
  t$Sv <- NA
  t$Ncr <- NA
  t$MEd <- NA
  t$N_ch_Ed <- NA

  t$final_check <- F


  for (row in 1:nrow(t)) {
    # calculate TL (kN)
    t[row, "TL"] <- axial_compression_force(as.logical(t[row, "top.level.y.n"]),
                                             as.numeric(t[row, "DL.kN.m"]),
                                             as.numeric(t[row, "LL.kN.m"]),
                                             as.numeric(t[row, "L.m"]),
                                             as.numeric(t[row, "P.kN.m"]),
                                             as.numeric(t[row, "theta.deg"]),
                                             as.numeric(t[row, "s.m"]),
                                             as.numeric(t[row, "Lcry.m"]),
                                             as.numeric(t[row, "Lcrz.m"]),
                                             as.character(t[row, "steel.grade"]),
                                             as.character(t[row, "member.type"]),
                                             as.numeric(t[row, "alpha_T"]),
                                             as.numeric(t[row, "delta_T"]),
                                             as.numeric(t[row, "k_T"]),
                                             as.numeric(t[row, "E.GPa"]),
                                             as.numeric(t[row, "IL.kN.m"]),
                                             as.numeric(t[row, "gamma"])
    )$TL
    print(as.numeric(t[row, "TL"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Ned (kN)
    t[row, "Ned"] <- axial_compression_force(as.logical(t[row, "top.level.y.n"]),
                                             as.numeric(t[row, "DL.kN.m"]),
                                             as.numeric(t[row, "LL.kN.m"]),
                                             as.numeric(t[row, "L.m"]),
                                             as.numeric(t[row, "P.kN.m"]),
                                             as.numeric(t[row, "theta.deg"]),
                                             as.numeric(t[row, "s.m"]),
                                             as.numeric(t[row, "Lcry.m"]),
                                             as.numeric(t[row, "Lcrz.m"]),
                                             as.character(t[row, "steel.grade"]),
                                             as.character(t[row, "member.type"]),
                                             as.numeric(t[row, "alpha_T"]),
                                             as.numeric(t[row, "delta_T"]),
                                             as.numeric(t[row, "k_T"]),
                                             as.numeric(t[row, "E.GPa"]),
                                             as.numeric(t[row, "IL.kN.m"]),
                                             as.numeric(t[row, "gamma"])
                                             )$Ned
    print(as.numeric(t[row, "Ned"]))
  }

  for (row in 1:nrow(t)) {
    t[row, "Ned_2"] <- round( as.numeric(t[row, "Ned"]) * 2 )
  }

  for (row in 1:nrow(t)) {
    # calculate Ned (kN)
    t[row, "selected_member_size"] <- trial_member_size(as.numeric(t[row, "Lcry.m"]),
                                                        as.numeric(t[row, "Lcrz.m"]),
                                                        as.numeric(t[row, "Ned"]),
                                                        as.character(t[row, "steel.grade"]),
                                                        as.character(t[row, "member.type"])
    )
    print(as.character(t[row, "selected_member_size"]))
  }

  #-----------------------------------#
  #            Check 1                #
  #-----------------------------------#

  for (row in 1:nrow(t)) {
    # calculate N_b_Rd_X for check 1 [kN]
    t[row, "N_b_Rd_X"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
    )$N_b_Rd_X
    print(as.character(t[row, "N_b_Rd_X"]))
  }

  for (row in 1:nrow(t)) {
    # calculate fy for check 1
    t[row, "fy_1"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
    )$fy
    print(as.character(t[row, "fy_1"]))
  }

  for (row in 1:nrow(t)) {
    # calculate N_pl_Rd for check 1
    t[row, "N_pl_Rd_1"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
    )$N_pl_Rd
    print(as.character(t[row, "N_pl_Rd_1"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Ncr for check 1
    t[row, "Ncr_1"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
    )$Ncr
    print(as.character(t[row, "Ncr_1"]))
  }

  for (row in 1:nrow(t)) {
    # calculate lambda_bar for check 1
    t[row, "lambda_bar_1"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
    )$lambda_bar
    print(as.character(t[row, "lambda_bar_1"]))
  }

  for (row in 1:nrow(t)) {
    # calculate alpha_yy for check 1
    t[row, "alpha_yy_1"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
    )$alpha_yy
    print(as.character(t[row, "alpha_yy_1"]))
  }

  for (row in 1:nrow(t)) {
    # calculate X for check 1
    t[row, "X_1"] <- check_overall_buckling_resistance_about_yy_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"])
                                                                           )$X
    print(as.character(t[row, "X_1"]))
  }


  #-----------------------------------#
  #            Check 2                #
  #-----------------------------------#

  for (row in 1:nrow(t)) {
    # calculate N_b_Rd_Y for check 2 [kN]
    t[row, "N_b_Rd_Y"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "L.m"]),
                                                                           as.numeric(t[row, "E.GPa"]),
                                                                           as.numeric(t[row, "h0.mm"])
                                                                          )$N_b_Rd_Y
    print(as.character(t[row, "N_b_Rd_Y"]))
  }

  for (row in 1:nrow(t)) {
    # calculate fy for check 2
    t[row, "fy_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$fy
    print(as.character(t[row, "fy_2"]))
  }

  for (row in 1:nrow(t)) {
    # calculate N_pl_Rd for check 2
    t[row, "N_pl_Rd_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$N_pl_Rd
    print(as.character(t[row, "N_pl_Rd_2"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Ieff for check 2
    t[row, "Ieff_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$Ieff
    print(as.character(t[row, "Ieff_2"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Ncr for check 2
    t[row, "Ncr_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$Ncr
    print(as.character(t[row, "Ncr_2"]))
  }

  for (row in 1:nrow(t)) {
    # calculate lambda_bar for check 2
    t[row, "lambda_bar_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$lambda_bar
    print(as.character(t[row, "lambda_bar_2"]))
  }

  for (row in 1:nrow(t)) {
    # calculate alpha_yy for check 2
    t[row, "alpha_yy_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$alpha_yy
    print(as.character(t[row, "alpha_yy_2"]))
  }

  for (row in 1:nrow(t)) {
    # calculate X for check 2
    t[row, "X_2"] <- check_overall_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                          as.character(t[row, "member.type"]),
                                                                          as.character(t[row, "steel.grade"]),
                                                                          as.numeric(t[row, "k"]),
                                                                          as.numeric(t[row, "L.m"]),
                                                                          as.numeric(t[row, "E.GPa"]),
                                                                          as.numeric(t[row, "h0.mm"])
    )$X
    print(as.character(t[row, "X_2"]))
  }


  #-----------------------------------#
  #            Check 3                #
  #-----------------------------------#

  for (row in 1:nrow(t)) {
    # calculate N_b_Rd_ch for check 3 [kN]
    t[row, "N_b_Rd_ch"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                           as.character(t[row, "member.type"]),
                                                                           as.character(t[row, "steel.grade"]),
                                                                           as.numeric(t[row, "k"]),
                                                                           as.numeric(t[row, "Lch.mm"]),
                                                                           as.numeric(t[row, "E.GPa"])
                                                                         )$N_b_Rd_ch
    print(as.character(t[row, "N_b_Rd_ch"]))
  }

  for (row in 1:nrow(t)) {
    # calculate fy for check 3
    t[row, "fy_3"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                         as.character(t[row, "member.type"]),
                                                                         as.character(t[row, "steel.grade"]),
                                                                         as.numeric(t[row, "k"]),
                                                                         as.numeric(t[row, "Lch.mm"]),
                                                                         as.numeric(t[row, "E.GPa"])
    )$fy
    print(as.character(t[row, "fy_3"]))
  }

  for (row in 1:nrow(t)) {
    # calculate N_pl_Rd for check 3
    t[row, "N_pl_Rd_3"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                         as.character(t[row, "member.type"]),
                                                                         as.character(t[row, "steel.grade"]),
                                                                         as.numeric(t[row, "k"]),
                                                                         as.numeric(t[row, "Lch.mm"]),
                                                                         as.numeric(t[row, "E.GPa"])
    )$N_pl_Rd
    print(as.character(t[row, "N_pl_Rd_3"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Ncr for check 3
    t[row, "Ncr_3"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                         as.character(t[row, "member.type"]),
                                                                         as.character(t[row, "steel.grade"]),
                                                                         as.numeric(t[row, "k"]),
                                                                         as.numeric(t[row, "Lch.mm"]),
                                                                         as.numeric(t[row, "E.GPa"])
    )$Ncr
    print(as.character(t[row, "Ncr_3"]))
  }

  for (row in 1:nrow(t)) {
    # calculate lambda_bar for check 3
    t[row, "lambda_bar_3"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                         as.character(t[row, "member.type"]),
                                                                         as.character(t[row, "steel.grade"]),
                                                                         as.numeric(t[row, "k"]),
                                                                         as.numeric(t[row, "Lch.mm"]),
                                                                         as.numeric(t[row, "E.GPa"])
    )$lambda_bar
    print(as.character(t[row, "lambda_bar_3"]))
  }

  for (row in 1:nrow(t)) {
    # calculate alpha_yy for check 3
    t[row, "alpha_yy_3"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                         as.character(t[row, "member.type"]),
                                                                         as.character(t[row, "steel.grade"]),
                                                                         as.numeric(t[row, "k"]),
                                                                         as.numeric(t[row, "Lch.mm"]),
                                                                         as.numeric(t[row, "E.GPa"])
    )$alpha_yy
    print(as.character(t[row, "alpha_yy_3"]))
  }

  for (row in 1:nrow(t)) {
    # calculate X for check 3
    t[row, "X_3"] <- check_local_buckling_resistance_about_zz_axis(as.character(t[row, "selected_member_size"]),
                                                                         as.character(t[row, "member.type"]),
                                                                         as.character(t[row, "steel.grade"]),
                                                                         as.numeric(t[row, "k"]),
                                                                         as.numeric(t[row, "Lch.mm"]),
                                                                         as.numeric(t[row, "E.GPa"])
    )$X
    print(as.character(t[row, "X_3"]))
  }


  #-----------------------------------#
  #            Check 4                #
  #-----------------------------------#

  for (row in 1:nrow(t)) {
    # calculate N_ch_Ed [kN]
    t[row, "N_ch_Ed"] <- max_compressive_axial_force_in_chords(as.character(t[row, "selected_member_size"]),
                                                                   as.character(t[row, "member.type"]),
                                                                   as.character(t[row, "steel.grade"]),
                                                                   as.numeric(t[row, "k"]),
                                                                   as.numeric(t[row, "L.m"]),
                                                                   as.numeric(t[row, "n"]),
                                                                   as.numeric(t[row, "Ad.mm2"]),
                                                                   as.numeric(t[row, "Lch.mm"]),
                                                                   as.numeric(t[row, "E.GPa"]),
                                                                   as.numeric(t[row, "h0.mm"]),
                                                                   as.numeric(t[row, "Ned"])
                                                                   )$N_ch_Ed
    print(as.character(t[row, "N_ch_Ed"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Sv
    t[row, "Sv"] <- max_compressive_axial_force_in_chords(as.character(t[row, "selected_member_size"]),
                                                               as.character(t[row, "member.type"]),
                                                               as.character(t[row, "steel.grade"]),
                                                               as.numeric(t[row, "k"]),
                                                               as.numeric(t[row, "L.m"]),
                                                               as.numeric(t[row, "n"]),
                                                               as.numeric(t[row, "Ad.mm2"]),
                                                               as.numeric(t[row, "Lch.mm"]),
                                                               as.numeric(t[row, "E.GPa"]),
                                                               as.numeric(t[row, "h0.mm"]),
                                                               as.numeric(t[row, "Ned"])
    )$Sv
    print(as.character(t[row, "Sv"]))
  }

  for (row in 1:nrow(t)) {
    # calculate Ncr
    t[row, "Ncr"] <- max_compressive_axial_force_in_chords(as.character(t[row, "selected_member_size"]),
                                                               as.character(t[row, "member.type"]),
                                                               as.character(t[row, "steel.grade"]),
                                                               as.numeric(t[row, "k"]),
                                                               as.numeric(t[row, "L.m"]),
                                                               as.numeric(t[row, "n"]),
                                                               as.numeric(t[row, "Ad.mm2"]),
                                                               as.numeric(t[row, "Lch.mm"]),
                                                               as.numeric(t[row, "E.GPa"]),
                                                               as.numeric(t[row, "h0.mm"]),
                                                               as.numeric(t[row, "Ned"])
    )$Ncr
    print(as.character(t[row, "Ncr"]))
  }

  for (row in 1:nrow(t)) {
    # calculate MEd
    t[row, "MEd"] <- max_compressive_axial_force_in_chords(as.character(t[row, "selected_member_size"]),
                                                               as.character(t[row, "member.type"]),
                                                               as.character(t[row, "steel.grade"]),
                                                               as.numeric(t[row, "k"]),
                                                               as.numeric(t[row, "L.m"]),
                                                               as.numeric(t[row, "n"]),
                                                               as.numeric(t[row, "Ad.mm2"]),
                                                               as.numeric(t[row, "Lch.mm"]),
                                                               as.numeric(t[row, "E.GPa"]),
                                                               as.numeric(t[row, "h0.mm"]),
                                                               as.numeric(t[row, "Ned"])
    )$MEd
    print(as.character(t[row, "MEd"]))
  }


  for (row in 1:nrow(t)) {
    # calculate final_check [T/F]
    t[row, "final_check"] <- as.logical( (as.numeric(t[row, "N_ch_Ed"]) / min(as.numeric(t[row, "N_b_Rd_X"]), as.numeric(t[row, "N_b_Rd_Y"]), as.numeric(t[row, "N_b_Rd_ch"]))) < 1.0 )
    print(as.character(t[row, "N_ch_Ed"]))
  }

  return(t)
}


#' Export output table to Excel file
#'
#' Export output table to Excel file.
#'
#' @param file_name Path and file name of the output table
#'
#' @export
#'
#' @return None
#'
compute_output_table <- function(file_name="tables/input/output_processed_table.xlsx") {
  require(writexl)

  t_OUTPUT <- process_input_table()
  writexl::write_xlsx(t_OUTPUT, path = file_name)
  View(t_OUTPUT)

  print("")
  print("")
  print("Completed OK")
  print("")
  print("================================================================")
  print("Processed table has been exported to output_processed_table.xlsx")
  print("================================================================")
}

