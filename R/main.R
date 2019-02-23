usethis::use_package("devtools")
usethis::use_package("roxygen2")
usethis::use_package("readxl")

require(devtools)
require(roxygen2)
# usethis::use_vignette("civilR")
# roxygen2::roxygenise()
# devtools::build_manual()

#' Lcry, critical_length_major_axis_Lcry [m]
#'
#' This is the description.
#'
#' @param L Total length of member [m]
#' @param Lkp Length to king post [m]
#' @param Lsp Length from splays [m]
#'
#' @export
#'
#' @return Lcry critical_length_major_axis_Lcry [m]
#'
critical_length_major_axis_Lcry <- function (L, Lkp, Lsp) {
  Lkp <- L / 2
  Lcry <- Lkp - Lsp
  return(Lcry)
}


#' Lcrz, critical_length_major_axis_Lcrz [m]
#'
#' This is the description.
#'
#' @param L Total length of member [m]
#' @param Lkp Length to king post [m]
#' @param Lsp Length from splays [m]
#'
#' @export
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
critical_length_minor_axis_Lcrz <- function (L, Lkp, Lsp) {
  Lcrz <- 0 # TBC
  return( Lcrz )
}


#' TL, temperature load [kN]
#'
#' This is the description.
#'
#' @param alpha_T Thermal coefficient of expansion [degC]
#' @param delta_T Change in temperature from the Installation temperature [degC]
#' @param k_T Coefficient Of temperature effect [dimensionless]
#' @param E Young's Modulus of Elasticity [GPa]
#' @param A Sectional area from table for given member size [cm2]
#'
#' @export
#'
#' @examples
#' calculate_TL <- function(alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, A=200)
#'
#' @return TL Temperature load [kN]
#'
calculate_TL <- function(alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, A) {
  TL <- alpha_T * delta_T * k_T * E * A
  return(TL)
}


#' Extract dimensions from reference table
#'
#' This is the description.
#'
#' @param h Member height?? [mm]
#' @param b Member width?? [mm]
#' @param m Member mass [kg??]
#' @param member_type Member type, UB or UC
#'
#' @export
#'
#' @examples
#' l1 <- member_dimensions(h=610, b=229, m=140, member_type="UB")
#' l2 <- member_dimensions(h=356, b=406, m=1299, member_type="UC")
#' l1$A; l2$tw
#'
#' @return
#' \itemize{
#'   \item \code{A} Area of section [cm2]
#'   \item \code{Two} Second item \eqn{\sqrt{a + \frac{b}{c}}}
#' }
#' @return tw Thickness web [mm]
#' @return tf Thickness flange tf [mm]
#' @return Iyy Second moment of area Axis y-y [cm4]
#' @return sh Depth of section h [mm]
#' @return sb Width of section b [mm]
#'
#'
member_dimensions <- function(h, b, m, member_type) {
  require(readxl)

  if ( member_type == "UB" ) {
    dimensions_table_name <- "./UB-Dimensions_properties.xlsx"
    nmax <- 107
  } else {
    dimensions_table_name <- "./UC-Dimensions_properties.xlsx"
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


# member_size_to_string
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
member_size_to_string <- function(h, b, m) {
  return( paste( h, b, m, sep=' x ' ) )
}
# member_size_to_string(h=610, b=229, m=140)


# member_size_to_string
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
member_size_string_to_elements <- function(s) {
  v <- as.numeric( unlist(strsplit(s, " x ")) )
  h <- v[1]
  b <- v[2]
  m <- v[3]

  return( list("h" = h, "b" = b, "m" = m) )
}
# member_size_string_to_elements("610 x 229 x 140")


# imperfection_factor_alpha_yy for rolled section (dimensionless)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
imperfection_factor_alpha_yy <- function(h, b, tf) {
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


# imperfection_factor_alpha_zz for rolled section (dimensionless)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
imperfection_factor_alpha_zz <- function(h, b, tf) {
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


# fy, yield strength [N/mm2]
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
fy <- function(tw, tf, steel_grade) {
  # tw thickness web [mm]
  # tf thickness flange [mm]
  # t maximum thickness [mm]

  t <- max( tw, tf )

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

  return( fy )
}
# fy(tw=47.6, tf=77, steel_grade="S355")


# Le, effective length of strut (mm)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
effective_length_of_strut <- function(k, L) {
  # k, effective lengh coefficient (dimensionless)
  # L, length of strut between restraints (mm)

  return( k * L )
}

# Ieff, Effective second moment of area (mm4)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
effective_second_moment_of_area <- function(h0, A) {
  # h0 - distance between centroids of chords (m)
  # A - cross-section area of strut (cm2)

  return( 0.5 * h0^2 * A * 100 )
}

# N_pl_Rd, plastic resistance of the cross-section to compression (kN)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
plastic_resistance_of_cross_section_to_compression <- function(A, fy) {
  # A, cross-section area of strut (cm2)
  # fy, yield strength [kN/mm2]

  return( fy * A * 100 )
}

# Ncr, Euler buckling load (kN)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
Euler_buckling_load <- function(Le, E, I) {
  # Le, effective length of strut (mm)
  # E, Young modulus (GPa <-> GN/m2)
  # I - check 1: Iyy, second moment of area Axis y-y (cm4)
  # I - check 2: Ieff, Effective second moment of area (mm4)
  # I - check 3: Ieff or Izz? (mm4)

  Ncr <- ( pi^2 * E * I ) / Le^2
  return( Ncr )
}

# lambda_bar, Relative slenderness (dimentionless)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
relative_slenderness <- function(N_pl_Rd, Ncr) {
  # N_pl_Rd, Plastic resistance of the cross-section to compression (kN)
  # Ncr, Euler buckling load (kN)

  return( sqrt( N_pl_Rd / Ncr ) )
}

# X, slenderness reduction factor (dimentionless)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
slenderness_reduction_factor <- function(alpha, lambda_bar) {
  # alpha - check 1: imperfection_factor_alpha_yy for rolled section (dimensionless)
  # alpha - check 2 & 3: imperfection_factor_alpha_zz for rolled section (dimensionless)
  # lambda_bar, Relative slenderness (dimentionless)

  Phi <- 0.5 * ( 1 + alpha * (lambda_bar - 0.2) + lambda_bar^2 )
  X <- 1 / ( Phi + sqrt( Phi^2 - lambda_bar^2 )  )
  return(X)
}

# N_b_Rd, overall buckling resistance of the struts about the axis (kN)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
overall_buckling_resistance_about_axis <- function(X, N_pl_Rd) {
  # X, slenderness reduction factor (dimentionless)
  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)

  return( X * N_pl_Rd )
}

# Sv, shear stiffness for K-shape lacing
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
shear_stiffness <- function(n=2, Ad, Lch, E, ho) {
  # n, number of planes of lacing, default n=2
  # Ad, section area of diagonal (lacing), cm2
  # Lch, length of chord of betwen restrains (lace points), m
  # E, Young modulus (GPa <-> GN/m2)
  # h0 - distance between centroids of chords (m)

  d <- sqrt( h0^2 + Lch^2 )  # length of diagonal
  Sv <- ( n * E * Ad * Lch * h0^2 ) / d^3
  return(Sv)
}


# MEd, second order moment [kN.m]
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
second_order_moment <- function(L, Ned, Sv, Ncr) {
  # L, length of strut between restraints (mm)
  # Ned, axial_compression_force_Ned (kN)
  # Sv, shear stiffness for K-shape lacing
  # Ncr, Euler buckling load from check2 global zz (kN)

  e0 <- L / 500 # e0, initial bow imperfection
  MEd_1 <- 0 # first order moment
  MEd <- ( Ned * e0 + MEd_1 ) / ( 1 - (Ned / Ncr) - (Ned / Sv))

  return(MEd)
}

# NEd_c, calculated NEd (kN)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
calculated_NEd <- function(N_b_Rd, Ieff, MEd, h0, A) {
  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  # Ieff, Effective second moment of area (mm4)
  # MEd, second order moment [kN.m]
  # h0 - distance between centroids of chords (m)
  # A, cross-section area of strut (cm2)

  return( 2 * ( N_b_Rd - (MEd*h0*A)/(2*Ieff) ) )
}

# VEd, maximum shear force in the lacing (for a laced strut subject to a compressive axial force only)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
maximum_shear_force_in_the_lacing <- function(MEd, L) {
  # MEd, second order moment [kN.m]
  # L, length of strut between restraints (mm)

  return( pi * MEd / L )
}


# axial_compression_force_Ned (kN)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
axial_compression_force_Ned <- function(
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
  # DL, dead load / self-weight of member (kN/m)
  # LL, live load / imposed load (kN/m)
  # L, total length of member (m)
  # AF, axial compression force of member per meter (kN/m)
  # theta, angle to wall (deg)
  # spacing (m)
  # critical_length_major_axis_Lcry (m)
  # critical_length_minor_axis_Lcrz (m)
  # steel grade, steel_grade (N/mm2) - categorical: S355/S275
  # member type, member_type (categorical: UC/UB)
  # alpha_T, Thermal coef. of expansion (degC)
  # delta_T, Change in temperature from the Installation temperature (degC)
  # k_T, Coefficient Of Temperature Effect (dimensionless)
  # E, Young's Modulus of Elasticity (GPa)
  # IL, Accidental Impact Load (kN/m)

  # DL=1; LL=1; L=12.7; AF=582; theta=90; spacing=7
  # Lcry=12.7; Lcrz=12.7; steel_grade='S355'; member_type='UB'
  # alpha_T=0.000012; delta_T=10; k_T=0.8; E=210; IL=50

  # SF, axial force / strut force (kN/m)
  # SF <- f(AF, theta)
  SF <- ( AF * spacing ) / sin( theta * pi / 180 )

  # load combination for strut design
  lc1 <- 1.35 * DL + 1.35 * SF + 1.5 * LL
  lc2 <- 1.35 * DL + 1.35 * SF + 1.05 *LL
  lc3 <- 1.35 * DL + 1.0 * SF + 1.5 * LL
  lc4 <- 1.35 * DL + 1.0 * SF + 1.05 * LL

  # design limit state
  da1_1 <- max( lc1, lc2 )
  da1_2 <- max( lc3, lc4 )

  # axial_compression_force_Ned (ULS) Ned_trial (kN)
  Ned_trial <- max( da1_1, da1_2 )

  # find trial member size
  mb <- trial_member_size(Lcry, Lcrz, Ned_trial, steel_grade, member_type)

  # extract member area from reference table
  l <- member_size_string_to_elements(mb)
  A <- member_dimensions(l$h, l$b , l$m, member_type)$A

  # calculate temperature load
  TL <- calculate_TL(alpha_T, delta_T, k_T, E, A)

  # load combination for strut design
  lc1 <- 1.35 * DL + 1.35 * SF + 1.5 * LL + 0.9 * TL
  lc2 <- 1.35 * DL + 1.35 * SF + 1.05 *LL + 1.5 * TL
  lc3 <- 1.35 * DL + 1.0 * SF + 1.5 * LL + 0.9 * TL
  lc4 <- 1.35 * DL + 1.0 * SF + 1.05 * LL + 1.5 * TL

  # axial_compression_force_Ned (ULS) Ned_ULS (kN)
  Ned_ULS <- max( lc1, lc2, lc3, lc4 )

  # load combination for strut design
  lc1 <- 1.0 * DL + 1.0 * SF + 0.7 * LL + 1.0 * IL
  lc2 <- 1.0 * DL + 1.0 * SF + 0.6 * LL + 0.5 * TL + 1.0 * IL
  # lc3 <- 1.0 * DL + 1.0 * SF + 0.7* LL
  # lc4 <- 1.0 * DL + 1.0 * SF + 0.6 * LL + 0.5 * TL

  # Accidental Impact Load (ALS) Ned_ALS (kN)
  Ned_ALS <- max( lc1, lc2 )

  Ned <- round( max(Ned_ULS, Ned_ALS) )

  return(Ned)
}

# axial_compression_force_Ned( DL=1, LL=1, L=12.7, AF=582, theta=90, spacing=7,
#                             Lcry=12.7, Lcrz=12.7, steel_grade='S355', member_type='UB',
#                             alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, IL=50 )


# trial member size [ height (mm) x width (mm) x mass (kg/m) ]
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
trial_member_size <- function(Lcry, Lcrz, Ned, steel_grade, member_type) {
  require(readxl)
  # critical_length_major_axis_Lcry (m)
  # critical_length_minor_axis_Lcrz (m)
  # axial_compression_force_Ned (kN)
  # steel grade, steel_grade (N/mm2) - categorical: S355/S275
  # member type, member_type (categorical: UC/UB)

  # Lcry=13.2; Lcrz=1.9; Ned=9250; steel_grade="S275"; member_type="UC"

  if ( steel_grade == "S355" ) {
    if ( member_type == "UC" ) {
      axial_compression_table_name <- "./s355/UC/UC-compression-S355.xlsx"
      nmax <- 138
    } else if ( member_type == "UB" ) {
      axial_compression_table_name <- "./s355/UB/UB-Axial compression-S355.xlsx"
      nmax <- 321
    } else {
      print("member type unknown. please enter valid one.")
    }
  } else if ( steel_grade == "S275" ) {
    if ( member_type == "UC" ) {
      axial_compression_table_name <- "./s275/UC/UC-compression-S275.xlsx"
      nmax <- 138
    } else if ( member_type == "UB" ) {
      axial_compression_table_name <- "./s275/UB/UB_Axial compression-S275.xlsx"
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
                          skip = 5
  )

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


  trial_member_size = trial_member_size_Lcry

  return(trial_member_size)
}
# trial_member_size(Lcry=12.7, Lcrz=12.7, Ned=4926, steel_grade="S275", member_type="UC")


# Check 1: overall buckling resistance of struts about y-y axis
# Output: N_b_Rd_X, overall_buckling_resistance_about_yy_axis (kN)
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
check_overall_buckling_resistance_about_yy_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {

  trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

  s <- member_size_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- fy(l$tw, l$tf, steel_grade)

  # Le, effective length of strut (mm)
  Le <- effective_length_of_strut(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, l$Iyy) / 1000

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd * 2, Ncr)

  # imperfection_factor_alpha_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_alpha_yy(h, b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_X <- round( overall_buckling_resistance_about_axis(X, N_pl_Rd) )

  return(N_b_Rd_X)
}


# Check 2: overall buckling resistance of struts about z-z axis
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
check_overall_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {
  # 2: \alpha_zz, I=Ieff [mm4], Le=kL[m], Npl,Rk=Npl,Rd*2 [KN]
  # \lambda= \lambda_Y,  X=X_y, \Phi=\Phi_y
  #
  # OUTPUT 2 : N_{b,Rd}=N_{b,Rd,y} [KN]

  trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

  s <- member_size_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- fy(l$tw, l$tf, steel_grade)

  # Le, effective length of strut (mm)
  Le <- effective_length_of_strut(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, l$Iyy) / 1000

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd * 2, Ncr)

  # imperfection_factor_alpha_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_alpha_yy(h, b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_X <- overall_buckling_resistance_about_axis(X, N_pl_Rd)

  return(N_b_Rd_X)
}


# Check 3: local buckling resistance of strut about z-z axis
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
check_local_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {
  # 3: \alpha_zz, I=Ieff[mm4], Le=Lch [m], Npl,Rch=Npl,Rd [KN]
  #
  # OUTPUT 3 :
  #   take min OUTPUT 1, OUTPUT 2, OUTPUT 3

  trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

  s <- member_size_string_to_elements(trial_member_size)

  # extract member area from reference table
  l <- member_dimensions(s$h, s$b , s$m, member_type)

  # fy, yield strength [N/mm2]
  fy <- fy(l$tw, l$tf, steel_grade)

  # Le, effective length of strut (mm)
  Le <- effective_length_of_strut(k, L)

  # N_pl_Rd, plastic resistance of the cross-section to compression (kN)
  N_pl_Rd <- plastic_resistance_of_cross_section_to_compression(l$A, fy) / 1000

  # Ncr, Euler buckling load (kN)
  Ncr <- Euler_buckling_load(Le, E, l$Iyy) / 1000

  # lambda_bar, Relative slenderness (dimentionless)
  lambda_bar <- relative_slenderness(N_pl_Rd * 2, Ncr)

  # imperfection_factor_alpha_yy for rolled section (dimensionless)
  alpha_yy <- imperfection_factor_alpha_yy(h, b, l$tf)

  # X, slenderness reduction factor (dimentionless)
  X <- slenderness_reduction_factor(alpha_yy, lambda_bar)

  # N_b_Rd, overall buckling resistance of the struts about the axis (kN)
  N_b_Rd_X <- overall_buckling_resistance_about_axis(X, N_pl_Rd)

  return(N_b_Rd_X)
}


# high-level flow
#' critical_length_major_axis_Lcry (m)
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param x A description of the parameter 'x'. The
#'   description can span multiple lines.
#' @param y A description of the parameter 'y'.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#' @return Lcrz critical_length_major_axis_Lcrz [m]
#'
main <- function() {

  # user inputs
  DL=1
  LL=1
  L=12.7
  AF=582
  theta=90
  spacing=7
  Lcry=12.7
  Lcrz=12.7
  steel_grade='S355'
  member_type='UB'
  alpha_T=0.000012
  delta_T=10
  k_T=0.8
  E=210
  IL=50

  # calculate Ned (kN)
  Ned <- axial_compression_force_Ned(DL, LL, L, AF, theta, spacing, Lcry, Lcrz, steel_grade, member_type, alpha_T, delta_T, k_T, E, IL)

  # determine member size
  member_size <- trial_member_size(Lcry, Lcrz, Ned, steel_grade, member_type)

  # apply 1st check
  k=0.8
  check1 <- check_overall_buckling_resistance_about_yy_axis(member_size, member_type, steel_grade, k, L, E)

  # display results
  print( paste0('Ned = ', Ned, ' kN') )
  print( paste0('Selected trial member size: ', trial_mb) )
  print( paste0('check #1 = ', check1) )
}

