usethis::use_package("devtools")
usethis::use_package("roxygen2")
usethis::use_package("readxl")

require(devtools)
require(roxygen2)
# usethis::use_vignette("civilR")
# roxygen2::roxygenise()
# devtools::build_manual()
# system("R CMD Rd2pdf .")

closest_critical_length_index
#' Function that find closest critical length by rounding input.


member_dimensions
#' Function that look into the 'Dimensions and properties' table for chosen member type from the Blue Book and pool dimentions Area of section,  A (cm2), Thickness web,  tw (mm), Thickness flange tf, (mm), Second moment of area Axis y-y, I_yy, (cm4), Depth of section,  h (mm), `Width of section, b (mm)


member_size_to_string
#' Generate a combined string from given tree number separeted by"x"

imperfection_factor_alpha_yy
#' Function calculate Imperfection factor for bucking about major axis
\eqn{\alpha_yy}
#' Function calculate Imperfection factor based on the bucling curve, which depends on dementions of the section. For rolled section.


imperfection_factor_alpha_zz
#' Function calculate Imperfection factor for bucking about manor axis
\eqn{\alpha_zz}
#' Function calculate Imperfection factor based on the buckling curve, which depends on dimentions of the Rolled section.



fy
#' Function calculate yield strength for specified steel grade
#' Function calculate  yield strength for specified steel grade, (N/mm20 or (MPa) based on Nominal thickness.



check_overall_buckling_resistance_about_yy_axis
#'Function check overall buckling resistance about major axis.

#'Function check overall buckling resistance of member about major axis y-y based on EC3 Approach.
\deqn{Le=kL},(mm).L- critical length for buckling about major axis
#'Steps of the check performed for laced struts:
#' 1. Plastic resistance of the cross-section to compression, (kN)
\deqn{N_{pl,Rd}= 2(Afy)}
#' 2. The Euler buckling load, (kN)
\deqn{N_{cr, X}=\frac{\pi^2EI}{L_{e}^2}}
#' 3. Relative slenderness, non-dimensional
\deqn{\bar{\lambda_X}=\sqrt\frac{N_{pl,Rd}}{N_{cr, X}}}
#' 4. Calculation \eqn{\Phi_X} parameter for slenderness reduction factor
\deqn{\Phi_X=o.5[1+\alpha(\bar{\lambda_X}-0.2)+\bar{\lambda_X}^2}
#' 5. Slenderness reduction factor, non-dimensional
\deqn{X_x=\frac{1}{\Phi_X+\sqrt{\Phi_X^2-\bar{\lambda_X}^2}}}
#' 6. Output Overall buckling resistance of the struts about y-y axis,(kN)
\deqn{N_{b,Rd,X}=\frac{X_x N_{pl,Rd}}}

#' The partial factors $γM$ that are applied to Resistance of members to instability:	$γ{M1}$ = 1.00



check_local_buckling_resistance_about_zz_axis
#'Function check overall buckling resistance about major axis.

#'Function check overall buckling resistance of member about minor axis z-z. Based on EC3 Approach.
\deqn{Le=kL},(mm).L- critical length for global buckling about minor axis

The effective second moment of area
\deqn{I_{eff}=0.5h0^2A
#'Steps of the check performed for laced struts:
#' 1. Plastic resistance of the cross-section to compression, (kN)
\deqn{N_{pl,Rd}= 2(Afy)}
#' 2. The Euler buckling load, (kN)
\deqn{N_{cr, Y}=\frac{\pi^2EI_{eff}}{L_{e}^2}}
#' 3. Relative slenderness, non-dimensional
\deqn{\bar{\lambda_Y}=\sqrt\frac{N_{pl,Rd}}{N_{cr, Y}}}
#' 4. Calculation \eqn{\Phi_X} parameter for slenderness reduction factor
\deqn{\Phi_Y=o.5[1+\alpha(\bar{\lambda_Y}-0.2)+\bar{\lambda_Y}^2}
#' 5. Slenderness reduction factor, non-dimensional
\deqn{X_Y=\frac{1}{\Phi_Y+\sqrt{\Phi_Y^2-\bar{\lambda_Y}^2}}}
#' 6. Output Overall buckling resistance of the struts about z-z axis,(kN)
\deqn{ N_{bRdY}=X_Y N_{pl,Rd}}}
#' The partial factors $γM$ that are applied to Resistance of members to instability: \eqn{γ{M1}= 1.00}




check_local_buckling_resistance_about_zz_axis
#'Function check lolocal buckling resistance of struts about minor axis
#'Function Check local buckling resistance of struts about minor axis, z-z. Based on EC3 Approach.

#'Steps of the check performed for laced struts:
#' 1. Plastic resistance of the cross-section to compression, (kN)
\deqn{N_{plRdch}=Afy}
#' 2. The Euler buckling load, (kN)
\deqn{N_{cr,ch}=\frac{\pi^2EI}{L_{e}^2}}
#' 3. Relative slenderness, non-dimensional
\deqn{\bar{\lambda_{ch}}=\sqrt\frac{N_{pl,Rd,ch}}{N_{cr, ch}}}
#' 4. Calculation \eqn{\Phi_X} parameter for slenderness reduction factor
\deqn{\Phi_{ch}=o.5[1+\alpha(\bar{\lambda_{ch}}-0.2)+\bar{\lambda_{ch}}^2}
#' 5. Slenderness reduction factor, non-dimensional
\deqn{X_{ch}=\frac{1}{\Phi_{ch}+\sqrt{\Phi_{ch}^2-\bar{\lambda_{ch}}^2}}}
#' 6. Output local buckling resistance of the struts about z-z axis,(kN)
\deqn{ N_{bRdch}=X_{ch}N_{plRdch}}
#' The partial factors $γ_M$ that are applied to Resistance of members to instability:
\eqn{γ_{M1}= 1.00}




effective_second_moment_of_area
#' Compute effective second moment of area



#' The effective second moment of area,  (mm^4) is a function of the distance between the centroids of the chords and the section area of a chord.
\eqn{I_{eff}=0.5h0^2A.





plastic_resistance_of_cross_section_to_compression
#' Function calculate plastic resistance

#' Function calculate plastic resistance of cross section, (kN) to compression based on cross-section area, A  and  yield strength, fy.






Euler_buckling_load
#'Function calculate the Euler buckling load, (kN)
\deqn{N_{cr,ch}=\frac{\pi^2EI}{L_{e}^2}}




relative_slenderness
#' Function compute, slenderness,   non-dimensional
\deqn{\lambda}=\sqrt\frac{N_{pl,Rd}}{N_{cr}}




slenderness_reduction_factor
#'Function compute slenderness reduction factor for general case
\deqn{\Phi=o.5[1+\alpha(\bar{\lambda}-0.2)+\bar{\lambda}^2}
\deqn{X=\frac{1}{\Phi+\sqrt{\Phi^2-\bar{\lambda}^2}}}

overall_buckling_resistance_about_axis
#' General case to compute overall buckling resistance of the struts about the axis
\deqn{N_{b,Rd}=X N_{pl,Rd}}


shear_stiffness
#'Shear stiffness calculation.

#'Function compute, Shear stiffness. Case of K-shaped arrangement of lacings, (kN).
#'The expression of shear stiffness is:
\deqn{S_v= \frac{nEA_dL{ch}h_0^2}{d^3}}

second_order_moment
#'Function compute Second order moment

#'Function compute Second order bending moment. The maximum bending moment, including the bow imperfection and the second order effects
\deqn{M_{Ed}=\frac{N_{Ed}e_0+M_{Ed}^I} {1-{\frac{N_{Ed}}{N_{crY}}}-{\frac{N_{Ed}}{S_v}}}}

maximum_shear_force_in_the_lacing
#' Function compute maximum shear force in the lacing
#' For a laced strut subject to a compressive axial force only:
\deqn{V_{Ed}=\pi\frac{M_{Ed}}{L}}


Maximum_compression_axial_force
#' Function compute maximum compression axial force in diagonals
#' Maximum compression axial force
#' The expression of the compression axial force Nd,Ed in a diagonal is derived from the shear force \deqn{nN_{Ed} \leq\frac{V_{Ed}d}{nh_0}}






#' Calculate the critical length along major \eqn{y} axis
#'
#' This is the description.
#'
#' @param L Total length of member [m]
#' @param Lkp Length to king post [m]
#' @param Lsp Length from splays [m]
#'
#' @export
#'
#' @return \eqn{L_{cry}} critical_length_major_axis_Lcry [m]
#'
critical_length_major_axis_Lcry <- function (L, Lkp, Lsp) {
  Lkp <- L / 2
  Lcry <- Lkp - Lsp
  return(Lcry)
}


#' Calculate the critical length along major \eqn{z} axis
#'
#' This is the description.
#'
#' @param L Total length of member [m]
#' @param Lkp Length to king post [m]
#' @param Lsp Length from splays [m]
#'
#' @export
#'
#' @return \eqn{L_{crz}} critical_length_major_axis_Lcrz [m]
#'
critical_length_minor_axis_Lcrz <- function (L, Lkp, Lsp) {
  Lcrz <- 0 # TBC
  return( Lcrz )
}


#' Calculate the temperature load
#'
#' Calculate Temperature Load as a function of a surface changes of temperature.
#' Usually used for calculation of Axial Compression Force for the top level member. \deqn{TL = \alpha_T \, \delta_T \, k_T  \, E \, A}
#'
#' @param alpha_T Thermal coefficient of expansion [degC]
#' @param delta_T Change in temperature from the Installation temperature [degC]
#' @param k_T Coefficient Of temperature effect [dimensionless]
#' @param E Young's Modulus of Elasticity [GPa]
#' @param A Sectional area from table for given member size [cm2]
#'
#' @export
#'
#' @return \eqn{TL} Temperature load [kN]
#'
calculate_TL <- function(alpha_T=0.000012, delta_T=10, k_T=0.8, E=210, A) {
  TL <- alpha_T * delta_T * k_T * E * A
  return(TL)
}


#' Extract dimensions from reference table
#'
#' Function that looks into the Blue Book \url{https://www.steelforlifebluebook.co.uk/} for dimensions and properties.
#'
#' @param h Member height [mm]
#' @param b Member width [mm]
#' @param m Member mass [kg/m]
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
#'   \item \eqn{A} Area of section [cm2]
#'   \item \eqn{tw} Thickness of web [mm]
#'   \item \eqn{tf} Thickness of flange [mm]
#'   \item \eqn{Iyy} Second moment of area axis y-y [cm4]
#'   \item \eqn{sh} Depth of section [mm]
#'   \item \eqn{sb} Width of section [mm]
#' }
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


#' Convert the member size individual dimensions to a standard string
#'
#' Generate a combined string from given three individual elements, separated by "x"
#'
#' @param h Member height [mm]
#' @param b Member width [mm]
#' @param m Member mass [kg/m]
#'
#' @export
#'
#' @examples
#' member_size_to_string(h=610, b=229, m=140)
#'
#' @return String of the member dimensions
#'
member_size_to_string <- function(h, b, m) {
  return( paste( h, b, m, sep=' x ' ) )
}


#' Convert individual member dimensions to a string
#'
#' This is the description.
#'
#' @param s String of the member dimensions
#'
#' @export
#'
#' @examples
#' member_size_string_to_elements("610 x 229 x 140")
#'
#' @return
#' \itemize{
#'   \item \eqn{h} Member height [mm]
#'   \item \eqn{b} Member width [mm]
#'   \item \eqn{m} Member weight [kg]
#' }
#'
member_size_string_to_elements <- function(s) {
  v <- as.numeric( unlist(strsplit(s, " x ")) )
  h <- v[1]
  b <- v[2]
  m <- v[3]
  return( list("h" = h, "b" = b, "m" = m) )
}


#' Calculate the imperfection factor \eqn{\alpha_{yy}} for rolled section [dimensionless]
#'
#' @param h Member height [mm]
#' @param b Member width [mm]
#' @param tf thickness of the flange [mm]
#'
#' @export
#'
#' @return \eqn{\alpha_{yy}} Imperfection factor for y-y axis
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


#' Calculate the imperfection factor \eqn{\alpha_{zz}} for rolled section
#'
#' @param h Member height [mm]
#' @param b Member width [mm]
#' @param tf thickness of the flange [mm]
#'
#' @export
#'
#' @return \eqn{\alpha_{zz}} Imperfection factor for z-z axis
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


#' Calculate the yield strength
#'
#' @param tw Thickness of the web [mm]
#' @param tf Thickness of the flange [mm]
#' @param steel_grade steel_grade [\eqn{N/mm^2}] - categorical: S355/S275
#'
#' @export
#'
#' @examples
#' fy(tw=47.6, tf=77, steel_grade="S355")
#'
#' @return \eqn{f_y} Yield strength [N/mm2]
#'
fy <- function(tw, tf, steel_grade) {
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

  return( fy )
}


#' Calculate the effective length of strut
#'
#' This is the description.
#'
#' @param k Effective lengh coefficient [dimensionless]
#' @param L Length of strut between restraints [mm]
#'
#' @export
#'
#' @return \eqn{L_e} Effective length of strut [mm]
#'
effective_length_of_strut <- function(k, L) {
  return( k * L )
}


#' Calculate the effective second moment of area
#'
#' This is the description.
#'
#' @param h0 Distance between centroids of chords [m]
#' @param A Cross-section area of strut [cm2]
#'
#' @export
#'
#' @return \eqn{I_{eff}} Effective second moment of area [\eqn{mm^4}]
#'
effective_second_moment_of_area <- function(h0, A) {
  return( 0.5 * h0^2 * A * 100 )
}


#' Calculate the plastic resistance of the cross-section to compression
#'
#' This is the description.
#'
#' @param A Cross-section area of the strut [\eqn{cm^2}]
#' @param fy Yield strength [\eqn{kN/{mm}^2}]
#'
#' @export
#'
#' @return \eqn{N_{pl,R_d}} Plastic resistance of the cross-section to compression [kN]
#'
plastic_resistance_of_cross_section_to_compression <- function(A, fy) {
  return( fy * A * 100 )
}


#' Calculate the Euler buckling load
#'
#' This is the description.
#'
#' @param Le Effective length of strut [mm]
#' @param E Young modulus [\eqn{GPa} / \eqn{GN/m^2}]
#' @param I - check 1: \eqn{I_{yy}}, second moment of area Axis y-y [\eqn{cm^4}]. Check 2: \eqn{I_{eff}}, Effective second moment of area [\eqn{mm4}]. Check 3: \eqn{I_{eff}} or \eqn{I_{zz}} [\eqn{mm^4}]
#'
#' @export
#'
#' @return \eqn{N_{cr}} Euler buckling load [kN]
#'
Euler_buckling_load <- function(Le, E, I) {
  Ncr <- ( pi^2 * E * I ) / Le^2
  return( Ncr )
}


#' Calculate the relative slenderness
#'
#' This is the description.
#'
#' @param N_pl_Rd Plastic resistance of the cross-section to compression [kN]
#' @param Ncr Euler buckling load [kN]
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
#' This is the description.
#'
#' @param alpha - check 1: imperfection_factor_alpha_yy for rolled section [dimentionless]. Check 2 & 3: imperfection_factor_alpha_zz for rolled section [dimentionless]
#' @param lambda_bar, Relative slenderness [dimentionless]
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


#' Calculate the overall buckling resistance of the struts about the axis
#'
#' This is the description.
#'
#' @param X Slenderness reduction factor [dimentionless]
#' @param N_pl_Rd Plastic resistance of the cross-section to compression [kN]
#'
#' @export
#'
#' @return \eqn{N_{b,R_d}} Overall buckling resistance of the struts about the axis [kN]
#'
overall_buckling_resistance_about_axis <- function(X, N_pl_Rd) {
  return( X * N_pl_Rd )
}


#' Calculate the shear stiffness for K-shape lacing
#'
#' This is the description.
#'
#' @param n Number of planes of lacing, default [\eqn{n=2}]
#' @param Ad Section area of diagonal (lacing), [\eqn{cm^2}]
#' @param Lch Length of chord of betwen restrains (lace points) [m]
#' @param E Young modulus [\eqn{GPa} / \eqn{GN/m^2}]
#' @param h0 Distance between centroids of chords [m]
#'
#' @export
#'
#' @return \eqn{S_v} Shear stiffness for K-shape lacing
#'
shear_stiffness <- function(n=2, Ad, Lch, E, h0) {
  d <- sqrt( h0^2 + Lch^2 )  # length of diagonal
  Sv <- ( n * E * Ad * Lch * h0^2 ) / d^3
  return(Sv)
}


#' Calculate the second order moment
#'
#' This is the description.
#'
#' @param L Length of strut between restraints [mm]
#' @param Ned Axial_compression_force_Ned [kN]
#' @param Sv Shear stiffness for K-shape lacing
#' @param Ncr Euler buckling load from check #2 global zz [kN]
#'
#' @export
#'
#' @return \eqn{M_{E_d}} Second order moment [kN.m]
#'
second_order_moment <- function(L, Ned, Sv, Ncr) {
  e0 <- L / 500 # e0, initial bow imperfection
  MEd_1 <- 0 # first order moment
  MEd <- ( Ned * e0 + MEd_1 ) / ( 1 - (Ned / Ncr) - (Ned / Sv))
  return(MEd)
}


#' Generate calculated \eqn{N_{E_d}}
#'
#' This is the description.
#'
#'
#' @param N_b_Rd Overall buckling resistance of the struts about the axis [\eqn{kN}]
#' @param Ieff Effective second moment of area [\eqn{mm^4}]
#' @param MEd Second order moment [\eqn{kN.m}]
#' @param h0 Distance between centroids of chords [\eqn{m}]
#' @param A Cross-section area of strut [\eqn{cm^2}]
#'
#' @export
#'
#' @return \eqn{N_{{E_d}_c}} Calculated \eqn{N_{E_d}} [kN]
#'
calculated_NEd <- function(N_b_Rd, Ieff, MEd, h0, A) {
  return( 2 * ( N_b_Rd - (MEd*h0*A)/(2*Ieff) ) )
}


#' Calculate the maximum shear force in the lacing (for a laced strut subject to a compressive axial force only)
#'
#' This is the description.
#'
#' @param MEd Second order moment [kN.m]
#' @param L Length of strut between restraints [mm]
#'
#' @export
#'
#' @return \eqn{V_{E_d}} Maximum shear force in the lacing (for a laced strut subject to a compressive axial force only)
#'
maximum_shear_force_in_the_lacing <- function(MEd, L) {
  return( pi * MEd / L )
}


#' Calculate the axial compression force
#'
#' Compute Axial Compression Force, \eqn{N_{ed}} [kN], for member without including Temperature effect.
#' Used as trial for the top level strut where temperature changes could not be neglected.
#' As well can be used to calculate final \eqn{N_{ed}} for struts from low levels of excavation, where temperature effect could be neglected.
#'
#' First of all function check which combination govern in ULS (Ultimate Limit State) without including Temperature load, TL [kN].
#' Then include TL calculations for Load Combinations applying partial factors based on the Table A1.2(B), EN1990-2002, p53
#' Compare maximum from ULS and ALS to define which mistake could govern.
#'
#' @param DL Dead load / self-weight of member [kN/m]
#' @param LL Live load / imposed load [kN/m]
#' @param L Total length of member [m]
#' @param AF Axial compression force of member per meter [kN/m]
#' @param theta Angle to wall [deg]
#' @param spacing spacing [m]
#' @param Lcry critical length major axis [m]
#' @param Lcrz critical length minor axis [m]
#' @param steel_grade steel_grade [\eqn{N/mm^2}] - categorical: S355/S275
#' @param member_type member_type (categorical: UC/UB)
#' @param alpha_T Thermal coef. of expansion [degC]
#' @param delta_T Change in temperature from the Installation temperature [degC]
#' @param k_T Coefficient Of Temperature Effect [dimensionless]
#' @param E Young's Modulus of Elasticity [GPa]
#' @param IL Accidental Impact Load [kN/m]
#'
#' @export
#'
#' @return \eqn{N_{ed}} Axial compression force [kN]
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
  # axial_compression_force_Ned( DL=1, LL=1, L=12.7, AF=582, theta=90, spacing=7,
  # Lcry=12.7, Lcrz=12.7, steel_grade='S355', member_type='UB', alpha_T=0.000012,
  # delta_T=10, k_T=0.8, E=210, IL=50 )

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


#' Determine member size [ height (mm) x width (mm) x mass (kg/m) ]
#'
#' Find optimized designation (or can call it member size) for given Axial Compression Force and critical length for major and minor axis.
#' Searching into the tables based on the 'Compression' tables of the Blue Book \url{https://www.steelforlifebluebook.co.uk/}
#'
#' @param Lcry critical length major axis [m]
#' @param Lcrz critical length minor axis [m]
#' @param Ned Axial compression force [kN]
#' @param steel_grade steel_grade [\eqn{N/mm^2}] - categorical: S355/S275
#' @param member_type member_type (categorical: UC/UB)
#'
#' @export
#'
#' @examples
#' trial_member_size(Lcry=12.7, Lcrz=12.7, Ned=4926, steel_grade="S275", member_type="UC")
#'
#' @return Member size [ height (mm) x width (mm) x mass (kg/m) ]
#'
trial_member_size <- function(Lcry, Lcrz, Ned, steel_grade, member_type) {
  require(readxl)

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

  trial_member_size <- trial_member_size_Lcry
  return(trial_member_size)
}


#' Perform check #1, calculating the overall buckling resistance of member about \eqn{y-y} axis
#'
#' Calculate the overall buckling resistance of member about \eqn{y-y} axis, based on EC3 Approach. \deqn{L_e=kL} [mm]
#' where \eqn{L} is the critical length for buckling about major axis \eqn{y-y}
#' Steps of the check performed for laced struts:
#' \enumerate{
#'   \item Plastic resistance of the cross-section to compression [kN] \deqn{N_{pl,R_d}= 2(A   fy)}
#'   \item The Euler buckling load [kN] \deqn{N_{cr,X}=\frac{\pi^2   E   I}{{L_e}^2}}
#'   \item Relative slenderness [dimensionless] \deqn{ \bar{\lambda_X} = \sqrt{ \frac{N_{pl,R_d}}{N_{cr,X}} } }
#'   \item Calculate \eqn{\Phi_X} parameter for slenderness reduction factor \deqn{ \Phi_X = 0.5   \left[ 1 + \alpha \left( \bar{\lambda_X}-0.2 \right) + {\bar{\lambda_X}}^2 \right] }
#'   \item Slenderness reduction factor [dimensionless] \deqn{ X_x = \frac{1}{ \Phi_X+\sqrt{{\Phi_X}^2-{\bar{\lambda_X}}^2} } }
#'   \item Output overall buckling resistance of the struts about \eqn{y-y} axis [kN] \deqn{ N_{b,R_d,X}=X_x   N_{pl,R_d} }
#' }
#' The partial factors \eqn{\gamma_M} that are applied to resistance of members to instability:	\eqn{\gamma_{M_1} = 1}
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type (categorical: UC/UB)
#' @param steel_grade steel_grade [\eqn{N/mm^2}] - categorical: S355/S275
#' @param k Coefficient [dimensionless]
#' @param L Total length of member [m]
#' @param E Young's Modulus of Elasticity [GPa]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,X}} Overall buckling resistance of struts about y-y axis [kN]
#'
check_overall_buckling_resistance_about_yy_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {

  # trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=1; L=12.7; E=210

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


#' Perform check #2, calculating the overall buckling resistance of struts about z-z axis
#'
#' This is the description.
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type (categorical: UC/UB)
#' @param steel_grade steel_grade [\eqn{N/mm^2}] - categorical: S355/S275
#' @param k Coefficient [dimensionless]
#' @param L Total length of member [m]
#' @param E Young's Modulus of Elasticity [GPa]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,y}} Overall buckling resistance of struts about z-z axis [kN]
#'
check_overall_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {
  # 2: \alpha_zz, I=Ieff [mm4], Le=kL[m], Npl,Rk=Npl,Rd*2 [KN]
  # \lambda= \lambda_Y,  X=X_y, \Phi=\Phi_y
  #
  # OUTPUT 2 : N_{b,Rd}=N_{b,Rd,y} [KN]

  # trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

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
  N_b_Rd_y <- overall_buckling_resistance_about_axis(X, N_pl_Rd)

  return(N_b_Rd_y)
}


#' Perform check #3, calculating the local buckling resistance of struts about z-z axis
#'
#' This is the description.
#'
#' @param trial_member_size Trial member size
#' @param member_type member_type (categorical: UC/UB)
#' @param steel_grade steel_grade [\eqn{N/mm^2}] - categorical: S355/S275
#' @param k Coefficient [dimensionless]
#' @param L Total length of member [m]
#' @param E Young's Modulus of Elasticity [GPa]
#'
#' @export
#'
#' @return \eqn{N_{b,Rd,X}} Local buckling resistance of struts about z-z axis [kN]
#'
check_local_buckling_resistance_about_zz_axis <- function(trial_member_size, member_type, steel_grade, k, L, E) {
  # 3: \alpha_zz, I=Ieff[mm4], Le=Lch [m], Npl,Rch=Npl,Rd [KN]
  #
  # OUTPUT 3 :
  #   take min OUTPUT 1, OUTPUT 2, OUTPUT 3

  # trial_member_size=trial_mb; member_type="UB"; steel_grade="S355"; k=0.8; L=12.7; E=210

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


#' Run the high-level complete flow
#'
#' This is the description.
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

  # calculate Ned (kN)
  Ned <- axial_compression_force_Ned(DL, LL, L, AF, theta, spacing, Lcry, Lcrz, steel_grade, member_type, alpha_T, delta_T, k_T, E, IL)

  # determine member size
  member_size <- trial_member_size(Lcry, Lcrz, Ned, steel_grade, member_type)

  # apply 1st check
  check1 <- check_overall_buckling_resistance_about_yy_axis(member_size, member_type, steel_grade, k, L, E)

  # display results
  print( paste0('Ned = ', Ned, ' kN') )
  print( paste0('Selected trial member size: ', trial_mb) )
  print( paste0('check #1 = ', check1) )
}

