#
#  Water Vapor Flux as calculated in SURFEX
#
#


# q_sat(T_s) : Saturated specific humidity at temperature T_s
# q_a        : Spesific humidity at lowest atmospheric level
# delta_i    : Surface ice fraction
               #w_gf/(w_gf + w_g) 
# h_u        : Relative humidity at surface

# h_ui       : Relative humidity for ice covered portion of surface


# Evaporation from soil water
E_gl <- (1-veg)*(1-P_sng)*(1-delta_i)*rho_a*C_H*V_a*(h_u*q_sat(T_s)-q_a)

# Sublimation from soil ice
E_gf <- (1-veg)*(1-P_sng)*delta_i*rho_a*C_H*V_a*(h_ui*q_sat(T_s)-q_a)


# evaporation sublimation ratio

E_rat <- 

