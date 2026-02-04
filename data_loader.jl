using JuMP, XLSX, CSV, DataFrames
# This file loads all data from the Excel file and defines parameters for the optimization model.

#--------------------------------------------------------------

# Parameters
# Load the Excel file
xlsx_file = XLSX.readxlsx("../DATA/DATA_MAJOR.xlsx")

# Import data w/ 4h-block resolution
LF_solar = xlsx_file["SOLAR"]["B2:G2191"] # EERA
LF_onwind = xlsx_file["ONWIND"]["B2:G2191"] # EERA
LF_offwind = xlsx_file["OFFWIND"]["B2:G2191"] # EERA
hydro_ror = xlsx_file["ROR"]["C2:C2191"] # historical run-of-river generation in 4h blocs
LOAD = xlsx_file["LOAD"]["B2:G2191"]/1000 # EERA
DEM_H2 = xlsx_file["DEMAND"]["C7"]

# COSTS
gas_cost = 40 # €/MWh, current gas price in Europe
biogas_cost = 80 # €/MWh Engie. (2022). Geographical analysis of biomethane potential and costs in Europe in 2050. Engie.
carbon_price = 500 # €/tCO2, implicit carbon price for deep decarbonization, see Quinet report.
#STOCK_BIO = 30e3 # GWh, Agora energiwende. (2022). AGORA Energiwende 2035. Agora Energiwende.
EMIS_GAS = 0.2 #t/MWh PCS
eff_CCGT = 0.6
eff_OCGT = 0.4
eff_BIO = 0.6
eff_SMR = 0.7 # efficiency of steam methane reforming
eff_BESS = 0.9 
eff_PHS = 0.8
eff_UHS = 1
eff_PtG = 0.7 # Pietzcker
eff_H2T = 0.6 # Pietzcker

CAPA_BIO = 6 # GW, maximum capacity of biogas from Agora Energiewende 2035
CAPA_PHS_P = 7*0.54 # derating factor from Manuel Villavicencio's thesis
CAPA_PHS_E = 7*CAPA_PHS_P*0.54

discount_rate = 0.05

OPEX_CCGT = 0.03 # Pietzcker
OPEX_OCGT = 0.03 # Pietzcker
OPEX_SMR = 0.05 # Fasihi
OPEX_PV = 0.01 # Pietzcker
OPEX_ONWIND = 0.03 # Pietzcker
OPEX_OFFWIND = 0.03 # Pietzcker
OPEX_BESS = 0.01 # Pietzcker, R. C., Osorio, S., & Rodrigues, R. (2021). Tightening EU ETS targets in line with the European Green Deal : Impacts on the decarbonization of the EU power sector. Applied Energy, 293, 116914. https://doi.org/10.1016/j.apenergy.2021.116914
OPEX_H2 = 0.03 # from Fasihi, M., Weiss, R., Savolainen, J., & Breyer, C. (2021).  
OPEX_UHS = 0.04 # from Fasihi, M., Weiss, R., Savolainen, J., & Breyer, C. (2021)

lifetime_CCGT = 45 # Pietzcker, R. C., Osorio, S., & Rodrigues, R. (2021). Tightening EU ETS targets in line with the European Green Deal : Impacts on the decarbonization of the EU power sector. Applied Energy, 293, 116914. https://doi.org/10.1016/j.apenergy.2021.116914
lifetime_OCGT = 45
lifetime_SMR = 30 # Fasihi
lifetime_BIO = 45
lifetime_PV = 25
lifetime_ONWIND = 25
lifetime_OFFWIND = 25
lifetime_BESS = 20 
lifetime_UHS = 40
lifetime_PtG = 20
lifetime_H2T = 40

# COST PARAMETERS €/kW
inv_cost_CCGT = 900 # Pietzcker, R. C., Osorio, S., & Rodrigues, R. (2021). Tightening EU ETS targets in line with the European Green Deal : Impacts on the decarbonization of the EU power sector. Applied Energy, 293, 116914. https://doi.org/10.1016/j.apenergy.2021.116914
inv_cost_OCGT = 400 # Pietzcker
inv_cost_SMR = 500 # Fasihi
inv_cost_UHS = 5 # €/kWh lined rock/salt cavern, from Fasihi, M., Weiss, R., Savolainen, J., & Breyer, C. (2021). Global potential of green ammonia based on hybrid PV-wind power plants. Applied Energy, 294, 116170. https://doi.org/10.1016/j.apenergy.2020.116170
inv_cost_PtG = 1000 # Agora energiwende. (2024). EU Map of hydrogen production costs says 1500 in 2023 and 600 in 2030. We cut in half
inv_cost_H2T = 900 # same as CCGTs
inv_cost_PV = 593 # IRENA 2024
inv_cost_ONWIND = 1670 # IRENA 2024
inv_cost_OFFWIND = 3000 # IRENA 2024
inv_cost_BESS_E = 200 # IRENA 2024: cost of 250€/kWh for 4h storage duration, 300€/kWh for 2h, we solve the system to determine cost of storage and power, we find 200€/kW and per kWh also
inv_cost_BESS_P = 200 



# annualized costs in k€/GW
annualized_cost_CCGT = 1e3*(inv_cost_CCGT*discount_rate/(1-(1+discount_rate)^(-lifetime_CCGT)) + OPEX_CCGT*inv_cost_CCGT)
annualized_cost_OCGT = 1e3*(inv_cost_OCGT*discount_rate/(1-(1+discount_rate)^(-lifetime_OCGT)) + OPEX_OCGT*inv_cost_OCGT)
annualized_cost_SMR = 1e3*(inv_cost_SMR*discount_rate/(1-(1+discount_rate)^(-lifetime_SMR)) + OPEX_SMR*inv_cost_SMR)
annualized_cost_PV = 1e3*(inv_cost_PV*discount_rate/(1-(1+discount_rate)^(-lifetime_PV)) + OPEX_PV*inv_cost_PV)
annualized_cost_ONWIND = 1e3*(inv_cost_ONWIND*discount_rate/(1-(1+discount_rate)^(-lifetime_ONWIND)) + OPEX_ONWIND*inv_cost_ONWIND)
annualized_cost_OFFWIND = 1e3*(inv_cost_OFFWIND*discount_rate/(1-(1+discount_rate)^(-lifetime_OFFWIND)) + OPEX_OFFWIND*inv_cost_OFFWIND)
annualized_cost_BESS_E = 1e3*(inv_cost_BESS_E*discount_rate/(1-(1+discount_rate)^(-lifetime_BESS)) + OPEX_BESS*inv_cost_BESS_E)
annualized_cost_BESS_P = 1e3*(inv_cost_BESS_P*discount_rate/(1-(1+discount_rate)^(-lifetime_BESS)) + OPEX_BESS*inv_cost_BESS_P)
annualized_cost_UHS = 1e3*(inv_cost_UHS*discount_rate/(1-(1+discount_rate)^(-lifetime_UHS)) + OPEX_H2*inv_cost_UHS)
annualized_cost_PtG = 1e3*(inv_cost_PtG*discount_rate/(1-(1+discount_rate)^(-lifetime_PtG)) + OPEX_H2*inv_cost_PtG)
annualized_cost_H2T = 1e3*(inv_cost_H2T*discount_rate/(1-(1+discount_rate)^(-lifetime_H2T)) + OPEX_H2*inv_cost_H2T)


# Marginal costs including carbon prices (k€/GWh)
COST_CCGT = (gas_cost+carbon_price*EMIS_GAS)/eff_CCGT
COST_OCGT = (gas_cost+carbon_price*EMIS_GAS)/eff_OCGT
COST_PHS_var = 1 # small cost to avoid turbining and pumping simultaneously
COST_BESS_var = 1
COST_UHS_var = 1
COST_BIO = biogas_cost/eff_BIO
COST_SMR = (gas_cost+carbon_price*EMIS_GAS)/eff_SMR
COST_SLACK = 1e4 # price of raising demand for adequacy, should be null after the training phase is completed
COST_SHEDDING = [1e4, 1.2e4, 1.4e4, 1.6e4, 1.8e4, 2.0e4] # VoLL, k€/GWh


