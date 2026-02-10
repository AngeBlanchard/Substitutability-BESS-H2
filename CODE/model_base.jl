using Pkg
using JuMP
using Revise
using CPLEX
using Pkg
using SDDP
using XLSX
using DataFrames
using CSV
using Distributions, Random
# This file runs the SDDP model.
# A first part defines the equations of the model, loading data from data_loader.jl
# A second part trains the model
# A third part runs Monte Carlo simulations to evaluate the trained policy.

#------------------------- PARAMETERS ---------------------------------------

iter_lim = 500 # number of iteration for training the policy
Nbsimus = 10

N = 2184 # full year of 4h blocks adjusted to have exact number of months
global N_tot = N
global N_red = Int(N/182) # 4h blocks yield months of 182 timesteps

# Import data
include("data_loader.jl")

# Here, choose the sensitivity factors for the investment costs of BESS and H2 storage
SENSI_BESS = 1
SENSI_H2 = 1

annualized_cost_BESS_E = annualized_cost_BESS_E * SENSI_BESS
annualized_cost_BESS_P = annualized_cost_BESS_P * SENSI_BESS
annualized_cost_UHS = annualized_cost_UHS * SENSI_H2
annualized_cost_H2T = annualized_cost_H2T * SENSI_H2
annualized_cost_PtG = annualized_cost_PtG * SENSI_H2

println("##########################################################
--------------- DATA & PARAMETERS LOADED -----------------
##########################################################")

function model_Applied()

    #################################################################################################################################
    #-------------------------------------------------- DATA ------------------------------------------------------------------------
    #################################################################################################################################

    Ω = [[LF_onwind[:,i],
        LF_offwind[:,i],
        LF_solar[:,i],
        LOAD[:,i]] for i in 1:6] # 6 scenarios of VRE and load
    

    #################################################################################################################################
    #-------------------------------------------------- GRAPH -----------------------------------------------------------------------
    #################################################################################################################################
    graph = SDDP.LinearGraph(N_red+1)
    
    return SDDP.PolicyGraph(
        graph,
        sense = :Min,
        lower_bound = 0.0,
        optimizer = CPLEX.Optimizer,
    ) do sp, t

    #################################################################################################################################
    #-------------------------------------------------- STATE VARIABLES -------------------------------------------------------------
    #################################################################################################################################
        @variable(sp, 0 <= CAPA_CCGT, SDDP.State, initial_value = 30)
        @variable(sp, 0 <= CAPA_OCGT, SDDP.State, initial_value = 20)
        @variable(sp, 0 <= CAPA_SMR, SDDP.State, initial_value = 6)
        @variable(sp, 0 <= CAPA_PV, SDDP.State, initial_value = 200)
        @variable(sp, 0 <= CAPA_ONWIND, SDDP.State, initial_value = 100)
        @variable(sp, 0 <= CAPA_OFFWIND, SDDP.State, initial_value = 100)
        
        @variable(sp, 0 <= CAPA_PtG, SDDP.State, initial_value = 10)
        @variable(sp, 0 <= CAPA_H2T, SDDP.State, initial_value = 10)
        @variable(sp, 0 <= CAPA_UHS, SDDP.State, initial_value = 5000)
        @variable(sp, 0 <= CAPA_BESS_E, SDDP.State, initial_value = 80)
        @variable(sp, 0 <= CAPA_BESS_P, SDDP.State, initial_value = 20)

        @variable(sp, 0 <= volume_PHS <= CAPA_PHS_E, SDDP.State, initial_value = 0.5 * CAPA_PHS_E)
        @variable(sp, 0 <= volume_UHS, SDDP.State, initial_value = 2500)
        @variable(sp, 0 <= volume_BESS, SDDP.State, initial_value = 40)
        @variable(sp, 0 <= cycling_UHS, SDDP.State, initial_value = 0)

    #################################################################################################################################
    #-------------------------------------------------- DECISION VARIABLES ----------------------------------------------------------
    #################################################################################################################################

        @variable(sp, 0 <= prod_ccgt[1:182]) # CCGT production
        @variable(sp, 0 <= prod_ocgt[1:182]) # OCGT production
        @variable(sp, 0 <= prod_SMR[1:182]) # H2 production via SMR
        @variable(sp, 0 <= prod_bio[1:182] <= CAPA_BIO)
        @variable(sp, 0 <= prod_H2T[1:182]) # elec produced via H2 dischargeines
        @variable(sp, 0 <= prod_PtG[1:182]) # H2 produced with electrolysers, total

        @variable(sp, 0 <= charge_UHS[1:182]) # total H2 injection in the storage facilities
        @variable(sp, 0 <= discharge_UHS[1:182]) # total withdrawal of H2 from the storage facilities

        @variable(sp, 0 <= charge_PHS[1:182] <= CAPA_PHS_P)
        @variable(sp, 0 <= discharge_PHS[1:182] <= CAPA_PHS_P)

        @variable(sp, 0 <= charge_BESS[1:182])
        @variable(sp, 0 <= discharge_BESS[1:182])

        @variable(sp, 0 <= shedding[1:5, 1:182])
        @constraint(sp, [i in 1:4, j in 1:182], shedding[i, j] <= 1) # first shedding tech are lower than 5GW
        @variable(sp, shedding_cumul[1:182] >= 0) # cumul of all shedding technologies

        @variable(sp, prod_ror[1:182] >= 0)
        @variable(sp, prod_pv[1:182] >= 0)
        @variable(sp, prod_onwind[1:182] >= 0)
        @variable(sp, prod_offwind[1:182] >= 0)

        @variable(sp, slack[1:182] >= 0) # just a variable to penalize demand supply inadequacy
        @variable(sp, slack_h2[1:182] >= 0) # just a variable to penalize demand supply inadequacy
        @variable(sp, slack_h2_neg[1:182] >= 0) # just a variable to penalize demand supply inadequacy
        @variable(sp, slack_capa[1:182] >= 0) # just a variable to penalize capacity inadequacy
        @variable(sp, load[1:182] >= 0)

        # the following are necessary because of complete recourse need in SDDP: we cannot enforce a hard constraint on final reservoir levels
        @variable(sp, PHS_pos >= 0) # used to penalize emptying PHS reservoir at end of times
        @variable(sp, PHS_neg >= 0) 
        @variable(sp, UHS_pos >= 0) # used to penalize emptying H2 reservoir at end of times
        @variable(sp, UHS_neg >= 0) 
        @variable(sp, BESS_pos >= 0) # used to penalize emptying BESS reservoir at end of times
        @variable(sp, BESS_neg >= 0) 

        # Load random variable
        @variable(sp, ξ_load[i=1:182])

        # continuity variables with state variables (decision variables that take the role of state variables within a node)
        @variable(sp, 0 <= volume_PHS_2[1:182] <= CAPA_PHS_E)
        @variable(sp, 0 <= volume_UHS_2[1:182])
        @variable(sp, 0 <= volume_BESS_2[1:182])
        @variable(sp, 0 <= cycling_UHS_2[1:182])

    # First stage: investment, costs include half filling at 100€/MWh (own assumption)
    if t == 1
        @constraint(sp, volume_UHS.out == 0.5 * CAPA_UHS.out)
        @constraint(sp, volume_BESS.out == 0.5 * CAPA_BESS_E.out)
        @constraint(sp, volume_PHS.out == 0.5 * CAPA_PHS_E)
        @constraint(sp, cycling_UHS.out == 0)

        # Electricity supply demand adequacy
        @constraint(sp, demand_cons[i=1:182], 0 == 0)

        # H2 supply demand adequacy
        @constraint(sp, demand_H2_cons[i=1:182], 0 == 0)

        @stageobjective(sp, CAPA_BESS_E.out * (annualized_cost_BESS_E + 100/2) + 
            CAPA_BESS_P.out * annualized_cost_BESS_P +
            CAPA_UHS.out * (annualized_cost_UHS + 100/2) + # initial half filling at 100€/MWh
            CAPA_CCGT.out * annualized_cost_CCGT +
            CAPA_OCGT.out * annualized_cost_OCGT +
            CAPA_SMR.out * annualized_cost_SMR +
            CAPA_PtG.out * annualized_cost_PtG +
            CAPA_H2T.out * annualized_cost_H2T +
            CAPA_PV.out * annualized_cost_PV +
            CAPA_ONWIND.out * annualized_cost_ONWIND +
            CAPA_OFFWIND.out * annualized_cost_OFFWIND
            )
        return
    end

    # if t>1: operation stages
        # investment is blocked
        @constraint(sp, CAPA_UHS.out == CAPA_UHS.in)
        @constraint(sp, CAPA_BESS_E.out == CAPA_BESS_E.in)
        @constraint(sp, CAPA_BESS_P.out == CAPA_BESS_P.in)
        @constraint(sp, CAPA_CCGT.out == CAPA_CCGT.in)
        @constraint(sp, CAPA_OCGT.out == CAPA_OCGT.in)
        @constraint(sp, CAPA_SMR.out == CAPA_SMR.in)
        @constraint(sp, CAPA_PtG.out == CAPA_PtG.in)
        @constraint(sp, CAPA_H2T.out == CAPA_H2T.in)
        @constraint(sp, CAPA_PV.out == CAPA_PV.in)
        @constraint(sp, CAPA_ONWIND.out == CAPA_ONWIND.in)
        @constraint(sp, CAPA_OFFWIND.out == CAPA_OFFWIND.in)
    
        # Storage capacity constraints
        @constraint(sp, volume_UHS.out <= CAPA_UHS.out)
        @constraint(sp, volume_BESS.out <= CAPA_BESS_E.out)

        # cycling constraint : forbidden to do more than 12 cycles per year
        @constraint(sp, cycling_UHS.out <= 12 * CAPA_UHS.out)
        
        # Within a node, multiple timesteps (i.e. hours of the day)
        @constraint(sp, volume_PHS_2[1] - volume_PHS.in == 0)
        @constraint(sp, volume_UHS_2[1] - volume_UHS.in == 0)
        @constraint(sp, volume_BESS_2[1] - volume_BESS.in == 0)
        @constraint(sp, cycling_UHS_2[1] - cycling_UHS.in == 0)

        for i in 1:181
            # given 4h-blocs, factor 4 between GWh and GW
            @constraint(sp, volume_PHS_2[i+1] - volume_PHS_2[i] + 4*discharge_PHS[i] - 4*charge_PHS[i] * eff_PHS  == 0)
            @constraint(sp, volume_UHS_2[i+1] - volume_UHS_2[i] + 4*discharge_UHS[i] - 4*charge_UHS[i] * eff_UHS == 0)
            @constraint(sp, volume_BESS_2[i+1] - volume_BESS_2[i] + 4*discharge_BESS[i] - 4*charge_BESS[i] * eff_BESS  == 0)
            @constraint(sp, cycling_UHS_2[i+1] - cycling_UHS_2[i] - 4*discharge_UHS[i] == 0)
        end

        for i in 1:182
            @constraint(sp, prod_ccgt[i] <= CAPA_CCGT.out + slack_capa[i])
            @constraint(sp, prod_ocgt[i] <= CAPA_OCGT.out + slack_capa[i])
            @constraint(sp, prod_SMR[i] <= CAPA_SMR.out + slack_capa[i]) # H2 production via SMR
            @constraint(sp, prod_H2T[i] <= CAPA_H2T.out + slack_capa[i]) # elec produced via eff_H2T
            @constraint(sp, prod_PtG[i] <= CAPA_PtG.out * eff_PtG + slack_capa[i]) # H2 produced with electrolysers

            @constraint(sp, volume_UHS_2[i] <= CAPA_UHS.out)
            @constraint(sp, volume_BESS_2[i] <= CAPA_BESS_E.out)
            @constraint(sp, cycling_UHS_2[i] <= 12 * CAPA_UHS.out)
            @constraint(sp, shedding_cumul[i] == sum(shedding[j, i] for j in 1:length(shedding[:, i])))

            # Charge & Discharge rates
            @constraint(sp, charge_BESS[i] <= CAPA_BESS_P.out + slack_capa[i]) 
            @constraint(sp, discharge_BESS[i] <= CAPA_BESS_P.out + slack_capa[i])

            # LOAD & ROR
            @constraint(sp, prod_ror[i] == hydro_ror[(t-2)*182+i])
            @constraint(sp, load[i] == ξ_load[i]) 

        end

        # VRE production
        @constraint(sp, con_pv[i in 1:182], prod_pv[i] - 1*CAPA_PV.out <= 0)
        @constraint(sp, con_onwind[i in 1:182], prod_onwind[i] - 1*CAPA_ONWIND.out <= 0)
        @constraint(sp, con_offwind[i in 1:182], prod_offwind[i] - 1*CAPA_OFFWIND.out <= 0)

        # transition constraints
        @constraint(sp, volume_PHS.out - volume_PHS_2[182] + 4*discharge_PHS[182] - 4*charge_PHS[182] * eff_PHS  == 0)
        @constraint(sp, volume_UHS.out - volume_UHS_2[182] + 4*discharge_UHS[182] - 4*charge_UHS[182] * eff_UHS == 0)
        @constraint(sp, volume_BESS.out - volume_BESS_2[182] + 4*discharge_BESS[182] - 4*charge_BESS[182] * eff_BESS == 0)
        @constraint(sp, cycling_UHS.out - cycling_UHS_2[182] - 4*discharge_UHS[182] == 0)

        # Electricity supply demand adequacy
        @constraint(sp, demand_cons[i=1:182],
        discharge_PHS[i] 
        + discharge_BESS[i]
        + prod_ccgt[i]
        + prod_ocgt[i] 
        + prod_H2T[i]
        + prod_bio[i] 
        + prod_ror[i] 
        + prod_pv[i] 
        + prod_onwind[i]
        + prod_offwind[i] 
        + shedding_cumul[i]
        - slack[i]
        - charge_PHS[i]
        - prod_PtG[i] / eff_PtG # electricity consumption of electrolysis
        - charge_BESS[i]
        == load[i] )

        # H2 supply demand adequacy
        @constraint(sp, demand_H2_cons[i=1:182],
        prod_PtG[i] 
        + prod_SMR[i] 
        - charge_UHS[i]
        + discharge_UHS[i]
        + slack_h2[i]
        - slack_h2_neg[i]
        - prod_H2T[i]/eff_H2T
        == DEM_H2)

        # Penalizing final reservoir levels discrepency
        if (t == N_red + 1)
            @constraint(sp, volume_PHS.out - 0.5*CAPA_PHS_E >= - PHS_neg)
            @constraint(sp, volume_PHS.out - 0.5*CAPA_PHS_E <= PHS_pos)
            @constraint(sp, volume_UHS.out - 0.5*CAPA_UHS.out >= - UHS_neg)
            @constraint(sp, volume_UHS.out - 0.5*CAPA_UHS.out <= UHS_pos)
            @constraint(sp, volume_BESS.out - 0.5*CAPA_BESS_E.out >= - BESS_neg)
            @constraint(sp, volume_BESS.out - 0.5*CAPA_BESS_E.out <= BESS_pos)
        end 
        
        # Month-ahead stochasticity approach
        SDDP.parameterize(sp, Ω) do ω 
            
            for (i, c) in enumerate(con_onwind)
                set_normalized_coefficient(c, CAPA_ONWIND.out, -ω[1][182*(t-2)+i])
            end
            for (i, c) in enumerate(con_offwind)
                set_normalized_coefficient(c, CAPA_OFFWIND.out, -ω[2][182*(t-2)+i])
            end
            for (i, c) in enumerate(con_pv)
                set_normalized_coefficient(c, CAPA_PV.out, -ω[3][182*(t-2)+i])
            end

            for i in 1:182
                JuMP.fix(ξ_load[i], ω[4][(t-2)*182 + i]; force = true)
            end
        end

        # Objective function for the dispatch problem
        @stageobjective(sp, 
        4 * sum(prod_ccgt[i]*COST_CCGT # factor 4 because one block is 4 hours
            + prod_ocgt[i]*COST_OCGT
            + prod_SMR[i]*COST_SMR
            + prod_bio[i] * COST_BIO
            + charge_PHS[i] * COST_PHS_var
            + charge_BESS[i] * COST_BESS_var
            + charge_UHS[i] * COST_UHS_var
            + sum(shedding[j, i] * COST_SHEDDING[j] for j in 1:length(shedding[:, i]))
            + slack[i] * COST_SLACK
            + slack_h2[i] * COST_SLACK
            + slack_h2_neg[i] * COST_SLACK
            + slack_capa[i] * COST_SLACK
        for i in 1:182)
        # discrepency penalties for end of time reservoir levels
        + (PHS_pos + PHS_neg) * 1000
        + (UHS_pos + UHS_neg) * 1000
        + (BESS_pos + BESS_neg) * 1000
        ) 
    end
end


println("##########################################################
-------------------- MODEL COMPILED ----------------------
##########################################################")
# Training part: either you train from scratch or you load previously saved cuts and continue training

model = model_Applied() 
# To continue training from previously saved cuts, uncomment the following line
#SDDP.read_cuts_from_file(model, "RESULTS/cuts_base_$SENSI_BESS.json")

SDDP.train(
    model, 
    iteration_limit = iter_lim,
    add_to_existing_cuts = true
)

# writing cuts to file
SDDP.write_cuts_to_file(model, "RESULTS/cuts_base_$SENSI_BESS.json")


println("##########################################################
-------------------- TRAINING COMPLETED ------------------
##########################################################")

#############################################################
# For running on Monte Carlo samples
#############################################################
model_sample = model_Applied() 
scenarios = [SDDP.sample_scenario(model_sample, SDDP.InSampleMonteCarlo())[1] for _ in 1:Nbsimus]
sampling_scheme_MC = SDDP.Historical(scenarios)


#############################################################
# Computation
#############################################################

model = model_Applied()
SDDP.read_cuts_from_file(model, "RESULTS/cuts_base_$SENSI_BESS.json")

#######################################################################################
#######################################################################################
############################# RESULTS_local ###########################################
#######################################################################################
#######################################################################################
sims = SDDP.simulate(
    model, 
    Nbsimus, 
    [
    :prod_ccgt, 
    :prod_ocgt, 
    :prod_H2T,
    :prod_bio, 
    :prod_ror,
    :discharge_PHS, 
    :charge_PHS, 
    :discharge_BESS,
    :charge_BESS,
    :prod_PtG,
    :prod_SMR,
    :discharge_UHS,
    :charge_UHS,
    :prod_pv, 
    :prod_onwind, 
    :prod_offwind,
    :volume_PHS_2, 
    :volume_UHS_2,
    :cycling_UHS_2,
    :volume_BESS_2,
    :volume_PHS, 
    :volume_UHS,
    :CAPA_UHS,
    :CAPA_BESS_E,
    :CAPA_BESS_P,
    :CAPA_CCGT,
    :CAPA_OCGT,
    :CAPA_SMR,
    :CAPA_PtG,
    :CAPA_H2T,
    :CAPA_PV,
    :CAPA_ONWIND,
    :CAPA_OFFWIND,
    :cycling_UHS,
    :volume_BESS,
    :shedding_cumul,
    :slack,
    :load,
    ];

    custom_recorders = merge(
    Dict{Symbol,Function}(Symbol("price_$i") => (sp::JuMP.Model) -> JuMP.dual(constraint_by_name(sp, "demand_cons[$i]")) for i in 1:182),
    Dict{Symbol,Function}(Symbol("price_H2_$j") => (sp::JuMP.Model) -> JuMP.dual(constraint_by_name(sp, "demand_H2_cons[$j]")) for j in 1:182)
    ),

    sampling_scheme = sampling_scheme_MC,
    )
    

objectives = map(sims) do simulation
    return sum(stage[:stage_objective] for stage in simulation)
end

μ_flex, ci_flex = SDDP.confidence_interval(objectives)
println("Confidence interval: ", μ_flex, " ± ", ci_flex)
println("lower bound: ", SDDP.calculate_bound(model))

value_functions = Dict(
node => SDDP.ValueFunction(model; node = node) for node in keys(model.nodes)
)

op_cost_UHS = Array{Float64}(undef, N_red, Nbsimus)
op_cost_BESS = Array{Float64}(undef, N_red, Nbsimus)
op_cost_PHS = Array{Float64}(undef, N_red, Nbsimus)

for t in 2:N_red+1
    for sim in 1:Nbsimus
        V = value_functions[t]
        levels = Dict(
            "CAPA_UHS" => sims[sim][t][:CAPA_UHS].out,
            "CAPA_BESS_E" => sims[sim][t][:CAPA_BESS_E].out,
            "CAPA_BESS_P" => sims[sim][t][:CAPA_BESS_P].out,
            "CAPA_CCGT" => sims[sim][t][:CAPA_CCGT].out,
            "CAPA_OCGT" => sims[sim][t][:CAPA_OCGT].out,
            "CAPA_SMR" => sims[sim][t][:CAPA_SMR].out,
            "CAPA_PtG" => sims[sim][t][:CAPA_PtG].out,
            "CAPA_H2T" => sims[sim][t][:CAPA_H2T].out,
            "CAPA_PV" => sims[sim][t][:CAPA_PV].out,
            "CAPA_ONWIND" => sims[sim][t][:CAPA_ONWIND].out,
            "CAPA_OFFWIND" => sims[sim][t][:CAPA_OFFWIND].out,
            "volume_PHS" => sims[sim][t][:volume_PHS].out,
            "volume_UHS" => sims[sim][t][:volume_UHS].out,
            "volume_BESS" => sims[sim][t][:volume_BESS].out,
            "cycling_UHS" => sims[sim][t][:cycling_UHS].out,
        )
        c, p = SDDP.evaluate(V, levels)
        op_cost_UHS[t-1, sim] = -p[Symbol("volume_UHS")]
        op_cost_BESS[t-1, sim] = -p[Symbol("volume_BESS")]
        op_cost_PHS[t-1, sim] = -p[Symbol("volume_PHS")]
    end
end

op_cost_UHS_df = DataFrame(op_cost_UHS, :auto)
op_cost_PHS_df = DataFrame(op_cost_PHS, :auto)
op_cost_BESS_df = DataFrame(op_cost_BESS, :auto)

#CSV.write("RESULTS/endo_testop_costs_UHS.csv", op_cost_UHS_df, delim = ";") 
#CSV.write("RESULTS/endo_testop_costs_PHS.csv", op_cost_PHS_df, delim = ";") 
#CSV.write("RESULTS/endo_testop_costs_BESS.csv", op_cost_BESS_df, delim = ";") 


#######################################################################################
#######################################################################################
##################################### EXPORT RESULTS_local ##################################
#######################################################################################
#######################################################################################
#------------------------------------- PRICES -------------------------------------------------
S = size(sims, 1)
prices = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            prices[182*(i-2)+j, s] = sims[s][i][Symbol("price_$j")]
        end
    end
end

prices_df = DataFrame(prices, :auto)

#CSV.write("RESULTS/base_prices.csv", prices_df, delim = ";")
############################## H2 PRICE ##################################

prices_H2 = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            prices_H2[182*(i-2)+j, s] = sims[s][i][Symbol("price_H2_$j")]
        end
    end
end

prices_H2_df = DataFrame(prices_H2, :auto)

#CSV.write("RESULTS/base_prices_H2.csv", prices_H2_df, delim = ";")

#####################################################################################################
#----------------------------------------- UHS discharging -------------------------------------------
#####################################################################################################

discharge_UHS = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            discharge_UHS[182*(i-2)+j, s] = sims[s][i][:discharge_UHS][j]
        end
    end
end

discharge_UHS_df = DataFrame(discharge_UHS, :auto)

#CSV.write("RESULTS/base_discharge_UHS.csv", discharge_UHS_df, delim = ";")


#####################################################################################################
#----------------------------------------- UHS charging --------------------------------------------
#####################################################################################################

charge_UHS = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            charge_UHS[182*(i-2)+j, s] = sims[s][i][:charge_UHS][j]
        end
    end
end

charge_UHS_df = DataFrame(charge_UHS, :auto)

#CSV.write("RESULTS/base_charge_UHS.csv", charge_UHS_df, delim = ";")

#####################################################################################################
#----------------------------------------- BESS discharging -------------------------------------------
#####################################################################################################

discharge_BESS = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            discharge_BESS[182*(i-2)+j, s] = sims[s][i][:discharge_BESS][j]
        end
    end
end

discharge_BESS_df = DataFrame(discharge_BESS, :auto)

#CSV.write("RESULTS/base_discharge_BESS.csv", discharge_BESS_df, delim = ";")

#####################################################################################################
#----------------------------------------- BESS charging --------------------------------------------
#####################################################################################################

charge_BESS = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            charge_BESS[182*(i-2)+j, s] = sims[s][i][:charge_BESS][j]
        end
    end
end

charge_BESS_df = DataFrame(charge_BESS, :auto)

#CSV.write("RESULTS/base_charge_BESS.csv", charge_BESS_df, delim = ";")

#####################################################################################################
#----------------------------------------- PHS discharging -------------------------------------------
#####################################################################################################

discharge_PHS = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            discharge_PHS[182*(i-2)+j, s] = sims[s][i][:discharge_PHS][j]
        end
    end
end

discharge_PHS_df = DataFrame(discharge_PHS, :auto)

#CSV.write("RESULTS/base_discharge_PHS.csv", discharge_PHS_df, delim = ";")

#####################################################################################################
#----------------------------------------- PHS charging --------------------------------------------
#####################################################################################################

charge_PHS = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            charge_PHS[182*(i-2)+j, s] = sims[s][i][:charge_PHS][j]
        end
    end
end

charge_PHS_df = DataFrame(charge_PHS, :auto)

#CSV.write("RESULTS/base_charge_PHS.csv", charge_PHS_df, delim = ";")

#------------------------------------- H2 cycling -------------------------------------------

cycling = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            cycling[182*(i-2)+j, s] = sims[s][i][:cycling_UHS_2][j]
        end
    end
end

cycling_df = DataFrame(cycling, :auto)

#CSV.write("RESULTS/base_cycling_UHS.csv", cycling_df, delim = ";")

#------------------------------------- H2 electrolysis ------------------------------------------

PtG = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            PtG[182*(i-2)+j, s] = sims[s][i][:prod_PtG][j]
        end
    end
end

PtG_df = DataFrame(PtG, :auto)

#CSV.write("RESULTS/base_ptg.csv", PtG_df, delim = ";")
    


#------------------------------------- H2 volume ------------------------------------------

vol_H2 = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            vol_H2[182*(i-2)+j, s] = sims[s][i][:volume_UHS_2][j]
        end
    end
end

vol_H2_df = DataFrame(vol_H2, :auto)

#CSV.write("RESULTS/base_vol_H2.csv", vol_H2_df, delim = ";")

###########################################################################################################
####################################### CHERRY RESULTS ####################################################
###########################################################################################################
# These are results for a single simulation (the first one), allowing to see typical profiles of production, load, etc.

results = ones(N_tot, 18)

for i in 2:N_red+1
    for j in 1:182
        results[182*(i-2)+j, 1] = sims[1][i][:prod_ror][j]
        results[182*(i-2)+j, 2] = sims[1][i][:prod_bio][j]
        results[182*(i-2)+j, 3] = sims[1][i][:prod_ccgt][j]
        results[182*(i-2)+j, 4] = sims[1][i][:prod_ocgt][j]
        results[182*(i-2)+j, 5] = sims[1][i][:prod_H2T][j]
        results[182*(i-2)+j, 6] = sims[1][i][:prod_pv][j]
        results[182*(i-2)+j, 7] = sims[1][i][:prod_onwind][j]
        results[182*(i-2)+j, 8] = sims[1][i][:prod_offwind][j]
        results[182*(i-2)+j, 9] = sims[1][i][:shedding_cumul][j]
        results[182*(i-2)+j, 10] = sims[1][i][:prod_PtG][j]/0.7 # power terms
        results[182*(i-2)+j, 11] = sims[1][i][:load][j]
        results[182*(i-2)+j, 12] = sims[1][i][:discharge_PHS][j]
        results[182*(i-2)+j, 13] = sims[1][i][:charge_PHS][j]
        results[182*(i-2)+j, 14] = sims[1][i][:discharge_BESS][j]
        results[182*(i-2)+j, 15] = sims[1][i][:charge_BESS][j]
        results[182*(i-2)+j, 16] = sims[1][i][:charge_UHS][j]
        results[182*(i-2)+j, 17] = sims[1][i][:discharge_UHS][j]
        results[182*(i-2)+j, 18] = sims[1][i][:prod_SMR][j]
    end
end

results_prod = DataFrame(results, :auto)

rename!(results_prod,
[:prod_ror, :prod_bio, :prod_ccgt, :prod_ocgt, :prod_H2T, :prod_pv, :prod_onwind, :prod_offwind, :shedding_cumul, :prod_PtG,
:load, :discharge_PHS, :charge_PHS, :discharge_BESS, :charge_BESS, :charge_UHS, :discharge_UHS, :prod_SMR])

CSV.write("RESULTS/base_cherry_$SENSI_BESS.csv", results_prod, delim = ";")

###########################################################################################################
####################################### MEAN RESULTS_local ######################################################
###########################################################################################################
#=
# Uncomment this section to see what the average behavior across all simulations is

results = ones(N_tot, 30)

for i in 2:N_red+1
    for j in 1:182
        results[182*(i-2)+j, 1] = round(mean([s[i][:discharge_PHS][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 2] = round(mean([s[i][:charge_PHS][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 3] = round(mean([s[i][:discharge_BESS][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 4] = round(mean([s[i][:charge_BESS][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 5] = round(mean([s[i][:prod_bio][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 6] = round(mean([s[i][:prod_ccgt_1][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 7] = round(mean([s[i][:prod_ccgt_2][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 8] = round(mean([s[i][:prod_ccgt_3][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 9] = round(mean([s[i][:prod_ocgt_1][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 10] = round(mean([s[i][:prod_ocgt_2][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 11] = round(mean([s[i][:prod_ocgt_3][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 12] = round(mean([s[i][:prod_DSM][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 13] = round(mean([s[i][:prod_ccgt_H2][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 14] = round(mean([s[i][:shedding_cumul][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 15] = round(mean([s[i][:prod_pv][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 16] = round(mean([s[i][:prod_wind][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 17] = round(mean([s[i][:prod_import][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 18] = round(mean([s[i][:prod_ror][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 19] = round(mean([s[i][:curt_pv][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 20] = round(mean([s[i][:curt_wind][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 21] = round(mean([s[i][:prod_export][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 22] = round(mean([s[i][:ptg][j] for s in sims]), digits = 2)/0.7 # power terms
        results[182*(i-2)+j, 23] = round(mean([s[i][:load][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 24] = round(mean([s[i][:charge_UHS][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 25] = round(mean([s[i][:discharge_UHS][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 26] = round(mean([s[i][:emissions][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 27] = round(mean([s[i][:prod_SMR][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 28] = round(mean([s[i][:import_H2][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 29] = round(mean([s[i][:charge_LINEPACK][j] for s in sims]), digits = 2)
        results[182*(i-2)+j, 30] = round(mean([s[i][:discharge_LINEPACK][j] for s in sims]), digits = 2)
    end
end

results_prod = DataFrame(results, :auto)

rename!(results_prod,
[:discharge_PHS, :charge_PHS, :discharge_BESS, :charge_BESS, :biomass, :CCGT_1, :CCGT_2, :CCGT_3, :OCGT_1, :OCGT_2, :OCGT_3, :DSM, :CCGT_H2,
:shedding, :solar, :wind, :imports, :run_of_river, :curtailment_solar, :curtailment_wind, :exports, :PtG, :load, 
:charge_UHS, :discharge_UHS, :emissions, :prod_SMR, :import_H2, :charge_LINEPACK, :discharge_LINEPACK])

#CSV.write("RESULTS/base_mean_results.csv", results_prod, delim = ";")
=#

#------------------------------------- VOLUMES -------------------------------------------------

volumes = ones(N_tot, 3)

for i in 2:N_red+1
    for j in 1:182
        volumes[182*(i-2)+j, 1] = round(mean([s[i][:volume_PHS_2][j] for s in sims]), digits = 2)
        volumes[182*(i-2)+j, 2] = round(mean([s[i][:volume_UHS_2][j] for s in sims]), digits = 2)
        volumes[182*(i-2)+j, 3] = round(mean([s[i][:volume_BESS_2][j] for s in sims]), digits = 2)
    end
end

volumes_df = DataFrame(volumes, :auto)
rename!(volumes_df,[:PHS, :H2, :BESS])

CSV.write("RESULTS/base_volumes_$SENSI_BESS.csv", volumes_df, delim = ";")
#=
    ############################################################################################
    ######################################## H2 prod dispatch ##################################
    ############################################################################################
    h2_disp = ones(N_tot, 4)

    for i in 2:N_red+1
        for j in 1:182
            h2_disp[182*(i-2)+j, 1] = round(mean([s[i][:prod_SMR_cons][j] for s in sims]), digits = 2)
            h2_disp[182*(i-2)+j, 2] = round(mean([s[i][:import_H2_cons][j] for s in sims]), digits = 2)
            h2_disp[182*(i-2)+j, 3] = round(mean([s[i][:discharge_LINEPACK_cons][j] for s in sims]), digits = 2)
            h2_disp[182*(i-2)+j, 3] = round(mean([s[i][:discharge_UHS_cons][j] for s in sims]), digits = 2)
            h2_disp[182*(i-2)+j, 4] = round(mean([s[i][:ptg_cons][j] for s in sims]), digits = 2)
        end
    end

    h2_disp_df = DataFrame(h2_disp, :auto)
    rename!(h2_disp_df,[:SMR, :IMPORT, :discharge, :ELEC])
CSV.write("RESULTS/base_H2_dispatch.csv", h2_disp_df, delim = ";")
=#

#------------------------------------- VRE PROD -----------------------------------------------
pv_prod = ones(N_tot, S)
onwind_prod = ones(N_tot, S)
offwind_prod = ones(N_tot, S)

for s in 1:S
    for i in 2:N_red+1
        for j in 1:182
            pv_prod[182*(i-2)+j, s] = sims[s][i][:prod_pv][j]
            onwind_prod[182*(i-2)+j, s] = sims[s][i][:prod_onwind][j]
            offwind_prod[182*(i-2)+j, s] = sims[s][i][:prod_offwind][j]
        end
    end
end

pv_prod_df = DataFrame(pv_prod, :auto)
onwind_prod_df = DataFrame(onwind_prod, :auto)
offwind_prod_df = DataFrame(offwind_prod, :auto)

#    CSV.write("RESULTS/base_pv.csv", pv_prod_df, delim = ";")
#    CSV.write("RESULTS/base_onwind.csv", onwind_prod_df, delim = ";")
#    CSV.write("RESULTS/base_offwind.csv", offwind_prod_df, delim = ";")

###########################################################################################################
####################################### GLOBAL  ###########################################################
###########################################################################################################

# This section summarizes key results such as total costs and installed capacities

M = max(1, 11) # in the case S<11 we still need 11 rows to fill with installed capacity
objective = zeros(M, 4)

for s in 1:S
    objective[s, 1] = sum(sims[s][i][:stage_objective] for i in 1:N_red+1)
end

objective[1, 2] = SDDP.calculate_bound(model)
objective[1, 4] = sims[1][1][:CAPA_CCGT].out
objective[2, 4] = sims[1][1][:CAPA_OCGT].out
objective[3, 4] = sims[1][1][:CAPA_SMR].out
objective[4, 4] = sims[1][1][:CAPA_PtG].out
objective[5, 4] = sims[1][1][:CAPA_H2T].out
objective[6, 4] = sims[1][1][:CAPA_PV].out
objective[7, 4] = sims[1][1][:CAPA_ONWIND].out
objective[8, 4] = sims[1][1][:CAPA_OFFWIND].out
objective[9, 4] = sims[1][1][:CAPA_UHS].out
objective[10, 4] = sims[1][1][:CAPA_BESS_E].out
objective[11, 4] = sims[1][1][:CAPA_BESS_P].out


objective_df = DataFrame(objective, :auto)
tech_names = ["CCGT", "OCGT", "SMR", "PtG", "H2T", "PV", "ONWIND", "OFFWIND", "UHS", "BESS_E", "BESS_P"]

# Extend tech_names to match the number of rows in objective_df
objective_df[!, :Technology] = vcat(tech_names, fill(missing, size(objective_df, 1) - length(tech_names)))

# Reorder columns so that :Technology is at position 3
select!(objective_df, 1:2, :Technology, 3:ncol(objective_df)-1)
CSV.write("RESULTS/base_objective_$SENSI_BESS.csv", objective_df, delim = ";")

#------------------------------------- Throwing ERRORS -------------------------------------------------
for s in 1:S
    for t in 2:N_red+1
        for j in 1:182
            if(sims[s][t][:slack][j] > 0)
                println("OOPS problem in slack here")
            end
        end
    end
end

