# Substitutability-BESS-H2
This repo gathers files and data to run the model from Blanchard &amp; Megy, Battery and Hydrogen Storage: Complements or Substitutes?  A German 2035 Case Study (November 25, 2025). Available at SSRN: https://ssrn.com/abstract=5802822 or http://dx.doi.org/10.2139/ssrn.5802822 

Download the Julia files and the DATA folder to run the code.
data_loader.jl compiles the data from the excel file
modelbase.jl is the core model. One can change the assumption about BESS and H2 costs in the beginning of this file, to compute the elasticity of substitution.
