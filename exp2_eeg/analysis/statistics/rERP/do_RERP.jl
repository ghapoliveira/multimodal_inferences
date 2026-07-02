# Fits the rERP using Christoph Aurnhammer's (https://github.com/caurnhammer/psyp23rerps) framework (rERP.jl).
# Run from the rERP folder: the relative paths assume eeg_continuous_for_julia.csv lives in ./rERP_outputs/

# Outputs: rERP_Emojis_Across_data.csv; rERP_Emojis_Across_models.csv;
#          rERP_Emojis_Within_data.csv; rERP_Emojis_Within_models.csv
# Within: each subject regressed separately; 
# Across: participants pooled as if they were one (following Aurnhammer's procedure).
#
# Data: observed data; 
# Models: regression statistics — coefficients, t-values, p-values, per predictor/timepoint/electrode.

# Load base functions
include("rERP.jl");

# 1. Indicate electrodes
elec = [:Fp1, :Fp2, :F7, :F3, :Fz, :F4, :F8, :FT9, :FC5, :FC1, :FC2, :FC6, :FT10, :T7,
 :C3, :Cz, :C4, :T8, :CP5, :CP1, :CP2, :CP6, :P7, :P3, :Pz, :P4, :P8, :O1, :Oz, :O2]; 

# 2. Configure the dynamic model
models = make_models(
    [:Subject, :Timestamp], 
    [:Item, :Condition], 
    elec, 
    [:Intercept, :Semantic_Score, :Info_Score, :Mean_Visual_Error] # The continuous variables
);

# 3. Load python file
mkpath("rERP_outputs")
csv_path = "./rERP_outputs/eeg_continuous_for_julia.csv"
dt = process_data(csv_path, false, models);

# 4. Intra-subject regression
println("Starting intra-subject analysis...")
@time fit_models(dt, models, "./rERP_outputs/rERPs_Emojis_Within");

# 5. Inter-subjects regression
println("Starting inter-subjects analysis...")
dts = copy(dt);
dts.Subject = ones(Int, nrow(dts));
@time fit_models(dts, models, "./rERP_outputs/rERPs_Emojis_Across");
