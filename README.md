# optimal-control-model

### How to use:
All functions are designed to be run from optimal_control_workflow.rmd in the root directory. Each stage in the process is set up in its own code chunk. Just run them in order. 

Structure:

\src
data_preparation.R import emissions and economic forecasts

latin_hypercube_sampling.R creates LHS for 5 parameters

model_parameters.R adds fixed parameters to LHS OR creates all parameters from mean values (currently set to mean values)

optimal_control runs multi-parameter forward-backward sweep, saves returned solution as RDS, calls shooting_method and forward_backward_sweep

forward_backward_sweep calculates solution for optimal control problem

shooting_method calculates lambda values to achieve end-time state target, calls forward_backward_sweep

visualization.R creates plots

No longer in use:
\src
experimental_design.R
optimal_control_revised.R
