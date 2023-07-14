# Asymmetrical tongue bracing simulations

This repository contains the results and analysis for Azreen et al. (in press). The repository is structured as follows:
* The `src` folder contains the scripts used to generate the simulation inputs (`generate_batchsim.py`) and to analyze the simulation results (`analyze_simulation_results.R`).
* The `data` folder contains the following items:
** A folder for each set of simulation data. The paper uses only the `50-150_jaw_simulations` results.
** The original set of successful bracing simulations from Liu et al. (2022; `bracing_activations.csv`)
** The `.csv` files relating simulation numbers to agonist/antagonist activation levels. The paper uses only `asymmetrical_activations_50_150_jaw.csv`.
** The `.psl` files used to drive the Artisynth simulations. This paper uses only `props_50_150_jaw.psl`.

For the code used to run the simulations, visit [this repository](https://github.com/connormayer/artisynth_models/tree/asymmetrical_bracing).

## Citation:

Azreen, J., Mayer, C., Liu, Y., Shamei, A., Stavness, I., & Gick, B. (in press). Biomechanical simulation of lateral asymmetry in tongue bracing. _Canadian Acoustics_.