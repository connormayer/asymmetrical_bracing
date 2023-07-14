import csv
import pandas

# This script generates the simulations used in this paper

# Designate agonists and antagonists
AGONISTS = [
    "VERT",
    "GGM",
    "MH",
    "GGP"
]

ANTAGONISTS = [
    "HG",
    "TRANS",
    "STY",
    "GGA",
    "IL"
]

# Load the file of original bracing outcomes from Liu et al. (2022)
df = pandas.read_csv("data/bracing_activations.csv")

# Just keep muscle activations
muscle_names = list(set(df.columns[8:18]))

# Build each of the four simulations starting from original one
new_rows = []
for index, row in df.iterrows():
    # Activate antagonists at 100% or 150%
    for l_ant_act in [1, 1.5]:
        # Activate agonists at 50% or 100%
        for l_ag_act in [0.5, 1]:
            # Record details of activations
            new_row = {}
            new_row['L_ANT'] = l_ant_act
            new_row['R_ANT'] = 1
            new_row['L_AG'] = l_ag_act
            new_row['R_AG'] = 1
            new_row['SL_ACT'] = 1

            # Scale original muscle activation by relevant factor
            for muscle in muscle_names:
                # SL isn't lateralized, so keep as is
                if muscle == "SL":
                    new_row["SL"] = row[muscle]
                    continue

                if muscle in ANTAGONISTS:
                    l_mult = l_ant_act
                else:
                    l_mult = l_ag_act

                # Add columns for lateralized msucle activations
                new_row[muscle + "_L"] = round(row[muscle] * l_mult, 3)
                new_row[muscle + "_R"] = row[muscle]
            # Jaw activation to get us to 5mm
            new_row['BI_CLOSE'] = 0.0015

            new_rows.append(new_row)

# Save a .csv that records the simulation number and its corresponding
# muscle activations
new_df = pandas.DataFrame(new_rows)
new_df.to_csv("data/asymmetrical_activations_50_150_jaw.csv")

# Create batchsim file to drive simulations in Artisynth. 
# We use a combination of the simulation number and the 'redef'
# command in BatchSim to run an arbitrary set of simulations,
# rather than one based on grid search or sampling
batchsim_rows = []
batchsim_rows.append('@PHONY "simnum" = {{%{}%}}'.format(
    '% %'.join([str(x) for x in range(len(new_df))])
))

muscle_names = new_df.iloc[:, 5:-1]

for muscle in muscle_names:
    batchsim_rows.append(
        '"models/jawmodel/models/tongue/bundles/{}:excitation" = {{}}'.format(muscle)
    )
batchsim_rows.append('"models/jawmodel/exciters/bi_close:excitation" = {%0.0015%}')

for idx, row in new_df.iterrows():
    batchsim_rows.append('redef')
    for muscle in muscle_names:
        batchsim_rows.append(
            '\t"models/jawmodel/models/tongue/bundles/{}:excitation" = {{%{}%}}'.format(
                muscle, row[muscle]
            )
        )
    batchsim_rows.append('when')
    batchsim_rows.append('\t"simnum" = {{%{}%}}'.format(str(idx)))
    batchsim_rows.append("end")

# Write the batchsim props file
with open('data/props_50_150_jaw.psl', 'w') as f:
    f.write('\n'.join(batchsim_rows))
