import csv
import pandas

AGONISTS = [
    "VERT",
    "GGM",
    "MH",
    "SL",
    "GGP"
]

ANTAGONISTS = [
    "HG",
    "TRANS",
    "STY",
    "GGA",
    "IL"
]

df = pandas.read_csv("data/bracing_activations.csv")

# Just keep muscle activations, plus new columns reflecting conditions
new_header = []

muscle_names = list(set(df.columns[8:18]) - set("SL"))

for name in muscle_names:
    new_header.extend([name + "-L", name + "-R"])
new_header.extend(["SL", "L_ANT", "R_ANT", "L_AG", "R_AG", "SL_ACT"])

new_rows = []
for index, row in df.iterrows():
    for l_ant_act in [1, 1.5]:
        for l_ag_act in [0.5, 1]:
            for sl_act in [0.5, 1]:
                    new_row = {}
                    new_row['L_ANT'] = l_ant_act
                    new_row['R_ANT'] = 1
                    new_row['L_AG'] = l_ag_act
                    new_row['R_AG'] = 1
                    new_row['SL_ACT'] = sl_act

                    for muscle in muscle_names + ["SL"]:
                        if muscle == "SL":
                            new_row["SL"] = round(row["SL"] * sl_act, 2)
                            continue

                        if muscle in ANTAGONISTS:
                            l_mult = l_ant_act
                        else:
                            l_mult = l_ag_act

                        new_row[muscle + "-L"] = round(row[muscle] * l_mult, 2)
                        new_row[muscle + "-R"] = row[muscle]

                    new_rows.append(new_row)

new_df = pandas.DataFrame(new_rows)

# Create batchsim file
batchsim_rows = []
batchsim_rows.append('@PHONY "simnum" = {{%{}%}}'.format(
    '% %'.join([str(x) for x in range(len(new_df))])
))

muscle_names = new_df.iloc[:, 5:]
for idx, row in new_df.iterrows():
    batchsim_rows.append('redef')
    for muscle in muscle_names:
        batchsim_rows.append('\t"{}"" = {{%{}%}}'.format(muscle, row[muscle]))
    batchsim_rows.append('when')
    batchsim_rows.append('\t"simnum" = {{%{}%}}'.format(str(idx)))
    batchsim_rows.append("end")
#     breakpoint()

# muscle_names = new_df.iloc[:, 5:]
# muscle_activation_sets = {}

# for muscle in muscle_names:
#     muscle_activation_sets[muscle] = sorted(pandas.unique(new_df[muscle]))
#     batchsim_rows.append('"{}"= {{%{}%}}'.format(
#         muscle, "% %".join(map(str, muscle_activation_sets[muscle]))
#     ))

# muscles = []
# activations = []

# for muscle, activation in muscle_activation_sets.items():
#     muscles.append(muscle)
#     activations.append(activation)

# muscles, activations = muscle_activation_sets.items()

with open('data/asymmetrical_batchsim.pl', 'w') as f:
    f.write('\n'.join(batchsim_rows))
