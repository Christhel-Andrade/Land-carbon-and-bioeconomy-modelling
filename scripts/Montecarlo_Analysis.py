##---Statistics calculation and plotting for LCA uncertainty analyis
##---February 2025, Christhel Andrade, INRAE

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.gridspec import GridSpec
import matplotlib.patches as mpatches
import matplotlib.lines as mlines
import matplotlib.ticker as mticker  # Correct import

# Load Excel file
file_name = "file_name.xlsx"
xls = pd.ExcelFile(file_name)

# Define impact categories and scenarios
impact_categories = xls.sheet_names
scenarios = pd.read_excel(xls, sheet_name=impact_categories[0]).columns.tolist()

# Define color palette
scenario_palette = dict(zip(scenarios, sns.color_palette("tab10", len(scenarios))))

# Define manual axis limits. -- adapt to your case
x_limits = {
    "Climate Change": (-1700, 1000),
    "Eutrophication Freshwater (P)": (-10, 2),
    "Eutrophication Marine water (N)": (-4, 10),
    "Particulate Matter": (-0.0003, 0.0012),
    "Water use": (-900, 970)
}
y_limits = {
    "Climate Change": (0, 0.015),
    "Eutrophication Freshwater (P)": (0, 5),
    "Eutrophication Marine water (N)": (0, 2.25),
    "Particulate Matter": (0, 35000),
    "Water use": (0, 0.03)
}

# Define axis labels (with units)
axis_labels = {
    "Climate Change": r"kg CO$_2$-eq",
    "Eutrophication Freshwater (P)": "kg P-eq.",
    "Eutrophication Marine water (N)": "kg N-eq.",
    "Particulate Matter": r"disease incidence",
    "Water use": r"m$^3$ world-eq deprived"
}

# Initialize storage
mc_data = []
original_values = []

# Extract data
for impact in impact_categories:
    df = pd.read_excel(xls, sheet_name=impact)
    original_impact = df.iloc[1]  # Original impact values
    df_montecarlo = df.iloc[2:]  # Monte Carlo results

    for scenario in scenarios:
        values = df_montecarlo[scenario].dropna().values
        mc_data.append({"Scenario": scenario, "Impact": impact, "Values": values})
        original_values.append({"Scenario": scenario, "Impact": impact, "Original": original_impact[scenario]})

# Convert to DataFrames
df_mc = pd.DataFrame(mc_data)
df_original = pd.DataFrame(original_values)

# Create Figure Layout (More Square Panels)
fig = plt.figure(figsize=(30, 25))
gs = GridSpec(3, len(impact_categories), figure=fig, height_ratios=[1.2, 1.2, 1.5])

from scipy.stats import gaussian_kde

# Plot KDEs
for i, impact in enumerate(impact_categories):
    ax = fig.add_subplot(gs[0, i])
    subset = df_mc[df_mc["Impact"] == impact]

    for scenario in scenarios:
        scenario_data = subset[subset["Scenario"] == scenario]["Values"].values[0]
        sns.kdeplot(scenario_data, ax=ax, color=scenario_palette[scenario], fill=True, alpha=0.3, bw_adjust=1)

        # Compute KDE density estimate
        kde = gaussian_kde(scenario_data)
        x_vals = np.linspace(min(scenario_data), max(scenario_data), 1000)
        y_vals = kde(x_vals)

        original_value = df_original[(df_original["Impact"] == impact) & (df_original["Scenario"] == scenario)]["Original"].values[0]
        y_original = kde(original_value)
        
        ax.scatter(original_value, y_original, color=scenario_palette[scenario], marker="o", s=80, edgecolor='none', linewidth=1.2)

    # Set manual limits
    ax.set_xlim(x_limits[impact])
    ax.set_ylim(y_limits[impact])
 
 # Set y-axis label only for the first subplot in the row
    if i == 0:
        ax.set_ylabel("Density", fontsize=25)
    else:
        ax.set_ylabel("")  # Remove y-axis label from other subplots
    
    # Set x-axis label for all
    ax.set_xlabel(axis_labels[impact], fontsize=24)
    ax.set_title(impact, fontsize=28)
    ax.xaxis.set_tick_params(labelsize=16)  # X-axis tick labels
    ax.yaxis.set_tick_params(labelsize=16)  # Y-axis tick labels
    ax.grid(True, linestyle="--", alpha=0.5)
    
    # Apply scientific notation
    ax.xaxis.set_major_formatter(mticker.ScalarFormatter(useMathText=True))
    ax.ticklabel_format(style="sci", axis="x", scilimits=(-2, 2))
    
  # Plot CDFs
for i, impact in enumerate(impact_categories):
    ax = fig.add_subplot(gs[1, i])
    subset = df_mc[df_mc["Impact"] == impact]

    for scenario in scenarios:
        scenario_data = subset[subset["Scenario"] == scenario]["Values"].values[0]
        sorted_data = np.sort(scenario_data)
        cdf = np.linspace(0, 1, len(sorted_data))
        ax.plot(sorted_data, cdf, color=scenario_palette[scenario], linewidth=2)

        # Original value as 'O' marker
        original_value = df_original[(df_original["Impact"] == impact) & (df_original["Scenario"] == scenario)]["Original"].values[0]
        original_prob = np.interp(original_value, sorted_data, cdf)
        ax.scatter(original_value, original_prob, color=scenario_palette[scenario], marker="o", s=80, edgecolor='none', linewidth=1.2) #color="black", marker="x", s=50)
       
    # Set manual limits
    ax.set_xlim(x_limits[impact])
    ax.set_ylim(0, 1.1)

    # Axis labels
   # ax.set_xlabel(axis_labels[impact], fontsize=26)
    #ax.set_ylabel("Cumulative Probability", fontsize=26)
    
    # Set y-axis label only for the first subplot in the row
    if i == 0:
        ax.set_ylabel("Cumulative Probability", fontsize=25)
    else:
        ax.set_ylabel("")  # Remove y-axis label from other subplots
    
    # Set x-axis label for all
    ax.set_xlabel(axis_labels[impact], fontsize=24)
    ax.xaxis.set_tick_params(labelsize=16)  # X-axis tick labels
    ax.yaxis.set_tick_params(labelsize=16)  # Y-axis tick labels
    ax.grid(True, linestyle="--", alpha=0.5)
    
    # Apply scientific notation
    ax.xaxis.set_major_formatter(mticker.ScalarFormatter(useMathText=True))
    ax.ticklabel_format(style="sci", axis="x", scilimits=(-2, 2))

# Prepare Boxplot Data
variability_data = []
original_values_dict = {}

for impact in impact_categories:
    df = pd.read_excel(xls, sheet_name=impact, header=None)
    df.columns = df.iloc[0]
    df = df[1:].reset_index(drop=True)

    # Store Original Values
    original_values_dict[impact] = df.iloc[0].to_dict()
    original_values_dict[impact] = {key: pd.to_numeric(value, errors="coerce") for key, value in original_values_dict[impact].items()}

    df_montecarlo = df.iloc[2:]

    for scenario in scenarios:
        values = df_montecarlo[scenario].dropna().values
        median_value = np.median(values)
        q1, q3 = np.percentile(values, [25, 75])  # IQR
        ci_low, ci_high = np.percentile(values, [2.5, 97.5])  # 95% CI
        sd_value = np.std(values)
        variability_data.append({
            "Impact": impact,
            "Scenario": scenario,
            "Values": values,
            "Median": median_value,
            "Q1": q1,
            "Q3": q3,
            "CI_low": ci_low,
            "CI_high": ci_high,
            "SD": sd_value
        })

# Convert Variability Data to DataFrame
variability_df = pd.DataFrame(variability_data)

# Plot Boxplots
for i, impact in enumerate(impact_categories):
    ax = fig.add_subplot(gs[2, i])
    all_values = []
    scenarios_list = []

    for row in variability_data:
        if row["Impact"] == impact:
            all_values.extend(row["Values"])
            scenarios_list.extend([row["Scenario"]] * len(row["Values"]))

    df_boxplot = pd.DataFrame({"Scenario": scenarios_list, "Values": all_values})
    sns.boxplot(x="Scenario", y="Values", data=df_boxplot, ax=ax, hue="Scenario", palette=scenario_palette, fliersize=3, linewidth=1, width=0.7, showfliers=True, legend=False, flierprops={"marker": "D", "markerfacecolor": "black", "markeredgecolor": "black", "markersize": 3},)

    
    # Overlay Original Value
    for j, scenario in enumerate(scenarios):
        original_value = original_values_dict[impact].get(scenario, None)
        if original_value is not None:
            ax.scatter(j, original_value, color="black", s=50, marker="o", zorder=3)
       
    
    # **Plot Standard Deviation (SD) Markers**
    for j, scenario in enumerate(scenarios):
        scenario_data = variability_df[(variability_df["Impact"] == impact) & (variability_df["Scenario"] == scenario)]
        if not scenario_data.empty:
            x_pos = j  # Align SD with scenario position
            sd_value = scenario_data["SD"].values[0]
            ax.scatter(x_pos, sd_value, color="red", edgecolors="none", s=50, marker="D", label="Standard Deviation" if (i == 0 and j == 0) else "")

    # **Add Confidence Interval (CI) as a Light Shaded Area**
    for j, scenario in enumerate(scenarios):
        scenario_data = variability_df[(variability_df["Impact"] == impact) & (variability_df["Scenario"] == scenario)]
        if not scenario_data.empty:
            x_pos = j  
            ci_low, ci_high = scenario_data["CI_low"].values[0], scenario_data["CI_high"].values[0]

            ax.fill_between(
                [x_pos - 0.3, x_pos + 0.3],  
                [ci_low, ci_low], [ci_high, ci_high],  
                color=scenario_palette[scenario],
                alpha=0.2
            )    
                 

    # Set manual limits
    ax.set_ylim(x_limits[impact])  # Use x_limits here because Boxplot has y on the y-axis

    # Axis labels
    ax.set_ylabel(axis_labels[impact], fontsize=25)
    ax.set_xlabel("")
    ax.xaxis.set_tick_params(labelsize=20)  # X-axis tick labels
    ax.yaxis.set_tick_params(labelsize=16)  # Y-axis tick labels
    ax.grid(True, axis='y', linestyle="--", alpha=0.5)

# Restore Thin Gray Borders for Subplots
for ax in fig.get_axes():
    for spine in ax.spines.values():
        spine.set_linewidth(0.5)  # Thin border
        spine.set_color("gray")  # Gray color


# Adjust Layout for Square Shape
plt.tight_layout(rect=[0, 0.1, 1, 1])

# Add Unified Legend at the Bottom
legend_patches = [mpatches.Patch(color=scenario_palette[scn], label=scn) for scn in scenarios]
#legend_lines = [mlines.Line2D([], [], color='black', linestyle="dashed", label="Original Value")]
sd_legend = [mlines.Line2D([], [], color="red", marker="D", linestyle="None", markersize=10, label="Standard Deviation")]
ci_legend = [mpatches.Patch(color="gray", alpha=0.5, label="Confidence Interval")]
original_legend = [mlines.Line2D([], [], color="black", marker="o", linestyle="None", markersize=10, label="Original LCA Result")]

# Place legend at the bottom
fig.legend(
    handles=legend_patches + sd_legend + ci_legend + original_legend,  
    title="Legend",
    loc="lower center",
    ncol=len(scenarios) // 2 + 2,  # Spread legend horizontally
    bbox_to_anchor=(0.5, 0.01),
    fontsize=26,
    title_fontsize=28,  # Bigger title
    frameon=True
)

# Save and Show
plt.savefig("montecarlo_KDE_CDF_Box3.png", dpi=500, bbox_inches="tight")
plt.show()
