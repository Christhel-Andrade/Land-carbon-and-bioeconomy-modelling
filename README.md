# Land carbon and bioeconomy modelling
Scripts illustrating workflows for soil carbon modelling, bioeconomy scenarios, spatial data processing, and LCA uncertainty analysis.

This repository provides selected scripts illustrating workflows for soil organic carbon modelling, spatial data processing, bioeconomy scenario construction, life cycle assessment (LCA), and uncertainty analysis.

---
## 1. Soil organic carbon scenario analysis

**Related work:**
Andrade Díaz et al., 2023, - https://doi.org/10.1016/j.apenergy.2022.120192 
Dataset: Andrade Díaz et al., 2022, https://doi.org/10.48531/JBRU.CALMIP/AUEEEJ

- `AMG_outputs.R`  
  Post-processes AMG soil carbon model outputs to analyze changes in soil organic carbon under different management and biomass mobilization scenarios.  
  The script evaluates changes across time horizons, spatial units, and scenarios, aggregating soil carbon changes at national scale.

- `Average_delta_surface.R`  
  Calculates percentage changes in soil organic carbon across spatial units and applies Jenks natural breaks classification to support map interpretation. Some classification thresholds are adapted to the case study and may require adjustment for other datasets.

---
## 2. Bioeconomy scenarios and carbon input modelling

**Related work:** 
Andrade Díaz et al., 2023 - https://doi.org/10.21203/rs.3.rs-3086337/v1
Andrade Díaz et al., 2025 - https://doi.org/10.1016/j.dib.2025.111910
Dataset: Andrade Díaz et al., 2022 - https://doi.org/10.48531/JBRU.CALMIP/VLKG8V

- `C_in_calcul.R`  
  Calculates carbon inputs to soil at simulation-unit level based on crop type and yield.  
  The workflow estimates crop residue production using crop-specific allometric or residue-to-product relationships, calculates residue carbon, and distinguishes baseline soil inputs from bioeconomy scenarios. For bioeconomy scenarios, it estimates carbon mobilized for various pathways, calculates conversion into products and coproducts, and estimates carbon returned to soil through scenario-specific return and application ratios.

- `Run_C_in_calcul.R`  
  Runs the carbon input calculation workflow across scenarios. Designed to execute calculations efficiently using parallel processing across available computing cores.

- `PCU_creation.R`  
  Prepares simulation units for spatially explicit soil carbon modelling. The script integrates GIS-derived shapefile attributes, agricultural statistics, crop rotations, yields, and climate projections to structure the inputs required by the soil carbon model.

- `ETP_Thornthwaite_Ec.R`  
  Calculates potential evapotranspiration using the Thornthwaite method from temperature and precipitation data. Produces spatially and temporally explicit evapotranspiration estimates at national.  
  
---
## 3. LCA and uncertainty analysis

- `GSA-LCA.ipynb`  
  Parametric LCA workflow in Brightway. Runs one-at-a-time sensitivity analysis and Monte Carlo simulation.

- `Montecarlo_Analysis.py`  
  Post-processes Monte Carlo simulation outputs by calculating summary statistics and generating plots to support interpretation of uncertainty in LCA results.

---

## Notes

- Some scripts depend on external datasets, model outputs, or file structures not included here.
- Some visualization parameters or classification thresholds are case-study specific and may need adjustment for other applications.

---

## License

This project is released under the MIT License. See LICENSE for details.
