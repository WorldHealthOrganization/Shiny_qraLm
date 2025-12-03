# Tool Description

This WHO Risk Estimation Tool for *Listeria monocytogenes* in Foods was
developed to support the quantitative estimation of listeriosis risk per
serving across several food commodities. It currently supports risk
assessments for three food products:

- **Frozen Vegetables:** This model focuses on processing beginning at
  the freezing facility, with the assumption that incoming vegetables
  have already been preconditioned (e.g., trimmed, peeled, washed). The
  model includes blanching, potential cross-contamination during
  freezing and packaging, and consumer handling activities such as
  defrosting and cooking.
- **RTE Cold-Smoked Fish:** The model targets RTE cold-smoked fish
  produced via brining, followed by smoking, slicing, and packing. It
  also accounts for cold-chain storage and consumer handling.
- **RTE Cantaloupe:** The model for RTE cantaloupe includes stages from
  pre-harvesting, harvesting, cleaning, and washing to processing,
  cold-chain storage, and consumer handling.

Stochastic modeling is used to simulate the risk of illness from
*Listeria monocytogenes*, accounting for both within-lot variability
(differences between individual units in a single production lot) and
between-lot variability (differences across production lots). For each
food commodity, the model generates a matrix of production lots, with
each lot comprising multiple individual units. Contamination levels are
assigned using probability distributions.

Risk is estimated for each unit, then averaged across the units to
produce a lot-level risk. Subsequently, the average or median risk per
serving is calculated across all lots, providing an overall estimate
used for comparison and scenario analysis. Users can compare multiple
dose-response models that incorporate virulence classes plus demographic
factors (age, sex), improving population-specific accuracy. Testing
strategies and sampling plans can also be explored to evaluate how
different interventions affect risk estimates, supporting informed
decision-making under varied production and surveillance conditions.

Because of storage limits on the hosted deployment, certain parameters
are bounded. Install the app locally to explore the full models and
parameter space. The source code is openly accessible in the following
[GitHub
repository:](https://github.com/WorldHealthOrganization/Shiny_qraLm)

### References & Supporting Material

- `qraLm` function reference:
  <https://github.com/WorldHealthOrganization/qraLm>
- FAO/WHO Summary Report: <https://www.fao.org/3/cc6993en/cc6993en.pdf>
- Joint FAO/WHO Expert meeting on *L. monocytogenes* (Part 2 risk
  assessment):
  <https://openknowledge.fao.org/server/api/core/bitstreams/7be15013-c4a0-4fc3-9088-4db8a2fe6a43/content>
