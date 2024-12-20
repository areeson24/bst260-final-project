Steps to reproduce results:
1) Download census population estimates and projections data that is already contained in `raw-data/`.
2) Run `code/wrangling.R` script to produce cleaned dataset for analysis.
3) Run `code/analysis.qmd` to generate figures and tables (which are stored in `plots/` as rds objects), in addition to model outputs and results.
4) Run `code/final-project.qmd` to produce final paper that loads in rds objects from `plots/`.
5) Run `code/supp-materials.qmd` to produce supplementary tables and figures that also loads in rds objects from `plots/`.
