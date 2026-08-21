---
output:
  word_document:
    fig_width: 4
    keep_md: yes
  html_document: default
  header-includes:
  - \usepackage{inputenc}
  - \usepackage{unicode-math}
  pdf_document:
    keep_tex: yes
    latex_engine: xelatex
---

<!--
Updated for Rceattle >= 5.10.0. What changed, and what it means for the ESP
indices this document writes:

* `quantities$ration` is now `quantities$consumption_at_age` (the annual ration
  of ONE fish, kg/yr) and `quantities$biomassByage` is
  `quantities$biomass_at_age`. Both old names now resolve to NULL.
* Population-level consumption is `consumption_at_age * avgN_at_age`, not
  ration x biomass. The old formula multiplied by weight-at-age twice (it was a
  direct port of the ADMB `biomassByage * ration2`, where ration2 is a RELATIVE
  ration in g/g). The corrected ration index is therefore lower than the 2024
  index by roughly the consumption-weighted mean weight-at-age: about 10% for
  pollock and arrowtooth, about 4x for Pacific cod. Flag this in the ESP
  submission -- it is a units fix, not a change in the model.
* `plot_*()` return ggplot objects, so figures are saved with `ggsave()` and
  annotated with layers; `jpeg()` / `dev.off()` / `mtext()` / `legend()`
  chains no longer work.
* In `plot_m_at_age()` and `plot_ration()`, colour separates the MODELS and
  line type separates the SEXES. `lty` used to key on the model.
* `M_at_age` and `B_eaten_as_prey` are REPORTed but no longer ADREPORTed, so
  the fit carries no standard errors for them (`plot_m_at_age(add_ci = TRUE)`
  says the same thing). The uncertainty block below reports NA rather than a
  number the model never produced.
* `fit_mod()`'s `phase` / `verbose` arguments moved onto `fit_control()`; see
  "GOA_25 fit models.R".
-->





# Indices


