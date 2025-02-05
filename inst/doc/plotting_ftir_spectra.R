## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  warning = FALSE,
  message = FALSE,
  error = TRUE,
  out.width = "90%",
  fig.width = 6,
  fig.height = 4
)

options(rmarkdown.html_vignette.check_title = FALSE)

library(PlotFTIR)

tempdir <- withr::local_tempdir()

for (i in seq_along(unique(sample_spectra$sample_id))) {
  write.csv(sample_spectra[sample_spectra$sample_id == unique(sample_spectra$sample_id)[i], c("wavenumber", "absorbance")],
    file = file.path(tempdir, paste0(unique(sample_spectra$sample_id)[i], ".csv")),
    row.names = FALSE
  )
}

file_directory <- tempdir

## ----installation_Cran, eval = FALSE------------------------------------------
# install.packages("PlotFTIR")

## ----installation_github, eval = FALSE----------------------------------------
# devtools::install_github("NRCan/PlotFTIR")

## ----library_call, eval = FALSE-----------------------------------------------
# library(PlotFTIR)

## ----read_files_en------------------------------------------------------------
spectra <- read_ftir_directory(
  path = file_directory,
  files = c("toluene.csv", "heptanes.csv", "isopropanol.csv", "paper.csv", "polystyrene.csv"),
  sample_names = c("toluene", "heptanes", "isopropanol", "paper", "polystyrene")
)

head(spectra)

## ----basicplot_en-------------------------------------------------------------
plot_ftir(spectra)

## ----offsetplot_en------------------------------------------------------------
plot_ftir_stacked(spectra)

## ----titles_en----------------------------------------------------------------
plot_ftir(spectra, plot_title = c("Title of my Spectra Plot", "Plotted with PlotFTIR"), legend_title = "Samples:") |>
  move_plot_legend(position = "bottom")

## ----rename_samples_en--------------------------------------------------------
plot_ftir(spectra) |>
  rename_plot_sample_ids(sample_ids = c("Methylbenzene" = "toluene", "C7" = "heptanes"))

## ----compress_en--------------------------------------------------------------
plot_ftir(spectra) |>
  compress_low_energy(cutoff = 1850, compression_ratio = 5)

## ----r_zoom_en----------------------------------------------------------------
plot_ftir(spectra) |>
  zoom_in_on_range(c(3600, 2600))

## ----markers_en---------------------------------------------------------------
plot_ftir(spectra) |>
  add_wavenumber_marker(
    wavenumber = 1495,
    text = "C-C Aromatic",
    line_aesthetics = list("linetype" = "dashed"),
    label_aesthetics = list("color" = "#7e0021")
  )

## ----combo_en-----------------------------------------------------------------
plot_ftir(spectra, plot_title = c("My FTIR Plot", "Closeup of Detailed Region 1600 to 800 wavenumbers"), legend_title = "Samples:") |>
  move_plot_legend(position = "bottom") |>
  zoom_in_on_range(zoom_range = c(1600, 800)) |>
  add_wavenumber_marker(wavenumber = 1495, text = "C-C Aromatic", line_aesthetics = c(color = "#7e0021", linetype = "dotted")) |>
  add_wavenumber_marker(wavenumber = 817, text = "C-C-O\nsymmetric", line_aesthetics = c(color = "#ff420e", linetype = "dotted")) |>
  add_wavenumber_marker(wavenumber = 1380, text = "CH3", line_aesthetics = c(linetype = "dashed")) |>
  rename_plot_sample_ids(c("C7 Alkane" = "heptanes", "2-Propanol" = "isopropanol", "Toluene" = "toluene"))

## ----transmittance_en---------------------------------------------------------
transmittance_spectra <- absorbance_to_transmittance(spectra)

plot_ftir(transmittance_spectra)

## ----scalar_en----------------------------------------------------------------
shifted_spectra <- add_scalar_value(ftir = spectra, value = 0.2, sample_ids = c("heptanes", "toluene"))

plot_ftir(shifted_spectra)

## ----plot_biodiesel_en--------------------------------------------------------
plot_ftir(biodiesel) |>
  zoom_in_on_range(c(2000, 1000))

## ----plot_biodiesel_baseline_1_en---------------------------------------------
biodiesel |>
  recalculate_baseline(method = "average", wavenumber_range = c(2000, 1900), individually = TRUE) |>
  plot_ftir() |>
  zoom_in_on_range(c(2000, 1000))

## ----plot_biodiesel_baseline_2_en---------------------------------------------
biodiesel |>
  recalculate_baseline(method = "point", wavenumber_range = 1250, individually = TRUE) |>
  plot_ftir() |>
  zoom_in_on_range(c(2000, 1000))

## ----plot_biodiesel_baseline_3_en---------------------------------------------
biodiesel |>
  recalculate_baseline(method = "minimum", wavenumber_range = c(1300, 1000), individually = FALSE) |>
  plot_ftir() |>
  zoom_in_on_range(c(2000, 1000))

## ----save_spectra, eval = FALSE-----------------------------------------------
# biodiesel |>
#   ploft_ftir() |>
#   save_plot(filename = "my_ftir_plot.png", filetype = ".png")

## ----clear_en,echo = FALSE, warning=FALSE, message=FALSE----------------------
# This just clears out items from the environment to make sure later french code is fresh.
rm(spectra, shifted_spectra, transmittance_spectra)

## ----installation_Cran_fr, eval = FALSE---------------------------------------
# install.packages("PlotFTIR")

## ----installation_github_fr, eval = FALSE-------------------------------------
# devtools::install_github("NRCan/PlotFTIR")

## ----library_call_fr, eval = FALSE--------------------------------------------
# library(PlotFTIR)

## ----read_files_fr------------------------------------------------------------
spectres <- read_ftir_directory(
  path = file_directory,
  files = c("toluene.csv", "heptanes.csv", "isopropanol.csv", "paper.csv", "polystyrene.csv"),
  sample_names = c("toluene", "heptanes", "isopropanol", "paper", "polystyrene")
)

head(spectres)

## ----basicplot_fr-------------------------------------------------------------
plot_ftir(spectres, lang = "fr")

## ----offsetplot_fr------------------------------------------------------------
plot_ftir_stacked(spectres, lang = "fr")

## ----titles_fr----------------------------------------------------------------
plot_ftir(spectres, lang = "fr", plot_title = c("Titre de mon tracé de spectres", "Tracé avec PlotFTIR"), legend_title = "Echantillons:") |>
  move_plot_legend(position = "bottom")

## ----rename_samples_fr--------------------------------------------------------
plot_ftir(spectres, lang = "fr") |>
  rename_plot_sample_ids(sample_ids = c("methylbenzene" = "toluene", "C7" = "heptanes"))

## ----compress_fr--------------------------------------------------------------
plot_ftir(spectres, lang = "fr") |>
  compress_low_energy(cutoff = 1850, compression_ratio = 5)

## ----r_zoom_fr----------------------------------------------------------------
plot_ftir(spectres, lang = "fr") |>
  zoom_in_on_range(c(3600, 2600))

## ----markers_fr---------------------------------------------------------------
plot_ftir(spectres, lang = "fr") |>
  add_wavenumber_marker(
    wavenumber = 1495,
    text = "C-C aromatique",
    line_aesthetics = list("linetype" = "dashed"),
    label_aesthetics = list("color" = "#7e0021")
  )

## ----combo_fr-----------------------------------------------------------------
plot_ftir(spectres, lang = "fr", plot_title = c("Mon Tracé IRTF", "Gros plan de la région détaillée de 1600 à 800 nombres d'ondes"), legend_title = "Samples:") |>
  move_plot_legend(position = "bottom") |>
  zoom_in_on_range(zoom_range = c(1600, 800)) |>
  add_wavenumber_marker(wavenumber = 1495, text = "C-C aromatique", line_aesthetics = c(color = "#7e0021", linetype = "dotted")) |>
  add_wavenumber_marker(wavenumber = 817, text = "C-C-O\nsymétrique", line_aesthetics = c(color = "#ff420e", linetype = "dotted")) |>
  add_wavenumber_marker(wavenumber = 1380, text = "CH3", line_aesthetics = c(linetype = "dashed")) |>
  rename_plot_sample_ids(c("C7 alcane" = "heptanes", "2-Propanol" = "isopropanol", "Toluene" = "toluene"))

## ----transmittance_fr---------------------------------------------------------
spectres_transmittance <- absorbance_to_transmittance(spectres)
plot_ftir(spectres_transmittance, lang = "fr")

## ----scalar_fr----------------------------------------------------------------
spectres_decales <- add_scalar_value(ftir = spectres, value = 0.2, sample_ids = c("heptanes", "toluene"))

plot_ftir(spectres_decales, lang = "fr")

## ----plot_biodiesel_fr--------------------------------------------------------
plot_ftir(biodiesel, lang = "fr") |>
  zoom_in_on_range(c(2000, 1000))

## ----plot_biodiesel_baseline_1_fr---------------------------------------------
biodiesel |>
  recalculate_baseline(method = "average", wavenumber_range = c(2000, 1900), individually = TRUE) |>
  plot_ftir(lang = "fr") |>
  zoom_in_on_range(c(2000, 1000))

## ----plot_biodiesel_baseline_2_fr---------------------------------------------
biodiesel |>
  recalculate_baseline(method = "point", wavenumber_range = 1250, individually = TRUE) |>
  plot_ftir(lang = "fr") |>
  zoom_in_on_range(c(2000, 1000))

## ----plot_biodiesel_baseline_3_fr---------------------------------------------
biodiesel |>
  recalculate_baseline(method = "minimum", wavenumber_range = c(1300, 1000), individually = FALSE) |>
  plot_ftir(lang = "fr") |>
  zoom_in_on_range(c(2000, 1000))

## ----save_spectra_fr, eval = FALSE--------------------------------------------
# biodiesel |>
#   ploft_ftir() |>
#   save_plot(filename = "mon_trace_irtf.png", filetype = ".png")

