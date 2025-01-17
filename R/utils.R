#' Convert Between Absorbance and Transmittance
#'
#' @description These functions allow for the convenient conversion between
#'   \%Transmittance and Absorbance units for the Y axis.
#'
#'   Converting between \%Transmittance and absorbance units for the Y axis is
#'   not a simple flipping of axis or inversion. Instead, the two are related by
#'   the following formulas:
#'
#' \deqn{
#'  A=-log_{10}(\tfrac{\%T}{100})
#' }
#'   and
#' \deqn{
#'  \%T=10^{-A}\cdot 100
#' }.
#'
#'   Ces fonctions permettent une conversion pratique entre les unités
#'   \%Transmittance et Absorbance pour l'axe Y. La conversion entre les unités
#'   \%Transmittance et Absorbance pour l'axe Y n'est pas un simple retournement
#'   d'axe ou une inversion. Au lieu de cela, les deux sont liés par les
#'   formules suivantes :
#'
#' \deqn{
#'  A=-log_{10}(\tfrac{\%T}{100})
#' }
#'   and
#' \deqn{
#'  \%T=10^{-A}\cdot 100
#' }
#'
#' @param ftir A data.frame of FTIR spectral data including column to be
#'   converted. Can't contain both `absorbance` and `transmittance` column as
#'   the receiving column would be overwritten
#'
#'   Un data.frame de données spectrales IRTF incluant la colonne à convertir.
#'   Ne peut pas contenir les colonnes `absorbance` et `transmittance` car la
#'   colonne de réception serait écrasée.
#'
#' @return a data.frame of FTIR spectral data with conversion between absorbance
#'   or transmittance as requested. Note the original data column is removed
#'   since FTIR spectral data frames can't be fed into plotting functions with
#'   both transmittance and absorbance data included.
#'
#'   un data.frame de données spectrales IRTF avec conversion entre l'absorbance
#'   ou la transmittance comme demandé. Notez que la colonne de données
#'   d'origine est supprimée car les trames de données spectrales IRTF ne
#'   peuvent pas être introduites dans les fonctions de tracé avec les données
#'   de transmittance et d'absorbance incluses.
#'
#' @examples
#' # Convert from absorbance to transmittance
#' sample_spectra_transmittance <- absorbance_to_transmittance(sample_spectra)
#'
#' # Convert back to absorbance
#' sample_spectra_absorbance <- transmittance_to_absorbance(sample_spectra_transmittance)
#'
#' @name conversion
NULL

#' @export
#' @rdname conversion
absorbance_to_transmittance <- function(ftir) {
  ftir <- check_ftir_data(ftir, "PlotFTIR::absorbance_to_transmittance")
  if (!("absorbance" %in% colnames(ftir))) {
    cli::cli_abort("Error in {.fn PlotFTIR::absorbance_to_transmittance}. {.arg ftir} must contain a {.var absorbance} column.")
  }
  ftir$transmittance <- (10^(ftir$absorbance * -1)) * 100
  ftir$absorbance <- NULL

  ftir <- ftir[, c("wavenumber", "transmittance", "sample_id")]

  return(ftir)
}

#' @export
#' @rdname conversion
transmittance_to_absorbance <- function(ftir) {
  ftir <- check_ftir_data(ftir, "PlotFTIR::transmittance_to_absorbance")
  if (!("transmittance" %in% colnames(ftir))) {
    cli::cli_abort("Error in {.fn PlotFTIR::transmittance_to_absorbance}. {.arg ftir} must contain a {.var transmittance} column.")
  }

  ftir$absorbance <- -log(ftir$transmittance / 100, base = 10)
  ftir$transmittance <- NULL

  ftir <- ftir[, c("wavenumber", "absorbance", "sample_id")]

  return(ftir)
}

#' Get Plot Sample IDs
#'
#' @description Get the sample IDs from a prepared plot. Useful if renaming in
#'   the plot legend.
#'
#'   Obtenez les ID d’échantillon à partir d’un tracé préparé. Utile si vous
#'   renommez dans la légende de le tracé
#'
#' @param ftir_spectra_plot A plot generated by [plot_ftir()] or
#'   [plot_ftir_stacked()].
#'
#'   Un tracé généré par [plot_ftir()] ou [plot_ftir_stacked()].
#'
#' @return A vector of factors corresponding to the sample IDs in the plot.
#'
#'   unvecteur de facteurs correspondant aux ID d'échantillon dans le tracé
#' @export
#'
#' @seealso [rename_plot_sample_ids()]
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Prepare a plot
#'   p <- plot_ftir(biodiesel)
#'
#'   # Get the Sample IDs
#'   get_plot_sample_ids <- (p)
#' }
get_plot_sample_ids <- function(ftir_spectra_plot) {
  # Package Checks
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ggplot2} package installation.",
      i = "Install {.pkg ggplot2} with {.code install.packages('ggplot2')}"
    ))
  }
  if (!ggplot2::is.ggplot(ftir_spectra_plot)) {
    cli::cli_abort("Error in {.fn PlotFTIR::get_plot_sample_ids}. {.arg ftir_spectra_plot} must be a ggplot object. You provided {.obj_type_friendly {ftir_spectra_plot}}.")
  }
  return(as.factor(unique(ftir_spectra_plot$data$sample_id)))
}

#' @title Check FTIR Data
#'
#' @description Check provided FTIR dataframe is appropriate for manipulation or plotting
#' Not typically called directly, but as a function in data integrety check process before
#' further calculation or plotting happens
#'
#' @param ftir A data.frame of FTIR spectral data including column to be
#'  converted. Can't contain both `absorbance` and `transmittance` column.
#'
#' @param fn The name of the function, used in printing error codes.
#'
#' @return invisible ftir data if ok, typically called for effect of failure.
#' @keywords internal
check_ftir_data <- function(ftir, fn) {
  if ("ir" %in% class(ftir)) {
    cli::cli_inform("Converting {.pkg ir} data to {.pkg PlotFTIR} structure.")
    ftir <- ir_to_plotftir(ftir)
  }

  if ("Spectra" %in% class(ftir)) {
    cli::cli_inform("Converting {.pkg ChemoSpec} data to {.pkg PlotFTIR} structure.")
    ftir <- chemospec_to_plotftir(ftir)
  }

  if (!(is.data.frame(ftir))) {
    cli::cli_abort("Error in {.fn {fn}}. {.arg ftir} must be a data frame. You provided {.obj_type_friendly ftir}.")
  }
  if (!("sample_id" %in% colnames(ftir))) {
    cli::cli_abort(c("Error in {.fn {fn}}. {.arg ftir} is missing a column.",
      i = "It must contain a column named {.var sample_id}."
    ))
  }
  if (!("wavenumber" %in% colnames(ftir))) {
    cli::cli_abort(c("Error in {.fn {fn}}. {.arg ftir} is missing a column.",
      i = "It must contain a column named {.var wavenumber}."
    ))
  }
  if (!any(colnames(ftir) == "absorbance", colnames(ftir) == "transmittance")) {
    cli::cli_abort("Error in {.fn {fn}}. {.arg ftir} must have one of {.var absorbance} or {.var transmittance} columns.")
  }
  if ("absorbance" %in% colnames(ftir) && "transmittance" %in% colnames(ftir)) {
    cli::cli_abort("Error in {.fn {fn}}. {.arg ftir} cannot contain both {.var absorbance} and {.var transmittance} columns.")
  }
  if (any(!(colnames(ftir) %in% c("sample_id", "wavenumber", "absorbance", "transmittance")))) {
    cli::cli_abort("Error in {.fn {fn}}. {.arg ftir} may only contain columns {.var sample_id}, {.var wavenumber}, and one of {.var absorbance} or {.var transmittance}.")
  }

  invisible(ftir)
}
