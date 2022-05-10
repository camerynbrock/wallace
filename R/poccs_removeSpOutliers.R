
#' @title poccs_removeSpOutliers Remove spatial outliers
#' @description The function removes spatial outliers from species occurrence
#'   points.
#'
#' @details
#' This function is called by the component Process Occurrence Data to remove
#'   spatial outliers from the species occurrence points. The user determines
#'   the method and p-value. The output is a revised data set of species
#'   occurrence points with spatial outliers omitted.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data
#' @param method the method for finding outlying occurrence data. Options are
#'   'iqr', 'grubbs', 'dixon', 'rosner'
#' @param pval user-specified p-value for assessing the significance of Grubbs
#'   test statistic.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component occs:
#'   Obtain occurrence data. Used to obtain species name for logger messages.
#' @param kRosner integer between 1 and 10. Determines the number of outliers
#'   suspected with a Rosner test. The value has no effect unless ‘method=’rosner''.
#'
#' @examples
#' occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
#'                              package = "wallace"))
#' revised_occs <- poccs_removeSpOutliers(occs = occs)
#'
#'
#' @return Output is a data frame of revised occurences with the same columns
#'   as occs
#' @author Cameryn Brock <cbrock@@conservation.org>
#' @seealso occOutliers package by Cory Merow
#' @export

poccs_removeSpOutliers <- function(occs, method, pval, kRosner = NULL,
                                   logger = NULL, spN = NULL) {

  if(is.null(occs)) {
    logger %>%
      writeLog(type = 'error',
               "Before processing occurrences, obtain the data in component 1.")
    return()

  } else {

    # Find outliers
    smartProgress(
      logger,
      message = paste0("Finding spatial outliers for ", spName(spN), "..."),
      {
        # get Spatial Points of occurrence records
        coords <- data.frame(
          x = occs$longitude,
          y = occs$latitude) %>%
          sp::SpatialPoints()

        # find outliers
        outliers <- occOutliers::findSpatialOutliers(
          pres = coords,
          pvalSet = pval,
          method = method,
          checkPairs = FALSE)

        # remove outliers from occs
        occs_revised <- occs %>%
          dplyr::filter(!dplyr::row_number() %in% outliers)
      }
    )

    logger %>%
      writeLog(
        hlSpp(spN),
        "Removed ", length(outliers), " spatial outliers. ",
        "Updated data has n = ", nrow(occs_revised), " records.")

    if(nrow(occs_revised) < 4) {
      logger %>%
        writeLog(
          type = "error",
          hlSpp(spN),
          "After removing occurremces, there are three or less points. ",
          "You need more occurrences to continue the analysis.")

      return()
    }

    if(nrow(occs) > 30 & method == "dixon") {
      logger %>%
        writeLog(
          type = "error",
          hlSpp(spN),
          "The Dixon test only applies to sample sizes between [3,30]. ",
          "Please select another method for your analysis.")

      return()
    }

    return(occs_revised)
  }
}

