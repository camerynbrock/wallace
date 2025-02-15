
#' @title xfer_time Transfer model to a new time
#' @description Function transfers the model generated in previous components to
#'   a new time and area using provided layers.

#' @details
#' This functions allows transferring the model created in previous
#'   components to a new time and area. The area of transfer is user provided in
#'   the map of the GUI and the transfer time user selected. The model will
#'   be transferred to the new area and time as long as the environmental
#'   variables are available for the area. This function returns a list
#'   including the cropped environmental variables used for transferring and
#'   the transferred model.
#'
#' @param evalOut ENMevaluate output from previous module and using any of
#'   the available algorithms.
#' @param curModel if algorithm is maxent, model selected by user as best
#'   or optimal, in terms of feature class and regularization multiplier (e.g
#'   'L_1'). Otherwise must be 1.
#' @param envs environmental layers of different time to be used for transferring
#'   the model. They must match the layers used for generating the model in the
#'   model component.
#' @param outputType output type to be used when algorithm is maxnet or
#'   maxent.jar.
#' @param alg modeling algorithm used in the model component. Can be one of:
#'   'bioclim', 'maxent.jar' or 'maxnet'.
#' @param xfExt extent of the area to transfer the model. This is defined by the
#'   user in the map of the GUI and is provided as a SpatialPolygons object.
#' @param clamp logical. Whether transfer will be of clamped or unclamped
#'   model.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @param spN character. Used to obtain species name for logger messages.
#' @examples
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                                        package = "wallace"),
#'                                            pattern = ".tif$",
#'                                            full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                                        package = "wallace"),
#'                                            pattern = ".tif$",
#'                                            full.names = FALSE))
#' ## extent to transfer
#' # set coordinates
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
#'                -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
#' # generate matrix
#' selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
#' polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
#'                                                  ID = 1)))
#' # load model
#' m <- readRDS(system.file("extdata/model.RDS",
#'                          package = "wallace"))
#' occsEnvs <- m@@occs
#' bgEnvs <- m@@bg
#' envsFut <- list.files(path = system.file('extdata/wc/future',
#'                                          package = "wallace"),
#'                       full.names = TRUE)
#' envsFut <- raster::stack(envsFut)
#' modXfer <- xfer_time(evalOut = m, curModel = 1,
#'                      envs = envsFut, alg = 'maxent.jar',
#'                      xfExt = polyExt, clamp = FALSE, outputType = 'cloglog')


#' @return A list of two elements: xferExt and xferTime. The first is a
#'   RasterBrick or RasterStack of the environmental variables cropped to the
#'   area of transfer. The second element is a raster of the transferred model
#'   with the specified output type.
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @seealso \code{\link[dismo]{predict}}, \code{\link{xfer_time}}
#' \code{\link{xfer_userEnvs}}
#' @export

xfer_time <- function(evalOut, curModel, envs, xfExt, alg, outputType = NULL,
                      clamp = NULL, logger = NULL, spN = NULL) {
  newPoly <- xfExt
  if (alg == 'BIOCLIM') {
    logger %>% writeLog(
      hlSpp(spN),
      'Transferring in time for BIOCLIM model.')
  } else if (alg == 'maxent.jar'| clamp == TRUE) {
    logger %>% writeLog(
      hlSpp(spN),
      'Transferring in time for clamped model ', curModel, '.')
  } else if (clamp == FALSE) {
    logger %>% writeLog(
      hlSpp(spN),
      'New time transfer for unclamped model' , curModel, '.')
  }


  smartProgress(
    logger,
    message = "Clipping environmental data to current extent...", {
    xftMsk <- raster::crop(envs, newPoly)
    xftMsk <- raster::mask(xftMsk, newPoly)
  })

  smartProgress(
    logger,
    message = ("Transferring to new time..."), {
    if (alg == 'BIOCLIM') {
      modXferTime <- dismo::predict(evalOut@models[[curModel]], xftMsk,
                                    useC = FALSE)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") outputType <- "exponential"
      modXferTime <- predictMaxnet(evalOut@models[[curModel]], xftMsk,
                                          type = outputType, clamp = clamp)
    } else if (alg == 'maxent.jar') {
      modXferTime <- dismo::predict(
        evalOut@models[[curModel]], xftMsk,
        args = c(paste0("outputformat=", outputType),
                 paste0("doclamp=", tolower(as.character(clamp)))),
        na.rm = TRUE)
    }
  })
  return(list(xferExt = xftMsk, xferTime = modXferTime))
}
