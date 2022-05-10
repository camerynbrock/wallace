poccs_removeSpOutliers_module_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    selectInput(
      inputId = ns("outlier_method"),
      label = "Method for detecting outliers:",
      choices = c("grubbs",
                  "iqr",
                  "dixon",
                  "rosner"),
      selected = "grubbs"),

    conditionalPanel(
      condition = paste0("input['", ns("outlier_method"), "'] == 'grubbs' "),
      numericInput(
        inputId = ns("outlier_pval"),
        label = "P-value for assessing significance:",
        value = NULL)
    ),

    conditionalPanel(
      condition = paste0("input['", ns("outlier_method"), "'] == 'rosner' "),
      numericInput(
        inputId = ns("outlier_krosner"),
        label = "Number of outliers suspected (kRosner):",
        value = NULL,
        min = 1,
        max = 10,
        step = 1)
    ),

    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
    ),

    # UI
    actionButton(ns("goRemoveSpOutliers"), "Remove Outliers"),

    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetOccs"), "Reset", class = 'butReset'),
    strong(" to original occurrence")
  )
}

poccs_removeSpOutliers_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  allSp <- common$allSp

  observeEvent(input$goRemoveSpOutliers, {

    # loop over all species if batch is on
    if (input$batch == TRUE) {spLoop <- allSp()} else {spLoop <- curSp()}

    for (sp in spLoop) {

      # FUNCTION CALL ####

      revised_occs <- poccs_removeSpOutliers(
        occs = spp[[sp]]$occs,
        method = input$outlier_method,
        pval = input$outlier_pval,
        kRosner = input$outlier_krosner,
        logger = logger,
        spN = sp)

      req(revised_occs)

      # LOAD INTO SPP ####
      # record present occs before removing outliers
      spp[[sp]]$procOccs$occsPreRm <- spp[[sp]]$occs
      spp[[sp]]$occs <- revised_occs
      spp[[sp]]$procOccs$occsNoSpOutliers <- revised_occs

      # REFERENCES ####
      knitcitations::citep(citation("occOutliers"))
      knitcitations::citep(citation("outliers"))
      knitcitations::citep(citation("EnvStats"))

      # METADATA ####
      spp[[sp]]$rmm$code$wallace$outlierMethod <- input$outlier_method
      spp[[sp]]$rmm$code$wallace$outlierPval <- input$outlier_pval
      spp[[sp]]$rmm$code$wallace$outlierKRosner <- input$outlier_krosner
    }

    common$update_component(tab = "Map")

  })

  # reset occurrences button functionality
  observeEvent(input$goResetOccs, {
    req(curSp())
    spp[[curSp()]]$occs <- spp[[curSp()]]$occData$occsCleaned
    spp[[curSp()]]$rmm$code$wallace$occsSelPolyCoords <- NULL
    spp[[curSp()]]$procOccs$occsNoSpOutliers <- NULL
    spp[[curSp()]]$rmm$code$wallace$removedIDs <- NULL
    logger %>% writeLog(
      hlSpp(curSp()), "Reset to original occurrences (n = ",
      nrow(spp[[curSp()]]$occs), ").")
  })

  return(list(
    save = function() {
      list(outlierMethod = input$method,
           outlierPval = input$pval)
    },
    load = function(state) {
      # Load
    }
  ))

}

poccs_removeSpOutliers_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs

  if (!is.null(spp[[curSp()]]$procOccs$occsNoSpOutliers)) {

    # map removed points blue and retained points red
    # to match colors for spatial thinning

    occs_preRm <- spp[[curSp()]]$procOccs$occsPreRm
    map %>%
      clearAll() %>%
      addCircleMarkers(
        data = occs_preRm,
        lat = ~latitude,
        lng = ~longitude,
        radius = 5,
        color = "red",
        fill = TRUE,
        fillColor = "blue",
        fillOpacity = 1,
        weight = 2,
        popup = ~pop) %>%
      addCircleMarkers(
        data = occs(),
        lat = ~latitude,
        lng = ~longitude,
        radius = 5,
        color = "red",
        fill = TRUE,
        fillColor = "red",
        fillOpacity = 1,
        weight = 2,
        popup = ~pop) %>%
      zoom2Occs(occs_preRm) %>%
      addLegend(
        "bottomright",
        colors = c('red', 'blue'),
        title = "Occ Records",
        labels = c('Retained', 'Removed'),
        opacity = 1)

  } else {
    # if you haven't thinned, map all points red
    map %>%
      clearAll() %>%
      addCircleMarkers(
        data = occs(),
        lat = ~latitude,
        lng = ~longitude,
        radius = 5,
        color = "red",
        fill = TRUE,
        fillColor = "red",
        fillOpacity = 0.2,
        weight = 2,
        popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
  }
}

poccs_removeSpOutliers_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    poccs_removeSpOutliers_knit = !is.null(species$rmm$code$wallace$outlierMethod),
    outlierMethod_rmd = species$rmm$code$wallace$outlierMethod,
    outlierPval_rmd = species$rmm$code$wallace$outlierPval,
    outlierKRosner_rmd = species$rmm$code$wallace$outlierKRosner
  )
}
