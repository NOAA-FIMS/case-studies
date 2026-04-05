# need to think about how to deal with multiple fleets, only using 1 right now
get_asap_data <- function(asap_input) {
  res <- data.frame(type = character(),
                name = character(),
                age = integer(),
                timing = double(),
                value = double(),
                unit = character(),
                uncertainty = double())
  years_in_model <- seq(asap_input$parms$styr, asap_input$parms$endyr)
  
  landings <- data.frame(type = "landings",
                     name = "fishery",
                     age = NA,
                     timing = years_in_model,
                     value = as.numeric(asap_input$catch.obs[1,]),
                     unit = "mt",
                     # This was
                     # asap_input$control.parms$catch.tot.cv[,1]
                     # but it was just reset to the following later in the parameters so using internal functionality to set it now
                     # uncertainty = rep(log(sqrt(log(as.numeric(mean(asap_input$control.parms$catch.tot.cv[,1], na.rm=TRUE)^2) + 1))), asap_input[["parms"]][["nyears"]])
                     uncertainty = rep(sqrt(log(as.numeric(mean(asap_input$control.parms$catch.tot.cv[,1], na.rm=TRUE)^2) + 1)), asap_input[["parms"]][["nyears"]])
  )
  
  # loop over all indices
  for (i in seq(asap_input$parms$nindices)) {
    index <- data.frame(
      type = "index",
      name = paste0("survey", i),
      age = NA_integer_,
      timing = years_in_model[asap_input$index.year.counter[[i]]],
      value = as.numeric(asap_input$index.obs[[i]]),
      unit = "mt",
      # uncertainty = rep(log(sqrt(log(as.numeric(mean(asap_input$index.cv[[i]], na.rm=TRUE)^2 + 1)))), asap_input[["parms"]][["nyears"]]))
      uncertainty = rep(
        sqrt(log(as.numeric(mean(asap_input$index.cv[[i]], na.rm=TRUE)^2 + 1))),
        length(asap_input$index.year.counter[[i]])
      )
    )
    if (i == 1){
      allinds <- index
    }else{
      allinds <- rbind(allinds, index)
    }
  }
  
  catchage <- data.frame(
    type = "age_comp",
    name = "fishery",
    age = rep(seq(1,asap_input$parms$nages), asap_input$parms$nyears),
    timing = rep(seq(asap_input$parms$styr, asap_input$parms$endyr), each = asap_input$parms$nages),
    value = as.numeric(t(asap_input$catch.comp.mats$catch.fleet1.ob)),
    unit = "",
    uncertainty = rep(asap_input$fleet.catch.Neff.init[1,], each = asap_input$parms$nages)
  )
  
  # loop over all indices
  for (i in 1:asap_input$parms$nindices){
    indexage <- data.frame(type = "age_comp",
                           name = paste0("survey", i),
                           age = rep(seq(1,asap_input$parms$nages), asap_input$parms$nyears),
                           timing = rep(seq(asap_input$parms$styr, asap_input$parms$endyr), each = asap_input$parms$nages),
                           value = as.numeric(t(asap_input$index.comp.mats[[i*2-1]])),
                           unit = "",
                           uncertainty = rep(asap_input$index.Neff.init[i,], each = asap_input$parms$nages))
    if (i == 1){
      allindsage <- indexage
    }else{
      allindsage <- rbind(allindsage, indexage)
    }
  }
  
  # Weight at age is in asap_input$WAA.mats$WAA.catch.all but for yellowtail the data
  # is time-invariant. Could just use the first row but the code below shows how
  # to use values for every year/age combination. Additionally, it adds the
  # extra year of data needed for FIMS to predict spawning biomass after all
  # catches are removed
  weight_at_age_matrix <- rbind(
      asap_input[["WAA.mats"]][["WAA.catch.all"]],
      asap_input[["WAA.mats"]][["WAA.catch.all"]][
        NROW(asap_input[["WAA.mats"]][["WAA.catch.all"]]), 
      ]
    )
  
  weight_at_age <- data.frame(
    type = "weight_at_age",
    name = NA_character_,
    age = rep(
      seq(asap_input[["parms"]][["nages"]]),
      length(seq(asap_input[["parms"]][["styr"]], asap_input[["parms"]][["endyr"]] + 1))
    ),
    timing = rep(
      seq(asap_input[["parms"]][["styr"]], asap_input[["parms"]][["endyr"]] + 1),
      each = asap_input[["parms"]][["nages"]]
    ),
    value = c(t(weight_at_age_matrix)),
    unit = "mt",
    uncertainty = NA
  )

  res <- rbind(res, landings, allinds, catchage, allindsage, weight_at_age)
  return(res)
}
