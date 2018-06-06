#' @export
# Function to calculate amount of insect species habitat

insectHab.fun = function(habInput, habFunInput, waterImpactedHab){

  # habInput = linear m of habitat in elevation bands for each insect species
  # habFunInput = function describing relationship between species habitat and water prevalence
  # waterImpactedHab = percent change in habitat from water in natural condition

  CQ.out = list()
  MX.out = list()
  MP.out = list()
  MN.out = list()

  waterImpactedHab = waterImpactedHab/100 #convert to percentage for multiplication

  # multiplication = amount of habitat for each sp in each basin *
    # how much that habitat is impacted by the water passed into the basin *
    # the insect species' relationship between water and habitat

 # **********************references subject to change based on new real inputs********************
  habInput[ , 3]*waterImpactedHab*habFunInput[ ,1] -> CQ.out
  habInput[ , 4]*waterImpactedHab*habFunInput[ ,2] -> MX.out
  habInput[ , 5]*waterImpactedHab*habFunInput[ ,3] -> MP.out
  habInput[ , 6]*waterImpactedHab*habFunInput[ ,4] -> MN.out


  # -------------------------------------------------------------------
  # Save output in dataframe
  habOut = tibble::tibble(CQ.out, MX.out, MP.out, MN.out)
  colnames(habOut) <- c("CQ_sum", "MX_sum", "MP_sum", "MN_sum")
  habOut = tibble::rownames_to_column(habOut, var = "BasinID" )
  habOut$BasinID = as.numeric(habOut$BasinID)
  return(habOut)
}
