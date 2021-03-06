##' @export
prepareYXdataset <- function(from = '1990-01-01' %>>% as.Date,to = '2016-06-01' %>>% as.Date){
  imfutils::imfRefCtry() %>>%
    dplyr::mutate(imfctry = as.numeric(imfctry)) ->
    ref

  imfutils::loadIFS2016() ->
    ifs

  imfutils::prepare_IMF_BOP_IIP() ->
    ifs_iip

  oecdutils::prepare_OECD_credit_data() ->
    oecd_credit

  imfutils::loadWEO2015oct() ->
    imfweo

  bisutils::adjustedCreditData() ->
    bis

  bisutils::loadBISlocData() ->
    bisloc

  bisutils::loadBIShousepriceData()->
    bishouse

  imfutils::loadFSI2016() ->
    imffsi

  amecoutils::loadAMECOselected() ->
    ameco

  amecoutils::loadAMECO_construction_indicators() ->
    ameco_cons

  ## -------------------------------------------------------------------------- ##
  ## SKELETON                                                                   ##
  ## -------------------------------------------------------------------------- ##
  ref  %>>%
    subset(!is.na(iso3)) %>>%
    (iso3) %>>%
    unique ->
    ids

  seq(
    from = from,
    to = to,
    by = '3 month'
  )-1 ->
    dates

  CJ(iso3 = ids, date = dates) ->
    skeleton

  ## ------------------------------------------------------------------------ ##
  ## IFS Data                                                                 ##
  ## ------------------------------------------------------------------------ ##
  (ref %>>%
     dplyr::select(imfctry,iso3) %>>%
     setkey(imfctry))[
    ifs %>>% setkey(imfctry)
    ] %>>%
    subset(!is.na(iso3)) %>>%
    subset(drop == FALSE )->
    ifs_v2

  ifs_v2 %>>%
    dplyr::select(
      iso3,date,variable = concept_id, value
    ) ->
    ifs_long

  ## -------------------------------------------------------------------------- ##
  ##  BIS credit data (y)                                                       ##
  ## -------------------------------------------------------------------------- ##
  (ref %>>%
     dplyr::select(iso2,iso3) %>>%
     setkey(iso2))[
       bis %>>%
         setkey(iso2)
       ] %>>%
    subset(!is.na(iso3)) %>>%
    dplyr::select(-iso2) %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    subset(!is.na(value)) ->
    bis_long

  (ref %>>%
     dplyr::select(iso2,iso3) %>>%
     setkey(iso2))[
    bisloc %>>%
      setkey(iso2)
    ] %>>%
    dplyr::select(-iso2) %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    subset(!is.na(iso3)) ->
    bisloc_long

  ## ------------------------------------------------------------------------ ##
  ## BIS House Prices                                                         ##
  ## ------------------------------------------------------------------------ ##
  (ref %>>%
     dplyr::select(iso2,iso3) %>>%
     setkey(iso2))[
       bishouse %>>%
         setkey(iso2)
       ] %>>%
    dplyr::select(-iso2) %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    subset(!is.na(iso3)) %>>%
    subset(!is.na(value)) ->
    bishouse_long

  ## -------------------------------------------------------------------------- ##
  ## OECD data (y)                                                              ##
  ## -------------------------------------------------------------------------- ##
  oecd_credit %>>%
    attr('widedata') %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    subset(!is.na(value)) ->
    oecd_credit_long

  ## ---------------------------------------------------------------------- ##
  ## Add WEO data (X)                                                       ##
  ## ---------------------------------------------------------------------- ##
  (ref %>>%
     dplyr::select(imfctry,iso3) %>>%
     setkey(imfctry))[
       imfweo %>>%
         dplyr::select(imfctry,
                       date,
                       variable = concept_id,
                       value) %>>%
         setkey(imfctry)
       ] %>>%
    subset(!is.na(iso3)) %>>%
    dplyr::select(-imfctry) %>>%
    subset(!is.na(value)) ->
    imfweo_long

  ## ---------------------------------------------------------------------- ##
  ## Add IMF FSI data (X)                                                   ##
  ## ---------------------------------------------------------------------- ##
  (ref %>>%
     dplyr::select(imfctry,iso3) %>>%
     setkey(imfctry))[
       imffsi %>>%
         dplyr::select(imfctry,
                       date,
                       variable = concept_id,
                       value) %>>%
         setkey(imfctry)
       ] %>>%
    subset(!is.na(iso3)) %>>%
    dplyr::select(-imfctry) %>>%
    subset(!is.na(value)) ->
    imffsi_long

  imffsi_long %>>%
    subset(
      ! variable %like% "^FSASD|^FSGD"        #Sectoral/Geo distribution
    ) %>>%
    subset(
      variable %like% "\\.PT"
    )->
    imffsi_long

  ## ---------------------------------------------------------------------- ##
  ## WB GFDD (X)                                                            ##
  ## ---------------------------------------------------------------------- ##
  wbutils::process_wbgfdd2015() %>>%
    subset(
    (variable %like% "GFDD.DI")|
      (variable %like% "GFDD.DM") |
      (variable %like% "GFDD.EI") |
      (variable %like% "GFDD.OI") |
      (variable %like% "GFDD.EM") |
      (variable %like% "GFDD.SI") |
      (variable %like% "GFDD.SM")
    ) ->
    wb_gfdd_long

  ## ------------------------------------------------------------------------ ##
  ## Derived: IR spreads                                                      ##
  ## ------------------------------------------------------------------------ ##
  ifs_v2 %>>%
    subset(
      concept_id == "FIGB.PA" |
        concept_id == "FITB.PA"
    ) %>>%
    subset(
      iso3 %in% c('USA','DEU')
    ) %>>%
    dcast(
      date ~ concept_id + iso3,
      value.var = 'value'
    ) ->
    ifs_yields_usd

  ifs_v2 %>>%
    subset(
      concept_id == "FIGB.PA" |
        concept_id == "FITB.PA"
    ) %>>%
    dcast.data.table(
      iso3 + date ~ concept_id,
      value.var = "value"
    ) ->
    ifs_yields

  (ifs_yields_usd %>>%
     setkey(date))[
       ifs_yields %>>% setkey(date),
       roll = TRUE,
       rollends = FALSE
       ] %>>%
    dplyr::select(
      iso3, date,
      contains("FIGB"),
      contains("FITB")
    ) %>>%
    dplyr::arrange(
      iso3,date
    ) %>>%
    dplyr::mutate(
      spread_st_deu = FITB.PA - FITB.PA_DEU,
      spread_lt_deu = FIGB.PA - FIGB.PA_DEU,
      spread_st_usa = FITB.PA - FITB.PA_USA,
      spread_lt_usa = FIGB.PA - FIGB.PA_USA,
      curve_deu =  spread_lt_deu - spread_st_deu,
      curve_usa =  spread_lt_usa - spread_st_usa
    ) ->
    spreads

  spreads %>>%
    melt.data.table(
      id.vars = c('iso3','date')
    ) %>>%
    subset(
      !is.na(value)
    ) ->
    spreads_long

  ## ------------------------------------------------------------------------ ##
  ## AMECO Data                                                               ##
  ## ------------------------------------------------------------------------ ##
  require(openxlsx)
  amecoutils::loadAMECOselected() ->
    ameco

  ## ------------------------------------------------------------------------ ##
  ## IMF IIP                                                                  ##
  ## ------------------------------------------------------------------------ ##
  ifs_iip %>>%
    melt.data.table(
      id.vars = c('iso3','date')
    ) %>>%
    subset(
      !is.na(value)
    ) ->
    ifs_iip_long

  ## -------------------------------------------------------------------------- ##
  ## JOIN DATA                                                                  ##
  ## -------------------------------------------------------------------------- ##
  list(
    bis_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    bisloc_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    ## oecd_credit_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    imfweo_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    imffsi_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    ifs_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    ifs_iip_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    wb_gfdd_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    spreads_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    bishouse_long %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    ameco %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character),
    ameco_cons %>>% dplyr::mutate(date = date %>>% as.Date, variable = variable %>>% as.character)
  ) %>>%
    rbindlist ->
    df_unbalanced

  CJ(iso3 = ids, date = dates,
     variable = df_unbalanced %>>% (variable) %>>% unique
     ) ->
    skeleton2

  ## MATCH
  (df_unbalanced %>>%
     setkey(variable,iso3,date))[
       skeleton2 %>>%
         setkey(variable,iso3,date),
       roll = FALSE,                       # Impute missing values using
       # spline later
       rollends = FALSE
       ] ->
    df_balanced1


  (df_unbalanced %>>%
     setkey(variable,iso3,date))[
       skeleton2 %>>%
         setkey(variable,iso3,date),
       roll = TRUE,
       rollends = FALSE
       ] %>>%
   setnames(
      old = 'value',
      new = 'value_locf'
    ) ->
    df_balanced2

  (df_balanced1 %>>%
     setkey(iso3,date,variable))[
       df_balanced2 %>>%
         setkey(iso3,date,variable)
       ] ->
    df_balanced


  ## MISSING VALUE IMPUTATION
  df_balanced %>>%
    dplyr::arrange(iso3,variable,date) %>>%
    dplyr::mutate(
      tsix = sprintf("%s.%s",
                     iso3,
                     variable)
    ) %>>%
    (df ~ df[, {
      .SD %>>%
        eventstudy::interpolateTS(na.interpolation = 'stine',
                      silent = TRUE,
                      plot = FALSE)
    }, by = tsix])    ->
    df_balanced_interpolated

  df_balanced_interpolated[
    is.na(value_locf),
    value_interpolated := NA
  ]

  ## df_balanced_interpolated %>>%
  ##   copy %>>%
  ##   (dt~dt[,{list(N = .N)}, by = c('iso3','variable','date')]) ->
  ##   test

  df_balanced_interpolated %>>%
    setkeyv(
      c('iso3','variable','date')
    ) %>>%
    unique %>>%
    dplyr::mutate(
      y = value_interpolated
    ) %>>%
    dcast(
      iso3+date~variable,
      value.var = c('y')
    ) ->
    yxdata

  attr(yxdata,'longdata') <- df_balanced_interpolated

  outfile = file.path(dir.root,'inst/data/derived/',
                      sprintf("%s yxdata.RData", Sys.Date()))

  save(yxdata, file = outfile)

  return(yxdata)
}


