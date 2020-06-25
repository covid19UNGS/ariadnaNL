


#' Calcula suma de cuadrados minimo entre un conjunto de simulaciones y un conjunnto de datos 
#' Usando la proporcion de fallecidos 
#' @param df 
#' @param dias_inicio 
#' @param dias_fin 
#' @param sims 
#' @param dias_lag Cantidad de dias previos al primer caso donde empieza la epidemia
#' @param res_num  number of results starting from minimum
#'
#' @return
#' @export
#'
#' @examples
fit_ariadnaNL_simulations <- function(df,dias_inicio,dias_fin,sims,pob_tot,dias_lag=0,res_num=1){
  

  #
  # Calculo fechas límites del ajuste
  #
  max_dias <- max(df$dias)- dias_fin         
  stopifnot(max_dias>dias_inicio)
  min_fecha <- min(df$fecha)
  fec_lim <- min_fecha + max(df$dias) - dias_fin
  
  # 
  # Ajuste a partir de dias_inicio 
  # 
  df <- df %>% filter(dias >= dias_inicio, dias <= max_dias )
  #res_fit <- sims %>% filter(step >= dias_inicio, step <=max_dias) %>% filter(siminputrow==1,random_seed==7235)
  #unique(res_fit$step)
  #sum((df$prop_fallecidos - res_fit$prop_fallecidos)^2)
  
  res_fit <- sims %>% filter(step >= dias_inicio+dias_lag,step <= max_dias+dias_lag ) %>% 
    group_by(siminputrow,random_seed) %>% do( {
      ssq <- sum((.$prop_fallecidos - df$prop_fallecidos)^2)
      tibble( sumsqr = ssq)
    })
  #
  # Seleccion de series temporales con los datos simulados
  # filter(sumsqr==min(sumsqr))
  fit <- res_fit %>% ungroup() %>% top_n(-res_num,sumsqr)  %>% select(-random_seed) %>% inner_join(sims) %>% mutate(fecha = min_fecha + step-dias_lag,casos_pred= prop_casos*pob_tot, fallecidos_pred=prop_fallecidos*pob_tot)
  

  return(list(fit=fit, fec_lim=fec_lim,fec_min=min_fecha+dias_inicio))
}


#' Read Netlogo simualtions and clean field names 
#'
#' @param fname name of the csv file with NetLogo simulations
#' @param skip if the simulation is done with Behavior Space tool it has to skip the 6 first lines
#'
#' @return a data.frame 
#' @export
#'
#' @examples
read_netlogo_simul <- function(fname,skip=6){
  require(readr)
  mdl <- read_csv(fname,skip=skip)
  
  nam <- names(mdl)
  nam <- gsub("-","_",nam)
  nam <- gsub("[][]","",nam)
  nam <- gsub(" ","_",nam)
  names(mdl) <- nam
  return(mdl)
}

rename_netlogo_simul <- function(mdl){
  nam <- names(mdl)
  nam <- gsub("-","_",nam)
  nam <- gsub("[][]","",nam)
  nam <- gsub(" ","_",nam)
  names(mdl) <- nam
  return(as_tibble(mdl))
}