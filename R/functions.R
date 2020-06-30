


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

  res_fit <- sims %>% filter(step >= dias_inicio+dias_lag,step <= max_dias+dias_lag ) %>% 
    group_by(siminputrow,random_seed) %>% inner_join(df, by= c('step' = 'dias')) %>% 
    summarise( ssq = sum( (prop_fallecidos.x - prop_fallecidos.y) ^2 ))

  # res_fit <- sims %>% filter(step >= dias_inicio+dias_lag,step <= max_dias+dias_lag ) %>% 
  #   group_by(siminputrow,random_seed) %>% do( {
  #     ssq <- sum((.$prop_fallecidos - df$prop_fallecidos)^2)
  #     tibble( sumsqr = ssq)
  #   })
  #
  # Seleccion de series temporales con los datos simulados
  #
  fit <- res_fit %>% ungroup() %>% top_n(-res_num,ssq)  %>% select(-random_seed) %>% inner_join(sims) %>% mutate(fecha = min_fecha + step-dias_lag,casos_pred= prop_casos*pob_tot, fallecidos_pred=prop_fallecidos*pob_tot)
  

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


plot_ajustes <- function(fit,dff)
{
  print(
    ggplot(fit %>% filter(fecha<=max(dff$fecha)+30), aes(fecha,fallecidos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept =c(fec_lim,fec_min),color="black",linetype = 3)  +  geom_vline(xintercept = fec_lim,color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  + geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3)
    + ylab("Fallecidos CABA") + ggtitle("Modelo de Individuos - Grupo covid19UNGS")
  )
  
  print(
    ggplot(fit , aes(fecha,fallecidos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept = fec_lim,color="black",linetype = 3)  +  geom_vline(xintercept = c(fec_lim,fec_min) ,color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  + geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) + ylab("Fallecidos CABA") + xlab("") + ggtitle("Modelo de Individuos - Grupo covid19UNGS")
  )
  
  print(
  ggplot(fit, aes(fecha,hospitalizados_pred)) + geom_point(size=0.1) +  scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab("Hospitalizados CABA") + xlab("") + ggtitle("Modelo de Individuos - Grupo covid19UNGS") + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) 
  )
  
  print( 
  ggplot(fit %>% filter(fecha<=max(dff$fecha)+30), aes(fecha,casos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,casos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) + ylab("Casos CABA") + xlab("")  + ggtitle("Modelo de Individuos - Grupo covid19UNGS") 
  )
  
  print(
    ggplot(fit , aes(fecha,casos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,casos),color='red',size=.5) +
      scale_y_log10() + geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab("Casos CABA") + 
      xlab("") +  ggtitle("Modelo de Individuos - Grupo covid19UNGS")
  )

}