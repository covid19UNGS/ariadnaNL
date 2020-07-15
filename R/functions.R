


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
  res_fit <- res_fit %>% group_by(siminputrow) %>% summarise(ssq=sum(ssq))
  # res_fit <- sims %>% filter(step >= dias_inicio+dias_lag,step <= max_dias+dias_lag ) %>% 
  #   group_by(siminputrow,random_seed) %>% do( {
  #     ssq <- sum((.$prop_fallecidos - df$prop_fallecidos)^2)
  #     tibble( sumsqr = ssq)
  #   })
  #
  # Seleccion de series temporales con los datos simulados
  #
  # fit <- res_fit %>% ungroup() %>% top_n(-res_num,ssq)  %>% select(-random_seed) %>% inner_join(sims) %>% mutate(fecha = min_fecha + step-dias_lag,casos_pred= prop_casos*pob_tot, fallecidos_pred=prop_fallecidos*pob_tot)
  
  fit <- res_fit %>% ungroup() %>% top_n(-res_num,ssq) %>% inner_join(sims) %>% mutate(fecha = min_fecha + step-dias_lag,casos_pred= prop_casos*pob_tot, fallecidos_pred=prop_fallecidos*pob_tot, fallecidos_dia = fallecidos_pred -lag(fallecidos_pred), hospitalizados_pred=nro_hospitalizados/poblacion*pob_CABA)
  
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


plot_ajustes <- function(fit,dff,lugar)
{
  print(
    ggplot(fit %>% filter(fecha<=max(dff$fecha)+30), aes(fecha,fallecidos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept =c(fec_lim,fec_min),color="black",linetype = 3)  +  geom_vline(xintercept = fec_lim,color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  + geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3)
    + ylab(paste("Fallecidos",lugar)) + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print( 
    ggplot(fit , aes(fecha,fallecidos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept = fec_lim,color="black",linetype = 3)  +  geom_vline(xintercept = c(fec_lim,fec_min) ,color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  + geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) + ylab(paste("Fallecidos",lugar)) + xlab("") + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print(
  ggplot(fit, aes(fecha,hospitalizados_pred)) + geom_point(size=0.1) +  scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab(paste("Hospitalizados",lugar)) + xlab("") + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS") + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) 
  )
  
  print( 
  ggplot(fit %>% filter(fecha<=max(dff$fecha)+30), aes(fecha,casos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,casos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) + ylab(paste("Casos",lugar)) + xlab("")  + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS") 
  )
  
  print(
    ggplot(fit , aes(fecha,casos_pred)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,casos),color='red',size=.5) +
      scale_y_log10() + geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + 
      geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
      geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) + 
      ylab(paste("Casos",lugar)) + 
      xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )

  fitm <- fit %>% group_by(step) %>% summarise(fecha=max(fecha), fallecidos_pred_sd=sd(fallecidos_pred),
                                               hospitalizados_pred_sd=sd(hospitalizados_pred),
                                               hospitalizados_pred=mean(hospitalizados_pred),
                                               fallecidos_pred=mean(fallecidos_pred),
                                               casos_pred_sd = sd( casos_pred ),
                                               casos_pred = mean(casos_pred))
                                               
  
  print(
    ggplot(fitm, aes(fecha,hospitalizados_pred,color="Hospitalizados")) + geom_point(size=0.1) + 
    scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab(paste("Hospitalizados/Fallecidos",lugar)) + xlab("") + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS") + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3,color="red") +
    geom_point(aes(fecha,fallecidos_pred,color="Fallecimientos"),size=0.2) + labs(color="") + scale_color_manual(values=c("brown","black")) + theme(legend.position = c(0.8, 0.1)) +  
    geom_ribbon(aes(ymin=hospitalizados_pred-hospitalizados_pred_sd, ymax=hospitalizados_pred+hospitalizados_pred_sd), linetype=2, alpha=0.1) 
  )

  print(
    ggplot(fitm, aes(fecha,casos_pred)) + geom_point(size=0.1) +  
    geom_ribbon(aes(ymin=casos_pred-casos_pred_sd, ymax=casos_pred+casos_pred_sd), linetype=2, alpha=0.1) +
    scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab(paste("Casos",lugar)) + xlab("") + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS") + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +
    geom_point(data=dff,aes(fecha,casos),color='red',size=.5)
  )
  
  ggplot(fitm, aes(fecha,fallecidos_pred)) + geom_point(size=0.1) +  
    geom_ribbon(aes(ymin=fallecidos_pred-fallecidos_pred_sd, ymax=fallecidos_pred+fallecidos_pred_sd), linetype=2, alpha=0.1) +
    scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab(paste("Fallecidos",lugar)) + xlab("") + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS") + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +
    geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5)
  
  return(fitm)
}


plot_ajustes_CI <- function(fit,dff,lugar)
{
  sum_fit <- fit %>% mutate(uti = hospitalizados_pred*.1) %>% group_by(fecha) %>% 
    summarise( fallecidos_hi95=quantile(fallecidos_pred, 0.975),fallecidos_lo95=quantile(fallecidos_pred, 0.025),fallecidos_med=mean(fallecidos_pred),
               fallecidos_sd=sd(fallecidos_pred),hospitalizados_hi95=quantile(hospitalizados_pred, 0.975),hospitalizados_lo95=quantile(hospitalizados_pred, 0.025),
               hospitalizados_med=mean(hospitalizados_pred),uti_hi95=quantile(uti, 0.975),uti_lo95=quantile(uti, 0.025),uti_med=mean(uti),
               casos_hi95=quantile(casos_pred, 0.975),casos_lo95=quantile(casos_pred, 0.025),casos_med=mean(casos_pred))
  
  print(
  
      ggplot(sum_fit %>% filter(fecha<=max(df$fecha)+30), aes(fecha,fallecidos_med)) + geom_point(size=0.1) +
        geom_ribbon(aes(ymin=fallecidos_lo95, ymax=fallecidos_hi95), linetype=2, alpha=0.1) +
        geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept =c(fec_lim,fec_min),color="black",linetype = 3)  +  geom_vline(xintercept = fec_lim,color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  + geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +  
        ylab(paste("Fallecidos",lugar)) + 
        xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print(
  ggplot(sum_fit, aes(fecha,fallecidos_med)) + geom_point(size=0.1) + 
    geom_ribbon(aes(ymin=fallecidos_lo95, ymax=fallecidos_hi95), linetype=2, alpha=0.1) +
    geom_point(data=dff,aes(fecha,fallecidos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept =c(fec_lim,fec_min),color="black",linetype = 3)  +  geom_vline(xintercept = fec_lim,color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  + geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +  
    ylab(paste("Fallecidos",lugar)) + 
    xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print(
  ggplot(sum_fit, aes(fecha,hospitalizados_med)) + geom_point(size=0.1) +  scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) +
    geom_ribbon(aes(ymin=hospitalizados_lo95, ymax=hospitalizados_hi95), linetype=2, alpha=0.1) +  
    ylab(paste("Hospitalizados",lugar)) + 
    xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print(
  ggplot(sum_fit, aes(fecha,uti_med)) + geom_point(size=0.1) +  scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) +
    geom_ribbon(aes(ymin=uti_lo95, ymax=uti_hi95), linetype=2, alpha=0.1) + geom_hline(yintercept = 1027, color= "red", linetype=2) +  
    ylab(paste("UTI",lugar)) + 
    xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  #
  # Chequeado
  # https://chequeado.com/el-explicador/camas-de-terapia-intensiva-en-la-ciudad-cuantas-hay-y-que-podria-pasar-segun-las-proyecciones/
  #
  
  print(
  ggplot(sum_fit %>% filter(fecha<=max(df$fecha)+30), aes(fecha,casos_med)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,casos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +
    geom_ribbon(aes(ymin=casos_lo95, ymax=casos_hi95), linetype=2, alpha=0.1)  +  
    ylab(paste("Casos",lugar)) + 
    xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print(
  ggplot(sum_fit, aes(fecha,casos_med)) + geom_point(size=0.1) + geom_point(data=dff,aes(fecha,casos),color='red',size=.5) + scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
    geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +
    geom_ribbon(aes(ymin=casos_lo95, ymax=casos_hi95), linetype=2, alpha=0.1) +  
    ylab(paste("Casos",lugar)) + 
    xlab("") +  ggtitle("Modelo AriadnaNL - Grupo covid19UNGS")
  )
  
  print(
    ggplot(sum_fit, aes(fecha,hospitalizados_med)) + geom_point(size=0.1) +  
      geom_ribbon(aes(ymin=hospitalizados_lo95, ymax=hospitalizados_hi95), linetype=2, alpha=0.1) +  
      scale_y_log10() +  geom_vline(xintercept = c(fec_lim,fec_min),color="black",linetype = 3) + ylab(paste("Hospitalizados/Fallecidos",lugar)) + xlab("") + ggtitle("Modelo AriadnaNL - Grupo covid19UNGS") + geom_vline(data= fases, aes(xintercept = fecha), col = "red", lty = 3)  +
      geom_text(data = fases, mapping = aes(label = nombre, x=fecha,y = 0), angle = 60, hjust = 0,size=3) +
      geom_point(aes(fecha,fallecidos_med),size=0.2,color="red") 
  )
  
}
