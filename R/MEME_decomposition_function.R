#'@title MEME_decomposition
#'
#'@description The MEME is an index decomposition method, which is not biased when studying mix (or structure) effects. This package is written for the example of a given environmental impact generated through the output of differente sectors in several years. The decomposition reveals the following drivers: the sector mix effect, the overall activity effect, and the environmental intensity effect.
#'
#'@param dataframe
#'
#'@return list
#'
#'@examples
#'sector <- rep(c("a", "b", "c"), 2)
#'Year <- rep(c(0,1), each=3)
#'environmental_pressure <- c(15,6,3,25,7,5)
#'output <- c(3,3,3,6,4,5)
#'df<-as.data.frame(cbind(sector, Year, environmental_pressure, output))
#'df$Year <- as.numeric(df$Year)
#'df$environmental_pressure <- as.numeric(df$environmental_pressure)
#'df$output <- as.numeric(df$output)
#'
#'@export MEME_decomposition



MEME_decomposition <- function(df)#df is a dataframe with 4 columns: sector, Year, environmental_pressure, output
{

  toy_model1 <- left_join(df,
                          df%>%
                            group_by(Year)%>%
                            summarise(total_output = sum(output, na.rm=T)),
                          by = c("Year"))

  toy_model1 <- left_join(toy_model1,
                          toy_model1%>%
                            group_by(Year)%>%
                            summarise(total_environmental_pressure = sum(environmental_pressure, na.rm=T)),
                          by = c("Year"))


  toy_model1$environmental_pressure[toy_model1$environmental_pressure==0] <- 10^-100
  toy_model1$output[toy_model1$output==0] <- 10^-100
  toy_model1$total_output[toy_model1$total_output==0] <- 10^-100
  toy_model1$total_environmental_pressure[toy_model1$total_environmental_pressure==0] <- 10^-100


  toy_model1$share_of_sector <- toy_model1$output / toy_model1$total_output
  toy_model1$environmental_intensity <- toy_model1$environmental_pressure / toy_model1$output


  toy_model_MEME <- left_join(toy_model1[1:8],
                              filter(toy_model1[1:8], Year==min(toy_model1$Year, na.rm=T))%>%
                                select(sector,
                                       environmental_pressure),
                              by = c("sector"),
                              suffix = c("", "_T0"))%>%
    mutate(environmental_pressure_increase_since_T0 = environmental_pressure - environmental_pressure_T0)%>%
    select(-environmental_pressure_T0)


  toy_model_MEME <- left_join(toy_model_MEME,
                              filter(toy_model_MEME, Year==min(toy_model1$Year, na.rm=T))%>%
                                select(sector,
                                       output),
                              by = c("sector"),
                              suffix = c("", "_T0"))%>%
    mutate(output_increase_since_T0 = output - output_T0)


  toy_model_MEME %<>%
    mutate(average_environmental_intensity = total_environmental_pressure / total_output)%>%
    mutate(difference_to_mean_intensity = environmental_intensity - average_environmental_intensity)


  toy_model_MEME <- left_join(toy_model_MEME,
                              filter(toy_model_MEME, Year==min(toy_model1$Year, na.rm=T))%>%
                                select(sector,
                                       environmental_intensity),
                              by = c("sector"),
                              suffix = c("", "_T0"))%>%
    mutate(environmental_intensity_increase_since_T0 = environmental_intensity - environmental_intensity_T0)



  toy_model_MEME <- left_join(toy_model_MEME,
                              filter(toy_model_MEME, Year==min(toy_model1$Year, na.rm=T))%>%
                                select(sector,
                                       average_environmental_intensity),
                              by = c("sector"),
                              suffix = c("", "_T0"))

  toy_model_MEME <- toy_model_MEME%>%
    mutate(difference_to_mean_intensity_T0 = environmental_intensity_T0 - average_environmental_intensity_T0)







  toy_model_MEME%<>%

    mutate(sector_mix_effect =

             1/2* (

               output_increase_since_T0 *
                 difference_to_mean_intensity +

                 output_increase_since_T0 *
                 difference_to_mean_intensity_T0 )

    )%>%



    mutate(overall_activity_effect =


             1/2* (

               output_increase_since_T0 *
                 average_environmental_intensity +

                 output_increase_since_T0 *
                 average_environmental_intensity_T0)

    )%>%



    mutate(intensity_effect =

             1/2* (

               environmental_intensity_increase_since_T0 *
                 output +

                 environmental_intensity_increase_since_T0 *
                 output_T0 )

    )



  toy_model_MEME%<>%
    filter(!is.na(Year))

  toy_model_MEME_decomposition <- toy_model_MEME%>%
    group_by(Year)%>%
    summarise(sector_mix_effect = sum(sector_mix_effect, na.rm=T),
              overall_activity_effect = sum(overall_activity_effect, na.rm = T),
              intensity_effect = sum(intensity_effect, na.rm = T),
              environmental_pressure = sum(environmental_pressure_increase_since_T0, na.rm = T))%>%
    mutate(sum_effects = sector_mix_effect+overall_activity_effect+intensity_effect)%>%
    mutate(ratio_environmental_pressure_to_sum_effects = environmental_pressure / sum_effects)








  ### plot separate share of origin


  library(tidyr)
  toy_model_MEME_decomposition <- gather(toy_model_MEME_decomposition[1:5],
                                         2:5,
                                         key="primary_driver",
                                         value="contributions")

  toy_model_MEME_decomposition$LHS_RHS <- "Primary_drivers"
  toy_model_MEME_decomposition$LHS_RHS[toy_model_MEME_decomposition$primary_driver=="environmental_pressure"] <- "environmental_pressure"
  toy_model_MEME_decomposition$T0 <- min(toy_model1$Year, na.rm=T)

  toy_model_MEME_decomposition <- toy_model_MEME_decomposition%>%
    group_by(Year, primary_driver, LHS_RHS, T0)%>%
    summarise(contributions = sum(contributions, na.rm=T))



  toy_model_MEME_decomposition$primary_driver <- factor(toy_model_MEME_decomposition$primary_driver,
                                                        levels = (c("environmental_pressure",
                                                                    "overall_activity_effect", "sector_mix_effect",
                                                                    "intensity_effect")))
  levels(toy_model_MEME_decomposition$primary_driver)
  #### plot decomposition
  library(ggplot2)
  theme_set(theme_classic())
  library(RColorBrewer)
  #display.brewer.all()
  color_blind <- c('#e66101','#fdb863','#b2abd2','#5e3c99')[c(2,4,3)]


  #setwd("C:/Users/nroux/Filr/Net Folders/DATAH73000/H73700/data/!projekt/2008_COUPLED/Nicolas/drivers environmental_pressure/decomposition/decomposition until 2011/plots/with trade mix/marshall edgeworth/with population/6 permutations")
  plot_decomposition_MEME <- ggplot(toy_model_MEME_decomposition[toy_model_MEME_decomposition$LHS_RHS == "Primary_drivers",])+
    geom_col(aes(x = Year, y = contributions, fill = primary_driver))+
    scale_fill_manual(values = color_blind,
                      name = "",
                      labels = c("Overall activity",
                                 "Sector mix",
                                 "Environmental intensity"))+
    geom_line(data = toy_model_MEME_decomposition[toy_model_MEME_decomposition$LHS_RHS == "environmental_pressure",],
              aes(x = toy_model_MEME_decomposition$Year[toy_model_MEME_decomposition$LHS_RHS == "environmental_pressure"],
                  y = toy_model_MEME_decomposition$contributions[toy_model_MEME_decomposition$LHS_RHS == "environmental_pressure"],
                  colour="environmental pressure"),
              size=1.2)+
    scale_color_manual(name = NULL, values = c("environmental pressure" = "blue"))+
    scale_x_continuous(breaks = c(1,2))+
    ylab(paste("increase since", min(toy_model1$Year, na.rm=T), sep = " "))+
    ggtitle("MEME decomposition of environmental pressure")+
    theme_classic()

  decomposition <- list(toy_model1, toy_model_MEME, toy_model_MEME_decomposition, plot_decomposition_MEME)
  names(decomposition) <- c("input", "contributions", "results","plot")
  return(decomposition)
}
