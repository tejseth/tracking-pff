library(rdtools)
library(tidyverse)
library(ggthemes)
library(nflfastR)
library(mltools)
library(data.table)
library(xgboost)
library(caret)
library(vip)
library(SHAPforxgboost)
library(ggimage)
library(ggcorrplot)
library(lme4)
library(merTools)
library(shiny)
library(shinythemes)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}










