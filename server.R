# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

## Required Packages & Data
library(tidyverse)
library(DT)
library(shiny)
library(markdown)
library("viridis",warn.conflicts = FALSE)
library(plotly)
library(ggplot2)

#load a dataframe of indices
index_data <- read_rds("./data/indices_2007-2022.rds") %>% 
               filter(!Treatment %in% c("IGNOR")) %>% 
               drop_na()

## Useful Objects for Plotting 
site_list <- c("MAT81", "MAT89", "MAT06", "MNT97", "NNT97", "DHT89", "WSG89", "SHB89")
block_list <- c("1", "2", "3", "4")
CT <- c("CT","CT1","CT2", "CT2-old")
NP <- c("F10","NP")
NP_gradient <- c("F0.5","F1","F2","F5", "F10")
N_types <- c("NO3", "NH4")
trtmt_list <- list(CT, "N", "P", "NP", "NP_gradient", "N_types")

## Formatting vectors 
np_colors <- RColorBrewer::brewer.pal(5, "YlGnBu")
np_colors[5] <- "green4"

### TEXT
Site.text <-  c("MAT81", "MAT89", "MAT06", "MNT97", "NNT97", "DHT89", "WSG89", "SHB89")
names(Site.text) <- c("MAT81", "MAT89", "MAT06", "MNT97", "NNT97", "DHT89", "WSG89", "SHB89")

## PLOT OPTIONS

# Color Palettes 
n_yellow <- rgb(255, 192, 0, maxColorValue = 255)
p_blue <- rgb(46, 117, 182, maxColorValue = 255)
np_green <- rgb(112, 173, 71, maxColorValue = 255) #lmat_colors[5] 
ct_gray <- rgb(175, 171, 171, maxColorValue = 255)
lmat_colors <- c(rgb(226, 240, 217, maxColorValue = 255), rgb(169, 209, 142, maxColorValue = 255), rgb(112, 173, 71, maxColorValue = 255), rgb(84, 130, 53, maxColorValue = 255), rgb(56, 87, 35, maxColorValue = 255))



# Define server logic required to draw plots ----
shinyServer(
  
  function(input, output, session) {
    
    options(digits=5)
# Reactive Conductor Functions  -------------------------------------------
    ## (see https://shiny.rstudio.com/articles/reactivity-overview.html)
    
    #**************************************************************************    
    ctl_comp_select <- function(index_data) {
      sites <- input$ctl_comp_sites
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      sub_data <- index_data %>%
        filter(Site %in% sites) %>%
        filter(Treatment %in% CT) %>%
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>% #Choose index to graph
        filter(Year >= input$ctl_comp_years[1] & Year <= input$ctl_comp_years[2]) %>%
        mutate(Year = factor(Year)) %>%
        mutate(Block = factor(Block)) %>%
        # SUMMARIZE by block and site
        group_by(Year, DOY, Date, Site, Block, Treatment) %>%
        summarize(across(modisNDVI:micasenseNDVIRE, ~ mean(.x, na.rm=T)),.groups = "keep") %>% 
        group_by(Year, DOY, Date, Site, Treatment) %>%
        summarize(across(modisNDVI:micasenseNDVIRE, 
                         list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x,na.rm = TRUE), n = ~ n()))
                  ,.groups = "keep") %>% 
        mutate(across(where(is.numeric), ~ round(., digits = 4)))
      
      return(sub_data)
    }
    #**************************************************************************  
    bysite_select <- function(index_data) {
      sites <- input$bysite_sites
        #unlist(site_list[as.numeric(input$bysite_sites)])
      trtmts <- input$bysite_trtmts
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>%
        filter(Year >= input$bysite_years[1] & Year <= input$bysite_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE by block and site
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize(across(modisNDVI:micasenseNDVIRE, ~ mean(.x, na.rm=T)),.groups = "keep") %>% 
        group_by(Year, DOY, Date, Site, Treatment) %>% 
        summarize(across(modisNDVI:micasenseNDVIRE, 
                         list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x,na.rm = TRUE), n = ~ n()))
                  ,.groups = "keep") %>% 
        mutate(across(where(is.numeric), ~ round(., digits = 4)))
      
      return(sub_data) 
    }

    #**************************************************************************   
    byblock_select <- function(index_data) {
      sites <- input$byblock_site
      blocks <- input$byblock_blocks
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% input$byblock_trtmts) %>% 
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>%
        filter(Block %in% blocks) %>% 
        filter(Year >= input$byblock_years[1] & Year <= input$byblock_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE - by block
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize(across(modisNDVI:micasenseNDVIRE, 
                         list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x,na.rm = TRUE), n = ~ n()))
                  ,.groups = "keep") %>% 
        mutate(across(where(is.numeric), ~ round(., digits = 4)))
      
      return(sub_data)
    }
    #**************************************************************************
    byplot_select <- function(index_data) {
      sites <- input$byplot_site
      blocks <- input$byplot_blocks
      trtmts <- input$byplot_trtmts
      measures <- input$byplot_measurement
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
    # SELECTION - subset of full dataframe
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>%
        filter(Block %in% blocks) %>% 
        filter(Replicate %in% measures) %>% 
        filter(Year >= input$byplot_years[1] & Year <= input$byplot_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        mutate(Replicate = factor(Replicate))  %>% 
        group_by(Year, DOY, Date, Site, Block, Treatment, Replicate) %>% 
        summarize(across(modisNDVI:micasenseNDVIRE, 
                         list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x,na.rm = TRUE), n = ~ n()))
                  ,.groups = "keep") %>% 
        mutate(across(where(is.numeric), ~ round(., digits = 4)))
      
      return(sub_data)
    }
# Select box updates.  When a file is loaded, get the sites and blocks ----
    # and select the first ones
    
    ## Block_Level tab ----
    observeEvent(input$byblock_site,{
      freezeReactiveValue(input, "byblock_trtmts")
      selected_t <-unique(filter(index_data, Site %in% input$byblock_site) %>%
                            select(Treatment))$Treatment      
      updateCheckboxGroupInput(session, inputId = "byblock_trtmts", choices = selected_t,
                               selected = selected_t[1])
      freezeReactiveValue(input, "byblock_blocks")
      selected_b <-unique(filter(index_data, Site %in% input$byblock_site) %>% select(Block))$Block
      updateCheckboxGroupInput(session, inputId = "byblock_blocks", choices = selected_b,
                               selected = selected_b[1])
      
    })
   ## Plot-Level tab  ----
    observeEvent(input$byplot_site,{
      freezeReactiveValue(input, "byplot_trtmts")
      selected_t <-unique(filter(index_data, Site %in% input$byplot_site) %>%
                            select(Treatment))$Treatment      
      updateCheckboxGroupInput(session, inputId = "byplot_trtmts", choices = selected_t,
                               selected = selected_t[1])
      freezeReactiveValue(input, "byplot_blocks")
      selected_b <-unique(filter(index_data, Site %in% input$byplot_site) %>% select(Block))$Block
      updateCheckboxGroupInput(session, inputId = "byplot_blocks", choices = selected_b,
                               selected = selected_b[1])
      
    })
    ## Site-Level tab  ----
    observeEvent(input$bysite_sites,{
      freezeReactiveValue(input, "bysite_trtmts")
      selected_t <-unique(filter(index_data, Site %in% input$bysite_sites) %>%
                            select(Treatment))$Treatment      
      updateCheckboxGroupInput(session, inputId = "bysite_trtmts", choices = selected_t,
                               selected = selected_t[1])
      # freezeReactiveValue(input, "bysite_blocks")
      # selected_b <-unique(filter(index_data, Site %in% input$bysite_sites) %>% select(Block))$Block
      # updateCheckboxGroupInput(session, inputId = "bysite_blocks", choices = selected_b,
      #                          selected = selected_b[1])
      
    })
# Reactive Dataframes -----------------------------------------------------
    data <- reactiveValues(ctl_comp_sub_data = NULL,
                           bysite_sub_data = NULL,
                           byblock_sub_data = NULL,
                           byplot_sub_data = NULL)
    
    
# Data Table Outputs --------------------------------------------------------
    
    ## Control Tab Table ----
    output$ctl_comp_table <- DT::renderDataTable({
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      sub_data_selected_data <- index_data %>%
        filter(Site %in% input$ctl_comp_sites,
               Treatment %in% CT,
               if (is_null(event_data("plotly_selecting")$customdata)) {
                 Date == Date
               } else {
                 Date %in% as_date(event_data("plotly_selecting")$customdata)
               } 
               ) %>%
        mutate(Treatment = replace(Treatment, Treatment %in% CT &
                                     aggregate_ctls, "CT"),
               Treatment)  %>% 
        group_by(Year, DOY, Date, Site, Block, Treatment) %>%
        summarize(across(
          contains(input$byblock_index),
          #modisNDVI:micasenseNDVIRE,
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            n = ~ n()
          )
        )
        , .groups = "keep") %>% 
        mutate(across(where(is.numeric), ~ round(., digits = 4)))

      DT::datatable(sub_data_selected_data, options = list(orderClasses = TRUE)) 
    })
    
    ## By Site Tab Table ----
    output$bysite_table <- DT::renderDataTable({
        bysite_selected_data <- index_data %>%
        filter(
          Site %in% input$bysite_sites,
          Treatment %in% input$bysite_trtmts,
          if (is_null(event_data("plotly_selecting")$customdata)) {
            Date == Date
          } else {
            Date %in% as_date(event_data("plotly_selecting")$customdata)
          } 
          ) %>%
        group_by(Date, Site, Treatment, Block) %>%
        summarise(across(
          contains(input$bysite_index),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            n = ~ n()
          )
        ), .groups = "keep") %>% 
        mutate(across(where(is.numeric), ~ round(., digits = 4)))
      
        DT::datatable(bysite_selected_data, options = list(orderClasses = TRUE))
    })
    
    ## By Block Tab Table ----
    output$byblock_table <- DT::renderDataTable({
      byblock_selected_data <- index_data %>%
        filter(
          Site %in% input$byblock_site,
          Treatment %in% input$byblock_trtmts,
          Block %in% input$byblock_blocks,
          if (is_null(event_data("plotly_selecting")$customdata)) {
            Date == Date
          } else {
            Date %in% as_date(event_data("plotly_selecting")$customdata)
          }
        ) %>%
        select(Year, Date, Site, Block, Treatment, Replicate,
               contains(input$byblock_index)) %>%
        mutate(across(where(is.numeric), ~ round(., digits = 4)))
      
      DT::datatable(byblock_selected_data, options = list(orderClasses = TRUE))
    })
    
    
    ## By Plot Tabel ----
    output$byplot_table <- DT::renderDataTable({
      DT::datatable(data$byplot_sub_data, options = list(orderClasses = TRUE))
    })
    
# Plot Outputs -------------------------------------------------------------
    
    ##  Controls Plots ----
    output$ctl_compPlot <- renderPlotly({ ######## AGGREGRATE BY SITE
      sub_data <- ctl_comp_select(index_data)
      which_index <- input$ctl_comp_index
      
      ### Reactive Dataframe 
      data$ctl_comp_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Treatment, contains(which_index))
      
      ### Plot
      index_tograph <- sub_data %>%   #Choose index to graph
        rename_with(~ sub(which_index, 'index', .),contains(which_index)) %>% 
        mutate(index_se = index_sd/sqrt(index_n-1))
      
     p<-ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Site, customdata = Date)) +
        geom_point() + 
        geom_line(aes(linetype=Treatment)) + 
        geom_errorbar(aes(ymin = index_mean - index_se , ymax= index_mean + index_se)) + 
        labs(y = which_index) +
        facet_grid(. ~ Year) + 
        scale_color_manual(values = c("MAT89" = "green4", "MAT06" = "chartreuse3", "MAT81" = "darkgreen", "SHB89" = "orange4",   
                                      "MNT97" = "darkorchid", "NNT97" = "mediumpurple4", "WSG89" = "dodgerblue2",  "DHT89" = "firebrick"
                                      )) 
     plotly::ggplotly(p) %>% 
       layout(dragmode = "select") %>%
       event_register("plotly_selecting")    
      
    })
    ##  Site-Level Plots ----
    output$sitePlot <- renderPlotly({ ######## AGGREGRATE BY SITE
      sub_data <- bysite_select(index_data)
      which_index <- input$bysite_index
      
      ### Reactive Dataframe 
      data$bysite_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Treatment, contains(which_index))
      
      ### Plot
      index_tograph <- sub_data %>% #Choose index to graph
        rename_with(~ sub(which_index, 'index', .),contains(which_index)) %>% 
        mutate(index_se = index_sd/sqrt(index_n-1))
      
      plotly::ggplotly(
        ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment, customdata = Date)) +
        geom_point() + 
        geom_line() + 
        geom_errorbar(aes(ymin = index_mean - index_se , ymax= index_mean + index_se)) + 
        scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                    "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                    "P" = "red2",
                                    "NP" = "green4",
                                    "F0.5" = np_colors[1],
                                    "F1" = np_colors[2],
                                    "F2" = np_colors[3],
                                    "F5" = np_colors[4],
                                    "F10" = np_colors[5]))  + 
        theme_gray(base_size=18) +
        labs(y = which_index) + 
        facet_grid(Site ~ Year, labeller = labeller(Site = Site.text))
      ) %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting") 
    })
    

    ## By Block Plots  ----
    output$blockPlot <- renderPlotly({
      sub_data <- byblock_select(index_data)
      which_index <- input$byblock_index
      
      ### Reactive Dataframe
      data$byblock_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Block, Treatment, contains(which_index))
      
      ### Plot 
      index_tograph <- sub_data %>% #Choose index to graph
        #rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .)))%>% 
        rename_with(~ sub(which_index, 'index', .),contains(which_index)) %>% 
        mutate(index_se = index_sd/sqrt(index_n-1))
      
      p<- ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Block, customdata = Date)) +
        geom_point() + 
        geom_line() + 
        geom_errorbar(aes(ymin = index_mean - index_se , ymax= index_mean + index_se)) + 
        scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                    "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                    "P" = "red2",
                                    "NP" = "green4",
                                    "F0.5" = np_colors[1],
                                    "F1" = np_colors[2],
                                    "F2" = np_colors[3],
                                    "F5" = np_colors[4],
                                    "F10" = np_colors[5]))  + 
        theme_gray(base_size = 18) +
        labs(y = which_index) + 
        facet_grid(Treatment ~ Year)
      ggplotly(p) %>% 
         layout(dragmode = "select") %>%
         event_register("plotly_selecting") 
      
    })
    
    ## Plot-Level Plots ----
    output$plotPlot <- renderPlotly({ ######## AGGREGRATE BY PLOT
      sub_data <- byplot_select(index_data)
      which_index <- input$byplot_index
      
      ### Reactive Dataframe
      
      data$byplot_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Block, Treatment, Replicate, contains(which_index), -contains("sd"))
      
      ### Plot
      index_tograph <- sub_data %>% #Choose index to graph
        rename_with(~ sub(which_index, 'index', .),contains(which_index))
      
      
      ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment)) +
        geom_point(aes()) + 
        geom_line(aes(linetype=Replicate)) +
        guides(shape="none")+
        scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                    "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                    "P" = "red2",
                                    "NP" = "green4",
                                    "F0.5" = np_colors[1],
                                    "F1" = np_colors[2],
                                    "F2" = np_colors[3],
                                    "F5" = np_colors[4],
                                    "F10" = np_colors[5]))  + 
        theme_gray(base_size = 18) +
        labs(y = which_index) + 
        facet_grid(Block ~ Year)
      
      
    })
    
    
    
  }
  
)