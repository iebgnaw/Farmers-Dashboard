

server <- function(input, output) {
  
  
  
  ######################### Correlation Starts ##############################

  
  ############# Interactive Input #########
  
  ## Category for Variable 1
  output$choose_cat_1 = renderUI({
    pickerInput("E_cat_1",
                choices = c(
                  "Policy", "Property Safeness", "Property Condition","Client"
                )
    )
  })
  
  ## Variable 1
  output$choose_var_1 = renderUI({
    if(input$E_cat_1 == "Policy")
      pickerInput("E_var_1",
                  choices = var_policy,
                  options = pickerOptions(
                    liveSearchPlaceholder = "Search Here",
                    liveSearch = TRUE
                  )
      )
    
    else if(input$E_cat_1 == "Property Safeness")
      pickerInput("E_var_1",
                  choices = var_prosafeness)
    
    else if(input$E_cat_1 == "Property Condition")
      pickerInput("E_var_1",
                  choices = var_procondition)
    
    else if(input$E_cat_1 == "Client")
      pickerInput("E_var_1",
                  choices = var_client)
    
    
  })
  
  
  
  ## Category for variable 2
  output$choose_cat_2 = renderUI({
    pickerInput("E_cat_2",
                choices = c("Property Safeness", "Policy", "Property Condition","Client"
                            )

    )
  })
  
  
  ## Variable 2
  output$choose_var_2 = renderUI({
    if(input$E_cat_2 == "Policy")
      pickerInput("E_var_2",
                  choices = var_policy,
                  options = pickerOptions(
                    liveSearchPlaceholder = "Search Here",
                    liveSearch = TRUE
                  )
      )
    
    else if(input$E_cat_2 == "Property Safeness")
      pickerInput("E_var_2",
                  choices = var_prosafeness
      )
    
    else if(input$E_cat_2 == "Property Condition")
      pickerInput("E_var_2",
                  choices = var_procondition
      )
    
    else if(input$E_cat_2 == "Client")
      pickerInput("E_var_2",
                  choices = var_client
      )
    
    
  })
  
  ## Action Button
  output$actionbutton = renderUI({
    actionButton("E_action", label = "Confirm", width = 150,
                 status = "primary"
    )
  })
  
  ############## Plot #################
  
  ## Observe the action button
  observeEvent(input$E_action, {
    
  ## Grpah Determination ##  
    output$E_plot1 = renderPlot({
      
      ## Get column name of input variables
      E_var_x = as.character(name_list[name_list$ALIAS == input$E_var_1, 1])   
      E_var_y = as.character(name_list[name_list$ALIAS == input$E_var_2, 1])
    
    
      ## Decide the type of graph  
    graph = (
      # 1. Binary + Binary
      if(input$E_var_1 %in% var_binary & input$E_var_2 %in% var_binary)
        
        ggplot(data,aes_string(x=E_var_x,fill=E_var_y, group=E_var_y))+
        geom_bar(aes(y=..prop..), position = "dodge")+
        xlab(input$E_var_1)+
        ylab("Proportion")+
        labs(fill = input$E_var_2)+
        scale_fill_manual(values = c("lightgray","darkblue"))
      
      
      # 2. Binary + Categorical (1)
      else if(input$E_var_1 %in% var_binary & input$E_var_2 %in% var_categorical)
        
        ggplot(data,aes_string(x=E_var_y,fill=E_var_x, group=E_var_x))+
        geom_bar(aes(y=..prop..), position = "dodge")+
        xlab(input$E_var_2)+
        ylab("Proportion")+
        labs(fill = input$E_var_1)+
        scale_fill_manual(values = c("lightgray","darkblue"))+
        coord_flip()
      
      
      # 3. Binary + Categorical (2)
      else if(input$E_var_1 %in% var_categorical & input$E_var_2 %in% var_binary)
        
        ggplot(data,aes_string(x=E_var_x,fill=E_var_y, group=E_var_y))+
        geom_bar(aes(y=..prop..), position = "dodge")+
        xlab(input$E_var_1)+
        ylab("Proportion")+
        labs(fill = input$E_var_2)+
        scale_fill_manual(values = c("lightgray","darkblue"))+
        coord_flip()
      
      # 4. Categorical + Categorical
      else if(input$E_var_1 %in% var_categorical & input$E_var_2 %in% var_categorical)
        
        ggplot(data,aes_string(x=E_var_x, y=E_var_y))+
        geom_count(aes(color=..n.., size=..n.., guide = FALSE))+
        xlab(input$E_var_1)+
        ylab(input$E_var_2)+
        labs(color = "Number", size = "Number")
      
      # 5. Binary + Numeric (1)
      else if(input$E_var_1 %in% var_binary & input$E_var_2 %in% var_numeric)
        
        ggplot(data,aes_string(E_var_x, E_var_y))+
        geom_boxplot()+
        xlab(input$E_var_1)+
        ylab(input$E_var_2)
      
      # 6. Binary + Numeric (2)
      else if(input$E_var_2 %in% var_binary & input$E_var_1 %in% var_numeric)
        
        ggplot(data,aes_string(E_var_y, E_var_x))+
        geom_boxplot()+
        xlab(input$E_var_2)+
        ylab(input$E_var_1)
      
      # 7. Numeric + Numeric
      else if(input$E_var_1 %in% var_numeric & input$E_var_2 %in% var_numeric)
        
        ggplot(data,aes_string(E_var_x, E_var_y))+
        geom_point()+
        xlab(input$E_var_1)+
        ylab(input$E_var_2)
      
      # 8. Numeric + Categorical (1)
      else if(input$E_var_1 %in% var_categorical & input$E_var_2 %in% var_numeric)
        
        ggplot(data,aes_string(E_var_x, E_var_y))+
        geom_boxplot()+
        xlab(input$E_var_1)+
        ylab(input$E_var_2)
      
      # 9. Numeric + Categorical (2)
      else if(input$E_var_1 %in% var_numeric & input$E_var_2 %in% var_categorical)
        
        ggplot(data,aes_string(E_var_y, E_var_x))+
        geom_boxplot()+
        xlab(input$E_var_2)+
        ylab(input$E_var_1)
    )
    
    ## Decide the theme of graph
      graph +
      theme_classic()+
      ggtitle(paste(input$E_var_1,"vs.",input$E_var_2))+
      theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
            axis.text.x = element_text(size = 15,
                                       face = "bold"),
            axis.text.y = element_text(size = 15, 
                                       face = "bold"),
            axis.title.x = element_text(size = 18,
                                        face = "bold",
                                        vjust = -5),
            axis.title.y = element_text(size = 18,
                                        face = "bold",
                                        vjust = 7),
            plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12)
            ) 
   
  })# End of renderplot
  })  # End of Observe event
  ############# End ###################

  ######################### Distribution Graphs Start ##############################
  
  ##3.1 for renderUI
  output$vari <- renderUI({
    
    
    ## set a inputId called "c_vari" to satisfy four categories.     
    
    if(input$category == "Policy")
      pickerInput(inputId = "c_vari", 
                  label = "Variable", 
                  choices = var_policy_dis
      )
    
    else if(input$category == "Property Safeness")
      pickerInput(inputId = "c_vari", 
                  label = "Variable", 
                  choices = var_prosafeness_dis
      )
    
    else if(input$category == "Property Condition")
      pickerInput(inputId = "c_vari", 
                  label = "Variable", 
                  choices = var_procondition_dis
      )
    
    else if(input$category == "Client")
      pickerInput(inputId = "c_vari", 
                  label = "Variable", 
                  choices = var_client_dis
      )
    
    
  })
  

  ##3.2 for renderPlot

    
    output$c_plot <- renderPlot({
      
      req(input$c_vari)
      
      ## set a variable for ggplot's xlab and title      
      labx = name_list[name_list$COL == input$c_vari, 2]
      
      ## (1)for binary variables
      graph_dis = (
        if(input$c_vari %in% var_binary_dis)
          
          ggplot(data, aes_string(x = input$c_vari)) +
          geom_bar(aes(y = ..prop.., group = 1),width = 0.3, fill = "darkblue") +
          xlab(labx) +
          ylab("proportion")
        
        ## (2)for numerical variables    
        else if(input$c_vari %in% var_numeric_dis)
          ggplot(data, aes_string(x = input$c_vari)) +
          geom_density(alpha = 0.5, fill = "lightblue") +
          xlab(labx) +
          ylab("count")
        
        ## (3)for categorical variables     
        else if(input$c_vari %in% var_categorical_dis_1)
          ggplot(data, aes_string(x = input$c_vari)) +
          geom_bar(aes(y = ..prop.., group = 1),fill = "darkblue") +
          xlab(labx) +
          ylab("Proportion")
        
        ## (4)for categorical variables (coord_flip)      
        else if(input$c_vari %in% var_categorical_dis_2)
          ggplot(data, aes_string(x = input$c_vari)) +
          geom_bar(aes(y = ..prop.., group = 1),fill = "darkblue") +
          coord_flip() +
          xlab(labx) +
          ylab("Proportion")
        
        ## (5)for special categorical variables - DED_AMT_3
        else if(input$c_vari %in% c("DED_AMT_3"))    
          ggplot(data, aes_string(x = input$c_vari)) +
          geom_bar(aes(y = ..prop.., group = 1),width = 200, fill = "darkblue") +
          xlab(labx) +
          ylab("Proportion")
        
      )
      graph_dis +
        theme_classic() +
        ggtitle(paste("The distributions of",labx)) +
        theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
              axis.text.x = element_text(size = 15,
                                         face = "bold"),
              axis.text.y = element_text(size = 15, 
                                         face = "bold"),
              axis.title.x = element_text(size = 18,
                                          face = "bold",
                                          vjust = -5),
              axis.title.y = element_text(size = 18,
                                          face = "bold",
                                          vjust = 7),
              plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 12)
        )
  })  
  ##3.3 for renderTable    

    output$c_table <- renderTable({
      
      req(input$c_vari)
      
      ##(1)for binary and categorical variables
      
      if(input$c_vari %in% var_binacat_dis)    
        
        data %>%
        group_by_(Type = input$c_vari) %>% 
        summarise(Count = n()) 
      
      ##(2)for numerical variables
      
      else if(input$c_vari %in% var_numeric_dis)  
        
        data %>%
        summarise(Mean = mean(!! rlang::sym(input$c_vari)),
                  StandardDeviation = sd(!! rlang::sym(input$c_vari)))
  })
  
  
  
  ############# End ###################

  ######################### Comparison Graphs Start ###################
  output$X_choose_var = renderUI({
    if (input$X_choose_cat == 'Policy'){
      pickerInput(inputId = 'X_var', 
                  choices = colnames(X_data_policy))
    }
    else if (input$X_choose_cat == 'Property Condition'){
      pickerInput(inputId = 'X_var', 
                  choices = colnames(X_data_procondition))
    }
    else if (input$X_choose_cat == 'Property Safeness'){
      pickerInput(inputId = 'X_var', 
                  choices = colnames(X_data_prosafeness))
    }
    else if (input$X_choose_cat == 'Client'){
      pickerInput(inputId = 'X_var', 
                  choices = colnames(X_data_client))
    }
  })

  
  
  ## Observe the action button
  
    
    output$X_plot = renderPlot({
      input_col = as.character(name_list[name_list$ALIAS == input$X_var,1])
      
      if (input$X_var %in% colnames(X_data_numeric)){
        #1. numerical
        p1 = ggplot(data, aes(x = get(input_col), fill= AH_DISC_IND)) +
          geom_density(alpha = 0.6) +
          theme_classic() +
          scale_fill_manual(values =c('lightgrey','darkblue')) +
          xlab(input_col) +
          ylab('') +
          guides(fill=guide_legend(title="With/Without Auto Insurance")) +
          ggtitle(paste(input$X_var, 'in With/Without Auto Insurance Groups')) +
          theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
                axis.text.x = element_text(size = 15,
                                           face = "bold"),
                axis.text.y = element_text(size = 15, 
                                           face = "bold"),
                axis.title.x = element_text(size = 18,
                                            face = "bold",
                                            vjust = -5),
                axis.title.y = element_text(size = 18,
                                            face = "bold",
                                            vjust = 7),
                plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 12)
          )
        
        p2 = ggplot(data, aes(x = get(input_col), fill= AH_DISC_IND)) +
          geom_histogram(position = 'dodge', bins = 15) +
          theme_classic() +
          scale_fill_manual(values =c('lightgrey','darkblue')) +
          xlab(input_col) +
          ylab('Count') +
          guides(fill=guide_legend(title="With/Without Auto Insurance")) +
          theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
                axis.text.x = element_text(size = 15,
                                           face = "bold"),
                axis.text.y = element_text(size = 15, 
                                           face = "bold"),
                axis.title.x = element_text(size = 18,
                                            face = "bold",
                                            vjust = -5),
                axis.title.y = element_text(size = 18,
                                            face = "bold",
                                            vjust = 7),
                plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 12)
          ) 
        
        p3 = ggplot(data, aes(x = AH_DISC_IND, y = get(input_col))) +
          theme_classic() +
          geom_boxplot() +
          xlab('With/Without Auto Insurance') +
          ylab(input_col)  +
          theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
                axis.text.x = element_text(size = 15,
                                           face = "bold"),
                axis.text.y = element_text(size = 15, 
                                           face = "bold"),
                axis.title.x = element_text(size = 18,
                                            face = "bold",
                                            vjust = -5),
                axis.title.y = element_text(size = 18,
                                            face = "bold",
                                            vjust = 7),
                plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 12)
          )
        
        grid.arrange(p1, p2, p3, nrow = 3)
      }
      else if (input$X_var %in% colnames(X_data_binary)){
        #2. binary
        ggplot(data, aes(x = AH_DISC_IND, group = get(input_col), fill = get(input_col))) +
          geom_bar(aes(y = ..prop..), position = 'dodge', width = 0.5) +
          theme_classic() +
          scale_fill_manual(values = c('lightgrey','darkblue')) +
          ggtitle(paste(input$X_var, 'in With/Without Auto Insurance Groups')) +
          xlab('') +
          ylab('Proportion') +
          labs(fill = input$X_var) +
          coord_flip() +
          theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
                axis.text.x = element_text(size = 15,
                                           face = "bold"),
                axis.text.y = element_text(size = 15, 
                                           face = "bold"),
                axis.title.x = element_text(size = 18,
                                            face = "bold",
                                            vjust = -5),
                axis.title.y = element_text(size = 18,
                                            face = "bold",
                                            vjust = 7),
                plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 12)
          ) }
      
      else if (input$X_var %in% colnames(X_data_categorical)){
        #3. categorical
        ggplot(data, aes(x = get(input_col), group = AH_DISC_IND, fill = AH_DISC_IND)) +
          geom_bar(aes(y = ..prop..), position = 'dodge') +
          theme_classic() +
          scale_fill_manual(values = c('lightgrey','darkblue')) +
          xlab(input$X_var) +
          ylab('Proportion') +
          guides(fill=guide_legend(title="With/Without Auto Insurance")) + 
          coord_flip() +
          ggtitle(paste(input$X_var,'in With/Without Auto Insurance Groups')) +
          theme(plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
                axis.text.x = element_text(size = 15,
                                           face = "bold"),
                axis.text.y = element_text(size = 15, 
                                           face = "bold"),
                axis.title.x = element_text(size = 18,
                                            face = "bold",
                                            vjust = -5),
                axis.title.y = element_text(size = 18,
                                            face = "bold",
                                            vjust = 7),
                plot.margin = unit(c(0,6,1,6),"cm"), # top, right, bottom, left
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 12)
          )}
  })
  ############# End ###################
  
  ############# Map Starts ###################
  
  

  output$plot = renderPlot(
    height = 650,width = 800,
    {
      
      ### depending on 2 input variables, we are filtering number of rows in Farmers data
      
      if(input$number.of.discount != 'NA' & input$HM_UMB_DISC_IND != "Ni" & input$AH_DISC_IND != "nf" )
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount 
                           & HM_UMB_DISC_IND == input$HM_UMB_DISC_IND
                           & AH_DISC_IND == input$AH_DISC_IND )# filtering according to the variable input
      }
      else if (input$number.of.discount == 'NA' & input$HM_UMB_DISC_IND != "Ni" & input$AH_DISC_IND == "nf")
      {
        farmMap10 = filter(farmMap9,HM_UMB_DISC_IND == input$HM_UMB_DISC_IND)
      }
      else if (input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount != 'NA' & input$AH_DISC_IND == "nf")
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount)
      }
      else if (input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount == 'NA' & input$AH_DISC_IND != "nf")
      {
        farmMap10 = filter(farmMap9,AH_DISC_IND == input$AH_DISC_IND)
      }
      else if (input$HM_UMB_DISC_IND != "Ni" & input$number.of.discount == 'NA' & input$AH_DISC_IND != "nf")
      {
        farmMap10 = filter(farmMap9,AH_DISC_IND == input$AH_DISC_IND & HM_UMB_DISC_IND == input$HM_UMB_DISC_IND)
      }
      else if (input$HM_UMB_DISC_IND != "Ni" & input$number.of.discount != 'NA' & input$AH_DISC_IND == "nf")
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount & HM_UMB_DISC_IND == input$HM_UMB_DISC_IND)
      }
      else if(input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount != 'NA' & input$AH_DISC_IND != "nf")
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount & AH_DISC_IND == input$AH_DISC_IND )
      }
      else if(input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount == 'NA' & input$AH_DISC_IND == "nf")
      {
        farmMap10 =farmMap9
      }
      ####  grouping by zip code to take count of cusotmers/ homes 
      
      farmMap12 = farmMap10 %>% 
        group_by(PROP_ZIP_CD) %>% 
        summarize(Number_of_Policies = n(),propBoth = sum(AH_DISC_IND == "Y"), 
                  propHomeOnly = sum(AH_DISC_IND == "N"))
      
      ### left join with farmers data to add lat and long for each zip code
      
      farmMap13 = left_join(farmMap12,farmMap10, by = "PROP_ZIP_CD")
      farmMap13$group = substr(farmMap13$PROP_ZIP_CD,1,3)
      
      #### remove duplicate pin codes from the farmers data file 
      
      duplicated(farmMap13$PROP_ZIP_CD) # remove duplicate values
      farmMap13[duplicated(farmMap13$PROP_ZIP_CD),]
      unique(farmMap13[duplicated(farmMap13$PROP_ZIP_CD),])
      farmMap13 = farmMap13[!duplicated(farmMap13$PROP_ZIP_CD),]
      
      #Group By county to get no. of homes per county and Plot
      
      dataSumm =farmMap10 %>%
        group_by(County) %>%
        summarise(n = n(), n_Yes = sum(AH_DISC_IND == 'Y')) %>%
        mutate(percent = n_Yes/n*100)
      
      
      #left Join with cnames to get long and lat
      
      plotData1 = left_join(cnames,dataSumm, by = c("subregion" = "County"))
      
      # generating Arizona map with each point on map representing no. of policies in that zip code
      # Arizona map also contains county names, no of policies in a county
      
      ggplot(map1,aes_string(x="long", y= "lat")) + 
        geom_polygon(aes_string(group = "group"), fill="lightgrey",color = "black") + 
        #geom_polygon(data = farmMap13,aes(x=longitude, y =latitude, 
        # fill = propPerZip, group = group))
        geom_point(data = farmMap13,aes_string(x="longitude", y ="latitude",color = "Number_of_Policies"),size = 3, alpha = 0.8) + 
        geom_text(data=plotData1, aes_string("long", "lat", label = "subregion"), size=5, vjust = 2) + 
        geom_text_repel(data=plotData1, aes_string("long","lat", label = "n"), size=4, fontface = "bold") + 
        #scale_color_continuous() + 
        scale_color_gradient2(low = "blue", high = "darkblue") + 
        #scale_color_gradient2(low = "blue", high = "darkblue",limits = c(0,500)) +
        coord_fixed(3) + 
        coord_map("polyconic") + 
        theme_void(base_size = 16)
    ############# End ###################
    
  })
    
    output$Takeaway1 <- renderText({ 
      
      
      if(input$number.of.discount != 'NA' & input$HM_UMB_DISC_IND != "Ni" & input$AH_DISC_IND != "nf" )
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount
                           & HM_UMB_DISC_IND == input$HM_UMB_DISC_IND
                           & AH_DISC_IND == input$AH_DISC_IND )# filtering according to the variable input
      }
      else if (input$number.of.discount == 'NA' & input$HM_UMB_DISC_IND != "Ni" & input$AH_DISC_IND == "nf")
      {
        farmMap10 = filter(farmMap9,HM_UMB_DISC_IND == input$HM_UMB_DISC_IND)
      }
      else if (input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount != 'NA' & input$AH_DISC_IND == "nf")
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount)
      }
      else if (input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount == 'NA' & input$AH_DISC_IND != "nf")
      {
        farmMap10 = filter(farmMap9,AH_DISC_IND == input$AH_DISC_IND)
      }
      else if (input$HM_UMB_DISC_IND != "Ni" & input$number.of.discount == 'NA' & input$AH_DISC_IND != "nf")
      {
        farmMap10 = filter(farmMap9,AH_DISC_IND == input$AH_DISC_IND & HM_UMB_DISC_IND == input$HM_UMB_DISC_IND)
      }
      else if (input$HM_UMB_DISC_IND != "Ni" & input$number.of.discount != 'NA' & input$AH_DISC_IND == "nf")
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount & HM_UMB_DISC_IND == input$HM_UMB_DISC_IND)
      }
      else if(input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount != 'NA' & input$AH_DISC_IND != "nf")
      {
        farmMap10 = filter(farmMap9,number.of.discount == input$number.of.discount & AH_DISC_IND == input$AH_DISC_IND )
      }
      else if(input$HM_UMB_DISC_IND == "Ni" & input$number.of.discount == 'NA' & input$AH_DISC_IND == "nf")
      {
        farmMap10 =farmMap9
      }
      
      dataSumm =farmMap10 %>%
        group_by(County) %>%
        summarise(n = n(), n_Yes = sum(AH_DISC_IND == 'Y')) %>%
        mutate(percent = n_Yes/n*100)
      az_total = sum(dataSumm$n)
      
      
      paste("Total Number of Policies = ", az_total)
    })

  
}

shinyApp(ui, server)
