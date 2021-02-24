function(input,output){
  
  #for calculating the summary statistics for the food variables
  sum_stat <- reactive({
    food<- food %>% select(-Country, -Continent) %>% 
      select(input$variable) %>%
      summary()%>%
      as.data.frame() %>%
      separate(Freq, c("Stat", "Value"), sep=":") %>%
      spread(key = Stat, value = Value)
  })
  
  
  #the three plots are for comparing different food variables and covid confirmed and death cases depending upon the input continent
  output$plot = renderPlot(
    food %>% filter(Continent == input$continent)  %>% select(Country, Continent, Animal_products,Cereals, Milk,Fruits,Pulses, Vegetables,Vegetable_oils, Vegetal) %>% 
      gather(key = "variable", value = "Value", 3:9) %>% 
      ggplot(aes(x = variable, y = Value)) +  geom_boxplot(aes(fill = variable)) + labs( title = "Food categories profile for selected continent",x = "Food Variables", y = "Values in %") + 
      theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold",hjust = 0.5))
    
  )
  output$plot2 = renderPlot(
    food %>% filter(Continent == input$continent)  %>% select(Country, Continent, Confirmed) %>% 
      gather(key = "variable", value = "Value", 3) %>% 
      ggplot(aes(x = variable, y = Value)) +  geom_boxplot(fill = "blue") + labs(title ="COVID Confirmed cases", x = "", y = "Values  in %") + 
      theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5))
    
  )
  output$plot3 = renderPlot(
    food %>% filter(Continent == input$continent)  %>% select(Country, Continent, Deaths) %>% 
      gather(key = "variable", value = "Value", 3) %>% 
      ggplot(aes(x = variable, y = Value)) +  geom_boxplot(fill = "red") + labs(title ="COVID Death cases", x ="", y = "Values in %") + theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold",hjust = 0.5))
    
  )
  #plot for countrywise food profile of Food profile and COVID cases
  output$plot4 = renderPlot(
    food_country %>% filter(Country == input$country) %>% gather(key = "variable", value = "value", 2:24) %>%  mutate(variable = factor (variable, 
                                                                                                                                         levels = c( "Alcohol", "Animal_fats", "Animal_products","Aquatic_product", "Cereals","Eggs", "Fish_seafood", "Fruits", "Meat", "Milk", 
                                                                                                                                                     "Offals","Oilcrops", "Pulses", "Spices", "Starchy_roots", "Stimulants", "Sugar_crops","Sugar_sweetners", "Treenuts", "Vegetable_oils", "Vegetables","Obesity","Confirmed")))%>% 
      ggplot(aes(x = variable, y = value)) +geom_point(aes(color = variable), size = 5) + 
      labs(title = "Food profile and COVID cases for selected country", x = "", y = "Values in %") +
      theme(axis.text.x = element_text(face = "bold", angle = 45),
        plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5)) 
  )
  #The next two plots are for the Continent wise comparision of alcohol vs obesity vs Covid confirmed cases and deaths
  output$plot5 = renderPlot(
    food %>% filter(Continent == input$Continent)  %>% select(Country, Continent, Obesity, Alcohol,Confirmed) %>%
      gather(key = "variable", value = "Value", 3:5) %>% 
      ggplot(aes(x = variable, y = Value)) +  geom_boxplot(aes(fill = variable)) + labs( x = "", y = "Values in %") + theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5))
      
  )
  output$plot6 = renderPlot(
    food %>% filter(Continent == input$Continent)  %>% select(Country, Continent, Deaths) %>%
      gather(key = "variable", value = "Value", 3) %>% 
      ggplot(aes(x = variable, y = Value)) +  geom_boxplot(fill= "red") + labs( x = "COVID DEATHS", y = "Values in %") +theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5))
  )
  #Drawing the pie chart the food categories depending upon the different Obesity level (high, low)
  output$plot7 = renderPlot(
    food_obes %>% filter(obesity_level == input$obesity_level) %>% select(Alcohol, Animal_products, Cereals, Pulses, Milk, Fruits, Vegetables, Vegetable_oils) %>% 
      summarise_all(mean) %>% gather(key = "Variable", value = "avg") %>% ggplot(aes(x = "", y = avg, fill = Variable)) + geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = paste(round(avg / sum(avg) * 100,1), "%")),position = position_stack(vjust = 0.5)) + 
      theme(plot.title = element_text(hjust=0.5),axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(fill = "Category", x = NULL, y = NULL, title = "Pie Chart of food categories (Obesity)") + coord_polar("y") +
      scale_fill_brewer(palette="Set2") +theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5))
  )
  #Drawing the pie chart the food categories depending upon the different undernorished rate(high, middle, low)
  output$plot8 = renderPlot(
    food_UN %>% filter(UN_rate == input$undernorished) %>% select(Alcohol, Animal_products, Cereals, Pulses, Milk, Fruits, Vegetables, Vegetable_oils) %>% 
      summarise_all(mean) %>% gather(key = "Variable", value = "avg") %>% ggplot(aes(x = "", y = avg, fill = Variable)) + geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = paste(round(avg / sum(avg) * 100,1), "%")),position = position_stack(vjust = 0.5)) + 
      theme(plot.title = element_text(hjust=0.5),axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(fill = "Category", x = NULL, y = NULL, title = "Pie Chart of food categories (Undernorishment)") + coord_polar("y") +
      scale_fill_brewer(palette="Set1") + theme(
        plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5))
  )
  #The next two plots are correlation plots with different methods 
  output$plot9 = renderPlot(
    corrplot(food_corr, method = "circle", type = "upper", col=brewer.pal(n=8, name="Spectral"))
  )
  output$plot10 = renderPlot(
    corrplot(food_corr, method = "number", type = "upper", col=brewer.pal(n=8, name="Spectral"))
  )
  #Summary table for selected variables of the data
  output$table <- renderTable(sum_stat()
  )
  #Summarise the data analysis
  output$text = renderText({paste("Major takeaways from the Data")})
  output$text1 = renderText({paste("Obesity is the major factor in countries with higher Death rate due to COVID.")})
  output$text2 = renderText({paste("Undernorishment is less likely to play role in higher COVID confirmed cases or deaths.")})
  output$text3 = renderText({paste("Generally, animal products are consumed more in countries with higher COVID deaths.")})
  output$text4 = renderText({paste("Alcohol consumption though is little higher in countries with higher obesity, but was not able to show strong
                                   coorelation with COVID confirmed or death cases.")})
  
  
  
}