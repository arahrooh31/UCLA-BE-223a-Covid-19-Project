
#====================================
#     LA county-level FUNCTIONS     #
#====================================
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date, label = c("cases", "deaths")) {
  cv_aggregated$Date = as.Date(cv_aggregated$Date, format = "%Y-%m-%d")
  plot_df = subset(cv_aggregated, Date <= plot_date)
  var = if_else(label == "cases", "TotalCases", "TotalDeaths")
  col = if_else(label == "cases", 1, 2)
  
  g1 = ggplot(plot_df, aes(x = Date, y = as.numeric(plot_df[[var]]), color = county_col[col])) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab(paste0("Cumulative ", label)) + theme_bw() + xlab("Date") + 
    scale_colour_manual(values = c(county_col[col])) + 
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10), plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date, label = c("cases", "deaths")) {
  cv_aggregated$Date = as.Date(cv_aggregated$Date, format = "%Y-%m-%d")
  plot_df_new = subset(cv_aggregated, Date <= plot_date)
  var = if_else(label == "cases", "DailyCases", "NewDeaths")
  col = if_else(label == "cases", 1, 2)
  
  g1 = ggplot(plot_df_new, aes(x = Date, y = as.numeric(plot_df_new[[var]]), color = county_col[col])) + geom_bar(position = "stack", stat = "identity") + 
    ylab(paste0("New ", label)) + theme_bw() + xlab("Date") + 
    scale_colour_manual(values = c(county_col[col])) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10), plot.margin = margin(5, 12, 5, 5))
  g1
}


#====================================
#     community-level FUNCTIONS     #
#====================================
# function to plot new cases by region
community_cases_plot = function(input_df, plot_content = c("cumulative", "new", "new_7MA"), plot_date_end) {
  
  input_df$date = as.Date(input_df$date, format = "%Y-%m-%d")
  plot_start_date = as.Date(min(input_df$date, na.rm = T), "%Y-%m-%d")
  
  var = case_when(plot_content == "cumulative" ~ "cumulative_cases_today",
                  plot_content == "new" ~ "new_cases_today", 
                  plot_content == "new_7MA" ~ "case_incremental_7d_MA")
  y_label = case_when(plot_content == "cumulative" ~ "Cumulative cases",
                      plot_content == "new" ~ "New cases",
                      plot_content == "new_7MA" ~ "New cases (7-day moving average)")

  legend_lable = c(unique(input_df$place_ID))
  
  g = ggplot(input_df, aes(x = as.Date(date), y = as.numeric(input_df[[var]]), color = place_ID)) + 
    xlim(c(as.Date(plot_start_date), (as.Date(plot_date_end) + 1))) + xlab("Date")
  
  g1 = g +
    geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab(y_label) + theme_bw() + 
    scale_fill_manual(values = community_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size = 11)))
}

# test function
# cv_cases_longitudinal$date = as.Date(cv_cases_longitudinal$date, format = "%Y-%m-%d")
# input_df = cv_cases_longitudinal %>% filter(place_ID %in% c("West Hills", "East Los Angeles") )
# community_cases_plot(input_df, "new", "2020-06-01")


#=================================
#     BBN Modeling FUNCTIONS     #
#=================================
# Function for making inference given a BBN structure and data
inference_on_new_samples = function(structure, data){
  target_variable_index = 1 # Assuming the target variable is at the first index
  pred_test_samples_prob = c()
  pred_classes = c()
  
  for (test_index in 1:nrow(data)){
    testing_sample = data[test_index, ]
    # Evidence is all the features of this sample.
    column_names = names(data)
    event_yes = paste("(", column_names[target_variable_index], " == 'Yes')", sep = "")
    event_no = paste("(", column_names[target_variable_index], " == 'No')", sep = "")
    evidence = paste("(", column_names[-target_variable_index], " == '",sapply(testing_sample[-target_variable_index], as.character), "')",sep = "", collapse = " & ") 
    # Make prediction at both values that the target variable can take on, 
    cmd_yes = paste("cpquery(structure, ", event_yes, ", ", evidence, ")", sep = "")
    cmd_no = paste("cpquery(structure, ", event_no, ", ", evidence, ")", sep = "")
    prediction_yes = eval(parse(text = cmd_yes))
    prediction_no = eval(parse(text = cmd_no))
    pred_test_samples_prob[test_index] = prediction_yes
    # and choose the value that gives maximum probability as the predicted class
    if (prediction_yes > prediction_no){
      pred_classes[test_index] = "Yes"
    }
    else{
      pred_classes[test_index] = "No"
    }
  }
  
  return (list(pred_test_samples_prob, pred_classes))
}

# plot network func
# using the visNetwork package to plot the network because it looks very nice.
plot.network = function(structure, ht = "400px"){
  nodes.uniq = unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes = data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  
  edges = data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}

# Cross validation function
run_cross_validation = function(structure, data, num_folds){
  target_variable_index = 1
  beta = 2
  folds = cut(seq(1, nrow(data)), breaks = num_folds, labels = FALSE)
  F1_folds = c()
  F2_folds = c()
  sensitivity_folds = c()
  specificity_folds = c()
  current_best_sensitivity = 0
  
  pred_positive_class_prob = c() # This variable stores all the predicted positive class probability of each sample. Each sample will be a testing sample exactly once throughout cross validation.
  for (fold_index in 1:num_folds){
    fold_testing_index = which(folds == fold_index, arr.ind = TRUE)
    fold_testing_data = data[fold_testing_index, ]
    
    fold_training_data = data[-fold_testing_index, ]
    
    bn.mod_fold = bn.fit(structure, data = fold_training_data, method = "bayes")
    pred_classes = c()
    pred_test_samples_prob = c()
    for (test_index in 1:nrow(fold_testing_data)){
      testing_sample = fold_testing_data[test_index, ]
      
      # Evidence is all the features of this sample.
      column_names = names(fold_testing_data)
      event_yes = paste("(", column_names[target_variable_index], " == 'Yes')", sep = "")
      event_no = paste("(", column_names[target_variable_index], " == 'No')", sep = "")
      
      evidence = paste("(", column_names[-target_variable_index], " == '",sapply(testing_sample[-target_variable_index], as.character), "')",sep = "", collapse = " & ") 
      
      # Make prediction at both values that the target variable can take on, 
      cmd_yes = paste("cpquery(bn.mod_fold, ", event_yes, ", ", evidence, ")", sep = "")
      cmd_no = paste("cpquery(bn.mod_fold, ", event_no, ", ", evidence, ")", sep = "")
      prediction_yes = eval(parse(text = cmd_yes))
      prediction_no = eval(parse(text = cmd_no))
      pred_test_samples_prob[test_index] = prediction_yes
      # and choose the value that gives maximum probability as the predicted class
      if (prediction_yes > prediction_no){
        pred_classes[test_index] = "Yes"
      }
      else{
        pred_classes[test_index] = "No"
      }
    }
    
    pred_positive_class_prob = append(pred_positive_class_prob, pred_test_samples_prob, after = length(pred_positive_class_prob))
    target_classes = as.numeric(fold_testing_data[, target_variable_index])
    pred_classes_numeric = as.numeric(factor(pred_classes))
    
    # Calculate the evaluation metrics for this fold and save the values across folds
    sens_spec_fold = confusionMatrix(factor(pred_classes_numeric), factor(target_classes), positive="2", mode="everything", dnn = c("Outbreak", "No_outbreak"))
    
    sensitivity_folds[fold_index] = sens_spec_fold$byClass["Sensitivity"]
    specificity_folds[fold_index] = sens_spec_fold$byClass["Specificity"]
    F1_folds[fold_index] = sens_spec_fold$byClass["F1"]
    F2_folds[fold_index] = ((sens_spec_fold$byClass["Sensitivity"] * sens_spec_fold$byClass["Precision"]) * (1 + beta**2)) / ((beta**2 * sens_spec_fold$byClass["Precision"]) + sens_spec_fold$byClass["Sensitivity"])
    
    #Find the fold with the best sensitivity and save the model from that fold
    if (sensitivity_folds[fold_index] >= current_best_sensitivity){
      current_best_sensitivity = sensitivity_folds[fold_index]
      current_best_fold = fold_index
      saved_best_model = bn.mod_fold
    }
    
  }
  average_F1 = round(mean(na.omit(F1_folds)), 3)
  average_F2 = round(mean(na.omit(F2_folds)), 3)
  average_sensitivity = round(mean(na.omit(sensitivity_folds)), 3)
  average_specificity = round(mean(na.omit(specificity_folds)), 3)

  return (list(average_sensitivity, average_specificity, average_F1, average_F2, pred_positive_class_prob, saved_best_model))
}

print("Success checkpoint 1: read in all functions")






