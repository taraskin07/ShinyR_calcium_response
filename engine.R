suppressWarnings({
  library(renv)
  library(shiny)
  library(shinythemes)
  library(rsconnect)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(pastecs)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(gtools)
  library(tidyverse)
  library(DescTools)
  library(randomcoloR)
  library(zoo)
  library(data.table)
  library(shinyWidgets)
  library(shinyShortcut)
  library(shinyalert)
  library(shinyjs)
  library(formatR)
})

# source('modules.R')

# Customization functions -------------------------------------------------

customDT <- function(datatbl, scrollY = '850px') {
  return(datatable(
    datatbl,
    options = list(
      autoWidth = TRUE,
      paging = F,
      scrollCollapse = T,
      scrollX = T,
      scrollY = scrollY,
      selection = list(target = 'column'),
      search = list(smart = F)
    )
  ))
  
}


css_styles_DT <- function(datatbl) {
  if ('Missing' %in% colnames(datatbl)) {
    if (max(datatbl$Missing) != 0) {
      datatbl <- datatable(datatbl) %>%
        
        formatStyle(
          columns = c("Missing", "Time_points_amount"),
          valueColumns = "Missing",
          color = styleInterval(c(0, nrow(datatbl)), c('black', 'whate', 'white')),
          backgroundColor = styleInterval(c(0, nrow(datatbl)), c('yellow', 'red', 'red'))
        )
    }
  }
  
  
  
  return(datatbl)
}



# Preliminary analysis -----------------------------------------------------


# Resolving "save sheets problem"
list_to_save <-
  function(list_of_df,
           list_of_names,
           list_of_booleans) {
    if ((length(list_of_df) == length(list_of_names)) &
        (length(list_of_names) == length(list_of_booleans))) {
      finlist <- setNames(list_of_df, list_of_names)
      finlist <- finlist[list_of_booleans]
      
      
    } else {
      stop(safeError(
        paste0(
          length(list_of_df),
          length(list_of_names),
          length(list_of_booleans),
          "List's length are not equal!"
        )
      ))
    }
    
    return(finlist)
    
  }






# Reading XLS -------------------------------------------------------------
reading_xls <-
  function(file,
           disp_opt,
           correct_time,
           step,
           change_names,
           cnames,
           sheet_n) {
    # Function just read the excel file (specific sheet == sheet_n)
    
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read_excel(file$datapath,
                       sheet = sheet_n)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError('Waiting data to load!'))
    })
    
    if ((correct_time %% 2) == 1) {
      df <- time_col_name(df, step = step)
    }
    
    
    if (change_names == "zeroes") {
      df <- rename_columns_zeros(df, cnames)
    } else if (change_names == "number") {
      df <- rename_columns(df, cnames)
    }
    
    
    if (disp_opt == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  }


# Extracting name ---------------------------------------------------------

filename <- function(file_name, str_name) {
  name <- unlist(strsplit(toString(file_name), split = '[.]'))
  res_name <- paste(name[1], str_name, sep = '-')
  return(res_name)
}



# CORRECT LATER -----------------------------------------------------------

custom_filename <- function(file_name, str_name) {
  nname <-
    str_extract(file_name, '(^\\d{4}-\\d{2}-\\d{2}-\\w+\\d+)\\D', group = 1)
  res_name <- paste0(nname, '-', str_name)
  return(res_name)
}


# Correcting Time columns -------------------------------------------------
time_col_name <- function(datafr,
                          step = 5,
                          name_only = FALSE) {
  time_col <- '([Tt]ime\\s?)'
  colnames(datafr)[grepl(time_col, colnames(datafr))] =  "Time"
  
  if (grep(time_col, colnames(datafr)) != 1) {
    datafr_temp[1] <- datafr[grep(time_col, colnames(datafr))]
    datafr <-
      cbind(datafr_temp, datafr[-grep(time_col, colnames(datafr))])
    
  }
  
  
  
  
  if (name_only == FALSE) {
    datafr$Time <-
      seq(from = 0,
          by = step,
          length.out = length(datafr$Time))
    
  }
  
  return(datafr)
}



# Correcting names columns ------------------------------------------------
rename_columns <- function(df, cnames) {
  df <- time_col_name(df, name_only = T)
  
  df_output <- df %>%
    rename_with(.cols = -matches("Time"),
                # selects all columns except "Time"
                ~ paste0(
                  cnames,
                  stringr::str_extract(.x, "\\b\\D*?0*([1-9][0-9]*)",
                                       group = 1)
                ))
  return(df_output)
}

adding_zeroes <- function(vctr, cols = 3) {
  if (cols - stringr::str_length(vctr) < 0) {
    strnum <- toString(vctr)
  } else {
    strnum <-
      paste0(strrep('0', cols - stringr::str_length(vctr)), vctr)
  }
  
  return(strnum)
}

rename_columns_zeros <- function(df, cnames) {
  df <- time_col_name(df, name_only = T)
  
  
  df_output <- df %>%
    rename_with(.cols = -matches("Time"), # selects all columns except "Time"
                ~ paste0(cnames,
                         unname(
                           sapply(
                             stringr::str_extract(.x, "\\b\\D*?0*([1-9][0-9]*)",
                                                  group = 1),
                             adding_zeroes,
                             cols = nchar(as.character(max(
                               as.numeric(na.omit(
                                 stringr::str_extract(.x,
                                                      "\\b\\D*?0*([1-9][0-9]*)",
                                                      group = 1)
                               ))
                             )))
                           )
                         )))
  return(df_output)
}


# Dividing values of 340 & 380 dataframes -> custom_ratio -----------------
custom_ratio <- function(df1, df2) {
  tryCatch({
    df_custom_ratio <- df1 / df2
  },
  error = function(e) {
    shinyalert(
      type = 'error',
      text = "Numerator and Denominator tables are not equal!\n
                 You won't be able to save excel files!\n
                 Consider to load only 'Ratio' data or fix your file.",
      showConfirmButton = T
    )
    
    # return a safeError if a parsing error occurs
    stop(safeError(e))
  })
  
  df_custom_ratio[1] <- df1[1]
  return(df_custom_ratio)
}



# Calculating basic statistics for initial time series --------------------
basic_statistics <- function(df) {
  df <- df %>%
    distinct(across(-1))
  
  res <- stat.desc(df)
  res_t <- as.data.frame(t(as.matrix(res)))
  res_a <- data.frame(
    Cell = rownames(res_t),
    Min = decim(res_t$min, 3),
    Max = decim(res_t$max, 3),
    Difference = decim(res_t$range, 3),
    Mean = decim(res_t$mean, 3),
    Median = decim(res_t$median, 3),
    SD.mean = decim(res_t$std.dev, 3),
    SE.mean = decim(res_t$SE.mean, 3),
    Time_points_amount = res_t$nbr.val,
    Missing = res_t$nbr.na
  )
  
  
  return(as.data.frame(res_a))
  
}




# Plotting ggploly graph --------------------------------------------------

ggplotly_render <- function(df_n,
                            baseline = FALSE,
                            b_min = 0,
                            b_max = 120,
                            region = FALSE,
                            r_min = 130,
                            r_max = 330,
                            ready = TRUE,
                            rcolor = 'black',
                            sorting = 'Native') {
  df_n <- time_col_name(df_n, name_only = T)
  
  
  df <- df_n %>%
    pivot_longer(!Time, names_to = "cells", values_to = "Signal")
  
  # unique_vals <- length(unique(df$cells))
  
  
  
  
  # color = reorder(cells, as.numeric(gsub("cell-", "", cells))))
  
  # color = reorder(cells, mixedorder(cells))
  
  if (sorting == 'Native') {
    p <- ggplot(df, aes(
      Time,
      Signal,
      group = cells,
      color = factor(cells,
                     colnames(df_n)[-1])
    ))
  } else if (sorting == 'Regex') {
    p <- ggplot(df, aes(
      Time,
      Signal,
      group = cells,
      color = reorder(cells,
                      as.numeric(
                        stringr::str_extract(cells,
                                             '\\b\\D*?0*([1-9][0-9]*)',
                                             group = 1)
                      ))
    ))
  } else if (sorting == 'Mixed') {
    p <- ggplot(df, aes(
      Time,
      Signal,
      group = cells,
      color = reorder(cells,
                      mixedorder(cells))
    ))
  } else if (sorting == 'Mixed_revered') {
    p <- ggplot(df, aes(
      Time,
      Signal,
      group = cells,
      color = reorder(cells,
                      mixedorder(cells, decreasing = TRUE))
    ))
    
  }
  
  p <- p +
    labs(color = "Traces") +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = rcolor)
  
  
  if (baseline == T) {
    p <- p +
      geom_vline(xintercept = b_max,
                 colour = "black",
                 linetype = "longdash") +
      geom_vline(xintercept = b_min,
                 colour = "black",
                 linetype = "longdash")
  }
  
  if (region == T) {
    p <- p +
      geom_vline(xintercept = r_max,
                 colour = "red",
                 linetype = "dotted") +
      geom_vline(xintercept = r_min,
                 colour = "red",
                 linetype = "dotted")
  }
  
  if (ready == T) {
    return(ggplotly(p))
  } else {
    return(p)
  }
  
  
}

plot_wrapper <-
  function(plot_object,
           slider1,
           slider2,
           slider3,
           slider4) {
    if (!is.null(slider1)) {
      plot_object <- plot_object +
        geom_vline(
          xintercept = slider1[2],
          colour = "blue",
          linetype = "dotted",
          size = 1
        ) +
        geom_vline(
          xintercept = slider1[1],
          colour = "blue",
          linetype = "dotted",
          size = 1
        )
    }
    
    if (!is.null(slider2)) {
      plot_object <- plot_object +
        geom_vline(
          xintercept = slider2[2],
          colour = "darkgreen",
          linetype = "dotted",
          size = 1
        ) +
        geom_vline(
          xintercept = slider2[1],
          colour = "darkgreen",
          linetype = "dotted",
          size = 1
        )
    }
    
    if (!is.null(slider3)) {
      plot_object <- plot_object +
        geom_vline(
          xintercept = slider3[2],
          colour = "red",
          linetype = "dotted",
          size = 1
        ) +
        geom_vline(
          xintercept = slider3[1],
          colour = "red",
          linetype = "dotted",
          size = 1
        )
    }
    
    if (!is.null(slider4)) {
      plot_object <- plot_object +
        geom_vline(
          xintercept = slider4[2],
          colour = "#00BBBB",
          linetype = "dotted",
          size = 1
        ) +
        geom_vline(
          xintercept = slider4[1],
          colour = "#00BBBB",
          linetype = "dotted",
          size = 1
        )
    }
    
    return(plot_object)
    
  }


random_color_generator <- function(df_n) {
  df_n <- time_col_name(df_n, name_only = T)
  
  
  df <- df_n %>%
    pivot_longer(!Time, names_to = "cells", values_to = "Signal")
  
  unique_vals <- length(unique(df$cells))
  
  color_values = randomColor(count = unique_vals,
                             hue = 'random',
                             luminosity = 'bright')
  
  return(color_values)
  
}


color_palette <- function(df, colors2000) {
  pattern <- '\\b\\D*?0*([1-9][0-9]*)'
  
  current_palette <- character()
  
  for (idx in 2:ncol(df)) {
    color_id <- str_extract(colnames(df)[idx], pattern, group = 1)
    
    if (idx > 2000) {
      color_id <- sample.int(1999, 1)
    }
    
    current_palette <-
      c(current_palette, colors2000[[as.integer(color_id)]])
    
  }
  
  return(current_palette)
}

# Creating a subset for a single plot to display --------------------------------------------------

display_single_plot <-
  function(df,
           cell_name,
           ready = T,
           lines = F) {
    df <- time_col_name(df, name_only = T)
    
    if (ready == T) {
      plot <- ggplotly_render(df[c('Time', cell_name)], ready = ready)
      
      return(ggplotly(plot))
      
    } else if (lines == T) {
      plot <- ggplotly_render(df[c('Time', cell_name)], ready = FALSE)
      
      return(plot)
      
    } else {
      return(df[c('Time', cell_name)])
      
    }
    
  }

# REMOVE LATER -----------------------------------------------------------

# Constructing cell's names --------------------------------------------------

constructing_names <- function(cell_name, cell_number, format) {
  if (format == "zeroes") {
    comb <- paste0(cell_name, adding_zeroes(cell_number))
    
  } else {
    comb <- paste0(cell_name, cell_number)
  }
  
  return(comb)
}

# CORRECT LATER -----------------------------------------------------------
# Choose the specific column of the DataFrame -----------------------------

get_col_names <- function(df, cell_name, cell_number, format) {
  col <- constructing_names(cell_name, cell_number, format)
  
  time_col <- colnames(df[1])
  
  return(df[c(time_col, col)])
  
}



# CORRECT LATER -----------------------------------------------------------


# Function to find a specific cell related to its number
finding_cell_name <- function(data, number) {
  list_of_names <- colnames(data)
  match <- paste0('(\\D0*)', number, '($|\\s)')
  cell_name <- list_of_names[grepl(match, list_of_names)]
  
  if (length(cell_name) > 1) {
    stop("More than one column with similar name!")
  }
  return(cell_name)
}


# ERASE LATER -------------------------------------------------------------

# get_col_names alternative

single_plot <- function(df, cell_number) {
  col <- finding_cell_name(df, cell_number)
  
  df <- time_col_name(df, name_only = T)
  
  return(df[c('Time', col)])
  
}




# Make "cell_number" first column of Basic Statistics dataframe

cell_number_row <- function(df) {
  df$cell_number <- rownames(df)
  df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
  return(df)
}






# Analyzing amplitude -----------------------------------------------------




# Reading CLEAN XLS -------------------------------------------------------------
reading_clean_xls <- function(file, sheet_n) {
  # Function just read the excel file (specific sheet == sheet_n)
  
  
  # when reading semicolon separated files,
  # having a comma separator causes `read.csv` to error
  tryCatch({
    df <- read_excel(file$datapath,
                     sheet = sheet_n)
  },
  error = function(e) {
    # return a safeError if a parsing error occurs
    stop(safeError(e))
  })
  
} # reading_clean_xls




# Calculating amplitude ---------------------------------------------------

find_amplitude <-
  function(clean_df,
           min_time,
           max_time,
           start_time,
           end_time) {
    # Obtaining long-format dataframe
    df_long <- clean_df %>%
      pivot_longer(!Time, names_to = "cell", values_to = "value")
    
    # For future sorting saving correct order of cell names
    cell_index <- colnames(clean_df[-1])
    
    # Obtaining subset for baseline in accordance with min_time and max_time
    subset_timerange <-
      subset(df_long, (Time >= min_time & Time <= max_time))
    
    # Obtaining subset for the Region of interest in accordance with start_time and end_time
    subset_regint <-
      subset(df_long, (Time >= start_time & Time <= end_time))
    
    # Maximum values for initial dataset
    result_max <- summarize(
      group_by(subset_regint, cell),
      Maximum = decim(max(value, na.rm = T), 3),
      Max_Time = paste(Time[which(value == Maximum)], collapse = ", ")
    )
    
    
    # Global values for initial dataset
    result_global <- summarize(
      group_by(df_long, cell),
      Global_Min_Values = decim(min(value, na.rm = T), 3),
      Global_Min_Time = paste(Time[which(value == Global_Min_Values)], collapse = ", "),
      Global_Max_Time = paste(Time[which(value == max(value, na.rm = T))], collapse = ", ")
    )
    
    
    
    
    # Minimum values for baseline subset
    result_min <- summarize(
      group_by(subset_timerange, cell),
      Baseline_Median = decim((median(value, na.rm = T)), 3),
      Baseline_SD = decim(sd(value, na.rm = T), 3),
      Baseline_CV = decim(100 * sd(value, na.rm = T) /
                            mean(value, na.rm = T), 0),
      Baseline_Minimum = decim(min(value, na.rm = T), 3),
      Baseline_Mean = decim(mean(value, na.rm = T), 3),
      Baseline_Min_Time = paste(Time[which(value == Baseline_Minimum)], collapse = ", ")
    )
    
    # Inner join of two resulting tables
    result_amplitude <- merge(result_max, result_min, by = "cell")
    result_amplitude <-
      merge(result_amplitude, result_global, by = "cell")
    
    # Adding additional columns - Difference and Amplitude
    result_amplitude_final <- result_amplitude %>%
      add_column(Difference = abs(decim((
        result_amplitude$Baseline_Median - result_amplitude$Baseline_Mean
      ),
      3
      ))) %>%
      add_column(Amplitude = decim((
        result_amplitude$Maximum - as.numeric(result_amplitude$Baseline_Median)
      ), 3)) %>%
      arrange(factor(cell, cell_index)) %>%
      select(
        cell,
        Amplitude,
        Maximum,
        Baseline_Median,
        Baseline_Mean,
        Difference,
        Baseline_SD,
        Baseline_CV,
        Global_Min_Values,
        Global_Min_Time,
        Baseline_Min_Time,
        Max_Time,
        Global_Max_Time
      )
    
    return(result_amplitude_final)
    
  }



# Another function to calculate MINIMUNM (380 nm) - refactor later!!!!



find_amplitude_Den <-
  function(clean_df,
           min_time,
           max_time,
           start_time,
           end_time) {
    # Obtaining long-format dataframe
    df_long <- clean_df %>%
      pivot_longer(!Time, names_to = "cell", values_to = "value")
    
    # For future sorting saving correct order of cell names
    cell_index <- colnames(clean_df[-1])
    
    # Obtaining subset for baseline in accordance with min_time and max_time
    subset_timerange <-
      subset(df_long, (Time >= min_time & Time <= max_time))
    
    # Obtaining subset for the Region of interest in accordance with start_time and end_time
    subset_regint <-
      subset(df_long, (Time >= start_time & Time <= end_time))
    
    # Minimum values for initial dataset
    result_min <- summarize(
      group_by(subset_regint, cell),
      Minimum = decim(min(value, na.rm = T), 3),
      Min_Time = paste(Time[which(value == Minimum)], collapse = ", ")
    )
    
    
    # Global values for initial dataset
    result_global <- summarize(
      group_by(df_long, cell),
      Global_Max_Values = decim(max(value, na.rm = T), 3),
      Global_Max_Time = paste(Time[which(value == Global_Max_Values)], collapse = ", "),
      Global_Min_Time = paste(Time[which(value == min(value, na.rm = T))], collapse = ", ")
    )
    
    
    # Maximum values for baseline subset
    result_max <- summarize(
      group_by(subset_timerange, cell),
      Baseline_Median = decim((median(value, na.rm = T)), 3),
      Baseline_SD = decim(sd(value, na.rm = T), 3),
      Baseline_CV = decim(100 * sd(value, na.rm = T) /
                            mean(value, na.rm = T), 0),
      Baseline_Maximum = decim(max(value, na.rm = T), 3),
      Baseline_Mean = decim(mean(value, na.rm = T), 3),
      Baseline_Max_Time = paste(Time[which(value == Baseline_Maximum)], collapse = ", ")
    )
    
    # Inner join of two resulting tables
    result_amplitude <- merge(result_min, result_max, by = "cell")
    result_amplitude <-
      merge(result_amplitude, result_global, by = "cell")
    
    # Adding additional columns - Difference and Amplitude
    result_amplitude_final <- result_amplitude %>%
      add_column(Difference = abs(decim((
        result_amplitude$Baseline_Mean - result_amplitude$Baseline_Median
      ),
      3
      ))) %>%
      add_column(Amplitude = -decim((
        result_amplitude$Minimum - as.numeric(result_amplitude$Baseline_Median)
      ), 3)) %>%
      arrange(factor(cell, cell_index)) %>%
      select(
        cell,
        Amplitude,
        Minimum,
        Baseline_Median,
        Baseline_Mean,
        Difference,
        Baseline_SD,
        Baseline_CV,
        Global_Max_Values,
        Global_Max_Time,
        Baseline_Max_Time,
        Min_Time,
        Global_Min_Time
      )
    
    return(result_amplitude_final)
    
    
    
  }






# Rounding values ---------------------------------------------------------

decim <- function(number, digits = 3) {
  round_result <-
    as.numeric(format(round(number, digits), nsmall = digits))
  
  return(round_result)
}


dcm <- Vectorize(decim)


# Amplitude Statistics ----------------------------------------------------

summarize_amplitudes <- function(amplitude, excl_df) {
  ampl_calculating <- amplitude %>%
    summarize(
      Amplitude_average = decim(mean(Amplitude, na.rm = T), 3),
      Amplitude_SD = decim(sd(Amplitude, na.rm = T), 3),
      Amplitude_3SD = decim(3 * Amplitude_SD, 3),
      Amplitude_CV_percent = decim(100 * Amplitude_SD / Amplitude_average, 2),
      Average_Baseline_CV_percent = decim(mean(Baseline_CV, na.rm = T), 2),
      Amount_of_cells = nrow(amplitude)
    ) %>%
    add_column(Bad_cells = nrow(excl_df)) %>%
    add_column(Percent_of_bad_cells = decim((100 * nrow(excl_df) / nrow(amplitude)), digits = 1)) %>%
    select(
      Amplitude_average,
      Amplitude_CV_percent,
      Average_Baseline_CV_percent,
      Amount_of_cells,
      Bad_cells,
      Percent_of_bad_cells,
      Amplitude_SD,
      Amplitude_3SD
    )
  
  
  
  return(as.data.frame(ampl_calculating))
  
}




# Shifting curves ---------------------------------------------------------


# Function finds the response-specific maximum for each trace
# and creates resulting list of indices = response-specific maximums
finding_local_maximum <- function(ts_table, start_time, end_time) {
  # Correcting Time column if not Time and not first in the dataframe
  dfts <- time_col_name(ts_table, name_only = T)
  
  # Creating k=step value
  timeStep <- unique(diff(dfts$Time))
  
  
  if (length(timeStep) != 1) {
    
    showModal(modalDialog(
      title = "Unequal step for the Time column!",
      "Check the Time column step! It should be equal!",
      footer = NULL
    ))
    
    stop('Check the Time column step! It should be equal!')
  }
  
  index1 <- min(which(dfts$Time >= start_time))
  index2 <- max(which(dfts$Time <= end_time))
  
  
  max_indices <- apply(dfts[index1:index2,], 2, which.max)
  
  initial_indicies <- max_indices + index1 - 1
  
  indices_list <- initial_indicies[-1]
  
  # Establishing the earliest maximum among all the traces
  lowest_value <- min(unlist(indices_list))
  
  # And its name
  trace_name <- names(which.min(unlist(indices_list)))
  
  # Creating list of lag values as compared to the one with the earliest maximum
  difference <- sapply(indices_list, function(x)
    x - lowest_value)
  
  
  
  return(difference)
  
}

# Shifting curves (matching maximums)
shift_to_match_maximum <- function(df_to_shift, difference) {
  # For Shiny R only
  withProgress(message = "Shifting...", value = 0, {
    count <- 0
    for (element in names(difference)) {
      df_to_shift[[element]] <- shift(df_to_shift[[element]],
                                      n = difference[[element]],
                                      type = 'lead')
      # For Shiny R only
      count <- count + 1
      incProgress(1 / length(names(difference)), detail = paste("Shifting trace", count))
    }
    
  })
  
  return(df_to_shift)
}



# Shifting curves (CCF)
CCF_matrix <- function(df_to_shift, lower, upper, max_lag) {
  # Correcting Time column if not Time and not first in the dataframe
  df_time <- time_col_name(df_to_shift, name_only = T)
  
  # Subsetting according the range (lower-upper)
  subset_timerange <-
    as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))[-1]
  
  # Creating CCF matrix
  CCF_matrix = matrix(
    numeric(),
    nrow = ncol(subset_timerange),
    ncol = ncol(subset_timerange)
  )
  rownames(CCF_matrix) = colnames(subset_timerange)
  colnames(CCF_matrix) = colnames(subset_timerange)
  
  # For shiny R only
  withProgress(message = "Calculating...", value = 0, {
    count <- 0
    len <- length(colnames(subset_timerange))
    
    for (i in 1:ncol(subset_timerange)) {
      
      columnName <- colnames(subset_timerange)[[i]]
      count <- count + 1
      
      for (j in i:ncol(subset_timerange)) {
        
        rowName <- colnames(subset_timerange)[[j]]
        
        if (j == i) {
          
          CCF_matrix[rowName, columnName] = 0
          
        } else {
          
          mtrx <- ccf(
            subset_timerange[columnName],
            subset_timerange[rowName],
            lag.max = max_lag,
            na.action = na.omit,
            plot = FALSE
          )
          
          data_table <- data.frame(ACF = mtrx$acf, Lag = mtrx$lag)
          lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
          CCF_matrix[rowName, columnName] = lag_for_max_acf
          CCF_matrix[columnName, rowName] = -lag_for_max_acf
        }
        
      }
      
      # For shiny R only
      incProgress(1 / len, detail = paste("Processing trace", count))
    }
    
  })
  return(CCF_matrix)
}

# For Shiny R interactive only
shift_with_CCF <- function(df_to_shift, CCF_matrix, max_lag) {
  # Correcting Time column if not Time and not first in the dataframe
  df_time <- time_col_name(df_to_shift, name_only = T)
  
  if (max(CCF_matrix) == max_lag) {
    shinyalert(
      type = 'error',
      text = "Lag value for some trace is the same as the maximum lag value entered!\nYou should consider to increase the maximum lag value or use another algorithm!",
      closeOnClickOutside = T,
      showConfirmButton = T
    )
    
  }
  
  # Columns contain information about a trace that should be the reference (CCF < 0)
  # So minimum in CCF_matrix == reference
  # Rows for the case when CCF > 0
  # So maximum in CCF_matrix == reference
  
  column_sums <- colSums(CCF_matrix)
  left_trace_column <- names(which(column_sums == min(column_sums)))[[1]]
  
  list_subtracted <- named_list_of_lag(df_time, CCF_matrix)

  for (nm in colnames(df_time)[-1]) {
    
    df_time[[nm]] <- shift(df_time[[nm]], list_subtracted[[nm]])
    
  }
  
  # for (nm in colnames(df_time)[-1]) {
  #   
  #   n = CCF_matrix[nm, left_trace_column]
  #   
  #   if (n <= 0) {
  #     df_time[[nm]] <- shift(df_time[[nm]], n)
  #   } else {
  #     df_time[[nm]] <- df_time[[nm]]
  #   }
  #   
  #   
  # }
  
  
  return(df_time)
  
}


named_list_of_lag <- function(df_time, CCF_matrix) {
  
  named_list <- list() 
  
  for (nm in colnames(df_time)[-1]) {
    n = CCF_matrix[nm, left_trace_column]
    
    named_list[[nm]] <- n 
  }
  
  max_shift <- max(unlist(named_list))
  
  list_subtracted <- lapply(named_list, function(x) x - max_shift)
  

  return(list_subtracted)
}


# Basic function to determine lag and its sign between two series of values
lag_and_sign <- function(maximum_after, maximum_before, max_lag) {
  # Cross-correlation function, omits any NA values and skip plot rendering
  mtrx <-
    ccf(
      maximum_after,
      maximum_before,
      lag.max = max_lag,
      na.action = na.omit,
      plot = FALSE
    )
  
  #To get data from resulting table - new dataframe is constructed with all the necessary information
  data_table <-
    data.frame(ACF = mtrx$acf,
               Lag = mtrx$lag,
               N = mtrx$n.used)
  
  #Lag that is corresponded to the maximum CCF (ACF) value
  lag_for_max_acf <- data_table$Lag[which.max(data_table$ACF)]
  
  
  # Can be >0 (maximum_after, maximum_before) or <0 (maximum_before, maximum_after) or 0 (maximum_before = maximum_after)
  return(lag_for_max_acf)
}






# Function to find curve with maximum that is closer to the left (earlier time) and lag_and_sign the reference to it
finding_shifted_curve <-
  function(df,
           main_cell_number,
           lower,
           upper,
           max_lag) {
    # Fixing the 'Time' column if necessary
    
    df_time <- time_col_name(df, name_only = T)
    
    
    
    # print(paste0('1    ',df_time))
    # Obtaining list of dataframe column names
    list_of_names <- colnames(df_time)
    # print(paste0('2    ',list_of_names))
    # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
    subset_timerange <-
      as.data.frame(subset(df_time, (Time >= lower & Time <= upper)))
    # print(paste0('3    ',subset_timerange))
    # Regular expression to identify cell with the number input given
    match <- paste0('(\\D0*)', main_cell_number, '($|\\s)')
    # print(paste0('4    ',match))
    # Current reference cell which probably would be changed (its number) during the process of finding the one with the earliest maximum
    reference <- list_of_names[grepl(match, list_of_names)]
    
    if (length(reference) > 1) {
      stop("More than one column with similar name!")
    }
    # print(paste0('reference    ',reference))
    # The cell, that was chosen by the operator as a reference
    main_cell <- list_of_names[grepl(match, list_of_names)]
    # print(paste0('main_cell    ',main_cell))
    # Excluding time column from column names list
    coln_df <- list_of_names[list_of_names != "Time"]
    # print(coln_df)
    # Finding the cell with the earliest maximum, skipping the Time column (coln_df instead of list_of_names)
    for (cell in coln_df) {
      if (lag_and_sign(subset_timerange[, cell], subset_timerange[, reference], max_lag) < 0) {
        # print(paste0('7    ',cell))
        reference <- cell
        
      }
      
    }
    
    
    # Shifting main series to the left in order to correlate with the reference (with the earliest maximum)
    # Position of the maximum CCF (ACF) value = lag to choose
    lag_for_max_acf <-
      lag_and_sign(subset_timerange[, main_cell], subset_timerange[, reference], max_lag)
    # print(lag_for_max_acf)
    # print(length(subset_timerange[, main_cell]))
    
    # Shifting initial dataframe column, related to the main_cell, that was chosen by the operator
    shifted_main_cell_values <-
      as.data.frame(subset_timerange[, main_cell][(lag_for_max_acf + 1):length(subset_timerange[, main_cell])])
    colnames(shifted_main_cell_values) <- main_cell
    
    # # Series of values, shifted to the left, without NA values (shorter series instead) from the main cell column that was chosen
    return(shifted_main_cell_values)
    
  }




shifting_curves <-
  function(df,
           shifted_main_cell_values,
           lower,
           upper,
           max_lag,
           counterv = 1) {
    # Fixing the 'Time' column if necessary
    df_time <- time_col_name(df, name_only = T)
    
    # Obtaining list of dataframe column names
    list_of_names <- colnames(df_time)
    
    # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
    subset_timerange <-
      subset(df_time, (Time >= lower & Time <= upper))
    
    
    # Excluding time column from column names list
    coln_df <- list_of_names[list_of_names != "Time"]
    
    #Resulting dataframe
    result_df <- data.frame(Time = df_time$Time)
    # lag_data <- data.frame(A = character(), B = numeric())
    # colnames(lag_data) <- c('Cell_name', colnames(shifted_main_cell_values)[1])
    
    
    
    if (counterv > 20) {
      stop('Too many iterations! Increase the interval, maximum lag or delete invalid curves!')
    }
    
    for (cell in coln_df) {
      lag_for_max_acf <-
        lag_and_sign(subset_timerange[, cell], shifted_main_cell_values, max_lag)
      
      
      
      if (lag_for_max_acf < 0) {
        main_cell_number <-
          str_extract(cell, '\\D0*(\\d+)($|\\s)', group = 1)
        print(cell)
        print(paste0("Main cell number now is: ", main_cell_number))
        shifted_main_cell_values <-
          finding_shifted_curve(df, main_cell_number, lower, upper, max_lag)
        counterv = counterv + 1
        print(paste0('The counter variable is: ', counterv))
        res <-
          shifting_curves(df,
                          shifted_main_cell_values,
                          lower,
                          upper,
                          max_lag,
                          counterv)
        return(res)
        
      } else {
        shifted_cell_values <-
          df[, cell][(lag_for_max_acf + 1):nrow(df[, cell]), ]
        
        result_df <-
          as.data.frame(cbind.fill(result_df, shifted_cell_values))
      }
      
    }
    
    return(result_df)
  }

# The same function but for constructing dataframe that has information about iterations and maximum_lag


shifting_curves_info <-
  function(lag_data,
           df,
           shifted_main_cell_values,
           lower,
           upper,
           max_lag,
           counterv = 1) {
    # Fixing the 'Time' column if necessary
    df_time <- time_col_name(df, name_only = T)
    
    # Obtaining list of dataframe column names
    list_of_names <- colnames(df_time)
    
    # Subsetting the dataframe in accordance with the region of interest set by the operator (lower - START time, upper - STOP time)
    subset_timerange <-
      subset(df_time, (Time >= lower & Time <= upper))
    
    
    # Excluding time column from column names list
    coln_df <- list_of_names[list_of_names != "Time"]
    
    
    
    if (counterv > 20) {
      return(lag_data)
      stop('Too many iterations! Increase the interval, maximum lag or delete invalid curves!')
    }
    
    for (cell in coln_df) {
      lag_for_max_acf <-
        lag_and_sign(subset_timerange[, cell], shifted_main_cell_values, max_lag)
      # print(paste0('Lag for ', cell, ': ', lag_for_max_acf))
      df_to_merge <- data.frame(A = cell, B = lag_for_max_acf)
      
      colnames(df_to_merge) <-
        c('Cell_name', colnames(shifted_main_cell_values)[1])
      # print(df_to_merge)
      
      
      if (!(colnames(shifted_main_cell_values)[1] %in% colnames(lag_data)) &
          (nrow(lag_data) > 0)) {
        lag_data <- as.data.frame(bind_rows(lag_data, df_to_merge))
        # print(lag_data)
      } else if (counterv == 1) {
        lag_data <- as.data.frame(rbind(lag_data, df_to_merge))
        # print(lag_data)
      } else {
        lag_data <- as.data.frame(bind_rows(lag_data, df_to_merge))
        print(lag_data)
      }
      
      
      
      
      
      
      
      
      if (lag_for_max_acf < 0) {
        main_cell_number <-
          str_extract(cell, '\\D0*(\\d+)($|\\s)', group = 1)
        # print(paste0("Main info number now is: ", main_cell_number))
        shifted_main_cell_values <-
          finding_shifted_curve(df, main_cell_number, lower, upper, max_lag)
        counterv = counterv + 1
        # print(paste0('The info variable is: ', counterv))
        res <-
          shifting_curves_info(lag_data,
                               df,
                               shifted_main_cell_values,
                               lower,
                               upper,
                               max_lag,
                               counterv)
        return(res)
        
      } else {
        info_df <- lag_data
      }
      
      
    }
    
    return(info_df)
  }






# Rotating plots ----------------------------------------------------------




average_curve <- function(df_read) {
  # Fixing the 'Time' column if necessary
  df_rot <- time_col_name(df_read, name_only = T)
  
  if (length(grep("^([Aa]verage|[Mm]ean)", colnames(df_rot))) == 0) {
    df_rot <- df_rot %>%
      add_column(Average = rowMeans(df_rot[-grep('^Time$', colnames(df_rot))]))
    
    
    df_rot <-
      df_rot[, c(grep('^Time$', colnames(df_rot)),
                 grep("^([Aa]verage|[Mm]ean)", colnames(df_rot)))]
    
  } else {
    df_rot <- df_rot %>%
      select(c(
        grep('^Time$', colnames(df_rot)),
        grep("^([Aa]verage|[Mm]ean)", colnames(df_rot))
      ))
  }
  
  return(df_rot)
  
}


getting_a_slice_of_df <-
  function(df_to_slice,
           cell_number_or_name,
           c_name = FALSE) {
    time_col <- '([Tt]ime\\s?)'
    colnames(df_to_slice)[grepl(time_col, colnames(df_to_slice))] =  "Time"
    
    if (c_name == TRUE) {
      cell_name <- cell_number_or_name
    } else {
      cell_name <- finding_cell_name(df_to_slice, cell_number_or_name)
    }
    
    
    df_to_slice <- df_to_slice %>%
      select('Time', all_of(cell_name))
    
    return(df_to_slice)
    
  }


# rotate_all <- function(df_to_rotate, list_of_names, listn = FALSE, lower_t, upper_t, baseline_r = TRUE, shift_down = TRUE) {
#
#   if (listn == FALSE) {list_of_names <- c()}
#
#   for (id in nrows)
#
#
# }

rotating_plot <-
  function(df_to_rotate,
           lower_t,
           upper_t,
           part = FALSE,
           shift_down = FALSE) {
    if (ncol(df_to_rotate) != 2) {
      stop(print(
        "Something wrong with the data: no such cell number or they are repeats!"
      ))
      
    } else if (colnames(df_to_rotate)[1] != 'Time') {
      if (length(grep('([Tt]ime\\s?)', colnames(df_to_rotate))) != 1) {
        stop(print("Something wrong with the data: no Time column!"))
        
      } else if (grep('([Tt]ime\\s?)', colnames(df_to_rotate)) == 1) {
        colnames(df_to_rotate)[1] <- 'Time'
        
        
      } else if (grep('([Tt]ime\\s?)', colnames(df_to_rotate)) == 2) {
        colnames(df_to_rotate)[2] <- 'Time'
        df_to_rotate <- df_to_rotate[, c(2, 1)]
        
      } else {
        stop(print("Something wrong with the data: no Time column!"))
        
      }
      
      
    }
    
    
    
    initial_col_name <- colnames(df_to_rotate)[2]
    
    colnames(df_to_rotate)[2] <- 'Cell'
    
    
    
    df_1 <- subset(df_to_rotate, Time < lower_t)
    
    df_2 <- subset(df_to_rotate, (Time >= lower_t & Time <= upper_t))
    
    df_3 <- subset(df_to_rotate, Time > upper_t)
    
    
    # Rotating
    
    average_lm <- coef(lm(Cell ~ Time, data = df_2))
    
    # b = average_lm[[1]]
    k = average_lm[[2]]
    
    
    
    # Rotate partially or the whole plot?
    
    if (part == T) {
      df_2$Cell <- df_2$Cell - k * df_2$Time
      
      if (shift_down == TRUE) {
        df_2$Cell <- df_2$Cell + k * df_2$Time[length(df_2$Time)]
        
      }
      
      
      df_to_rotate <- rbind(df_1, df_2, df_3)
      
    } else {
      df_to_rotate$Cell <- df_to_rotate$Cell - k * df_to_rotate$Time
    }
    
    
    
    
    # Returning initial column name if differs
    
    colnames(df_to_rotate)[which(names(df_to_rotate) == 'Cell')] <-
      initial_col_name
    
    return(df_to_rotate)
    
    
  }



# For single plot replacing the column with new values

replace_columns_in_dfs <- function(df_full, df_part) {
  inter <-
    intersect(colnames(df_full), colnames(df_part)[!grepl('([Tt]ime\\s?)', colnames(df_part))])
  
  df_full[which(colnames(df_full) == inter)] <-
    df_part[!grepl('([Tt]ime\\s?)', colnames(df_part))]
  
  return(df_full)
  
}


# Miscellaneous -----------------------------------------------------------


# Opposite to intersect() function

outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}

# Compose function for lapply

Compose <- function(x, ...)
{
  lst <- list(...)
  for (i in rev(seq_along(lst)))
    x <- lst[[i]](x)
  x
}


# Borrowed function to add columns with different length to dataframe (empty values are NA)
cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(
      , n - nrow(x), ncol(x)
    ))))
}



# Rotation ----------------------------------------------------------------


# Rotating every single plot one by one

rotate_all <- function(df_to_rotate,
                       list_of_names,
                       lower_base,
                       upper_base,
                       lower_reg,
                       upper_reg,
                       baseline_r = TRUE,
                       shift_down = TRUE)
  
{
  if (length(list_of_names) == 0) {
    names_list <-
      colnames(df_to_rotate)[-grep('([Tt]ime\\s?)', colnames(df_to_rotate))]
    
  } else {
    names_list <- outersect(list_of_names,
                            (colnames(df_to_rotate)[-grep('([Tt]ime\\s?)', colnames(df_to_rotate))]))
    
  }
  
  
  for (name in names_list)
  {
    df2dim_single_and_rotated <-
      rotating_plot(
        getting_a_slice_of_df(df_to_rotate, name, c_name = T),
        lower_reg,
        upper_reg,
        part = FALSE
      )
    
    if (baseline_r == TRUE) {
      df2dim_single_and_rotated_part <-
        rotating_plot(df2dim_single_and_rotated,
                      lower_base,
                      upper_base,
                      part = TRUE,
                      shift_down)
      
      df_to_rotate <-
        replace_columns_in_dfs(df_to_rotate, df2dim_single_and_rotated_part)
      
    } else {
      df_to_rotate <-
        replace_columns_in_dfs(df_to_rotate, df2dim_single_and_rotated)
      
    }
  }
  
  return(df_to_rotate)
  
}


# Function to find b coefficient in linear regression ---------------------


b_find <- function(data_frame_input, b_min, b_max) {
  data_frame_input <- time_col_name(data_frame_input, name_only = T)
  
  
  subsetted <-
    subset(data_frame_input, (Time <= b_max & Time >= b_min))
  
  b = mean(subsetted[[2]])
  
  return(b)
  
}


# Filling space on plot with polygon  -------------------------------------

find_intersection_time <- function(y, x1, y1, x2, y2) {
  x <- x1 + (y - y1) * (x2 - x1) / (y2 - y1)
  
  return(x)
  
}



dataframe_with_intersections <-
  function(timeframe, r_min, r_max, b) {
    timeframe <- time_col_name(timeframe, name_only = T)
    
    init_name <- colnames(timeframe)[2]
    colnames(timeframe)[2] <- 'Current'
    
    
    df_part <- subset(timeframe, (Time <= r_max & Time >= r_min))
    
    for (id in 2:nrow(df_part)) {
      if ((df_part[id,]$Current > b) & (df_part[id - 1,]$Current < b) |
          (df_part[id,]$Current < b) &
          (df_part[id - 1,]$Current > b)) {
        time <- find_intersection_time(b,
                                       df_part[id,]$Time,
                                       df_part[id,]$Current,
                                       df_part[id - 1,]$Time,
                                       df_part[id - 1,]$Current)
        
        df_find0 <-
          timeframe[timeframe$Time <= df_part[id - 1,]$Time,]
        # df_find0$Time <- as.numeric(df_find0$Time)
        
        df_find <- c("Time" = as.numeric(time), "Current" = b)
        # df_find$Time <- as.numeric(df_find$Time)
        
        df_find2 <- timeframe[timeframe$Time >= df_part[id,]$Time,]
        
        timeframe <- rbind(df_find0, df_find, df_find2)
        # timeframe$Time <- timeframe(df_find0$Time)
      }
      
      
    }
    
    
    colnames(timeframe)[2] <- init_name
    
    
    return(timeframe)
    
  }


polygon_function <- function(df_rotated, r_min, r_max, averb) {
  df_rotated <- time_col_name(df_rotated, name_only = T)
  
  
  init_name <- colnames(df_rotated)[2]
  colnames(df_rotated)[2] <- 'Current'
  
  df_rotated2 <- df_rotated
  
  df_rotated2$Current[(df_rotated2$Time > r_min &
                         df_rotated2$Time < r_max &
                         df_rotated2$Current < averb)] <- averb
  
  
  if (df_rotated$Current[df_rotated$Time == r_min] > averb) {
    df_rotated2$Current[(df_rotated2$Time < r_min)] <- averb
    
    
  } else {
    df_rotated2$Current[(df_rotated2$Time <= r_min)] <- averb
  }
  
  df_rotated2$Time[df_rotated2$Time < r_min] <- r_min
  
  
  if (df_rotated$Current[df_rotated$Time == r_max] > averb) {
    df_rotated2$Current[(df_rotated2$Time > r_max)] <- averb
    
    
  } else {
    df_rotated2$Current[(df_rotated2$Time >= r_max)] <- averb
  }
  
  df_rotated2$Time[df_rotated2$Time > r_max] <- r_max
  
  
  return(df_rotated2)
}
