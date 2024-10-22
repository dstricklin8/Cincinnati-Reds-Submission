library(tidyverse)
library(gt)
library(gtExtras)
library(shiny)
library(shinythemes)
library(bslib)

# Seed
set.seed(6432)

# Load Data
load("app_files/files.rda")

gt_color_pills <- function(gt_object, columns, palette = c("#C84630", "#5DA271"),
                           fill_type = "continuous", rank_order = "desc",
                           digits = NULL, domain = NULL, format_type = "number",
                           scale_percent = TRUE, suffix = "", reverse = FALSE,
                           outline_color = NULL, outline_width = 0.25,
                           pal_type = "discrete", pill_height = 25, ...) {
  
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% class(gt_object))
  
  data <- gt_object[["_data"]]
  
  column_data <- data[[rlang::as_string(rlang::ensym(columns))]]
  
  if (fill_type == "rank") {
    ranked_column <- rank(column_data, na.last = "keep", ties.method = "average")
    
    if (rank_order == "desc") {
      ranked_column <- max(ranked_column, na.rm = TRUE) - ranked_column + 1
    }
  } else {
    ranked_column <- column_data
  }
  
  if (is.null(domain)) {
    domain <- range(ranked_column, na.rm = TRUE)
    warning("Domain not specified, defaulting to observed range within the specified column.", call. = FALSE)
  }
  
  pal <- if (grepl(x = palette[1], pattern = "::")) {
    paletteer::paletteer_d(palette = palette, direction = if (reverse) -1 else 1, type = pal_type) %>% as.character()
  } else {
    if (reverse) rev(palette) else palette
  }
  
  format_value <- function(value, digits, format_type) {
    if (format_type == "percent" && scale_percent) {
      value <- value * 100
    }
    
    if (!is.null(digits)) {
      value <- round(value, digits)
    }
    
    formatted_value <- switch(format_type,
                              "number" = formatC(value, format = "f", digits = digits),
                              "comma" = formatC(value, format = "f", big.mark = ",", digits = digits),
                              "currency" = paste0("$", formatC(value, format = "f", big.mark = ",", digits = digits)),
                              "percent" = paste0(formatC(value, format = "f", digits = digits), "%"),
                              as.character(value))
    
    return(paste0(formatted_value, suffix))
  }
  
  formatted_values <- sapply(column_data, function(x) format_value(x, digits, format_type))
  max_width <- max(nchar(formatted_values))
  
  generate_pill_html <- function(value, rank_value) {
    color <- scales::col_numeric(palette = pal, domain = domain)(rank_value)
    text_color <- gt:::ideal_fgnd_color(color)
    formatted_value <- format_value(as.numeric(value), digits, format_type)
    
    outline_style <- if (!is.null(outline_color)) glue::glue("border: {outline_width}px solid {outline_color};") else ""
    
    glue::glue("<span style='display: inline-block; width: {max_width}ch; padding-left: 3px; padding-right: 3px; height: {pill_height}px; line-height: {pill_height}px; background-color: {color}; color: {text_color}; border-radius: 10px; text-align: center; {outline_style}'>{formatted_value}</span>")
  }
  
  gt_object %>%
    text_transform(
      locations = cells_body(columns = {{ columns }}),
      fn = function(x) {
        mapply(generate_pill_html, x, ranked_column)
      }
    )
}

gt_theme_savant <- function(gt_object, ...) {
  
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))
  
  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]
  
  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }
  
  gt_object %>%
    # cell body
    gt::tab_style(locations = gt::cells_body(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # col. headers
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # group rows
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Roboto Condensed"),
          weight = 650,
          size = px(14),
          color = "#FFFDF5"
        ),
        gt::cell_fill(
          color = "#000000"
        )
      )
    ) %>%
    # footnote
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(12))) %>%
    # title
    gt::tab_style(locations = gt::cells_title('title'),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Roboto Condensed'), size = px(18))) %>%
    # subtitle
    gt::tab_style(locations = gt::cells_title('subtitle'),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # caption
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(12))) %>%
    # spanner
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Roboto Condensed"),
        weight = 650,
        size = px(8)
      )
    ) %>%
    gt::tab_options(
      data_row.padding = 1,
      table_body.hlines.color = "transparent",
      column_labels.border.top.color = 'black',
      column_labels.border.top.width = px(1),
      column_labels.border.bottom.style = 'none',
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = 'solid',
      row_group.padding = px(1.5),
      heading.align = 'center',
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "white",
      table.border.bottom.style = 'none',
      table.border.top.style = 'none',
      source_notes.border.lr.style = "none",
      ...
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_css(c(paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
                  paste0("#", table_id, " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"),
                  paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
                  paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
                  paste0("#", table_id, " .gt_column_spanner {font-size: 12px; font-weight: bold; text-decoration: underline;}")))
  
}




# UI ----
ui <- page_sidebar(
  title = "Pitch Mix Report",
  sidebar = sidebar(open = "always",
                    helpText("Select a Hitter to analyze his Pitch Mix"),
                    sidebarPanel(width = 12, uiOutput("hitter_Select")
                    )
                    ),
  
  mainPanel(width = 12,
            layout_columns(col_widths = c(8, 4, 12), 
                           row_heights = c(2, 1),
                           card(
                             full_screen = TRUE,
                             card_header("Proportion Distributions"),
                             plotOutput("pitch_locs")
                           ),
                           card(
                             full_screen = TRUE,
                             card_header("Pitch Percentages"),
                             gt_output("future_preds")
                           ),
                           card(
                             full_screen = TRUE,
                             card_header("Stat Overview"),
                             gt_output("summary_gt")
                           )
            )
  )
)

# Server ----
server <- function(input, output) {
  
  output$hitter_Select <- renderUI({
    selectInput("hitter_var", "Select Hitter",
                choices = sort(unique(player_info$player_name)))
  })
  
  output$pitch_locs <- renderPlot({
    
    df %>% 
      filter(player_name %in% input$hitter_var) %>% 
      ggplot(aes(game_year, prop, group = pitch_group, fill = pitch_group)) +
      geom_col(position = "dodge", color = "black") +
      geom_line(position = position_dodge(width = 0.9)) +
      geom_point(position = position_dodge(width = 0.9), shape = 21) +
      scale_fill_manual(values = pitch_group_colors) +
      theme_minimal() +
      labs(x = "Year", fill = "", y = "Proportion of Pitches") +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill = "#582c83"),
            strip.text = element_text(colour = "white", face = "bold"))
    
    
  })
  
  output$future_preds <- render_gt({
    
    # Get Proportions ----
    t1 <- table_df %>% 
      filter(player_name %in% input$hitter_var) %>% 
      select(-c(batter_id, player_name)) %>% 
      ungroup() %>% 
      gt() %>% 
      gt_theme_savant() %>% 
      cols_label(
        game_year = md("Year"),
        num_pitches = md("\\#")
      ) %>% 
      fmt_number(num_pitches, decimals = 0) %>% 
      fmt_percent(c(OS, BB, FB), decimals = 0) %>% 
      tab_style(
        style = cell_text(align = "center"), locations = cells_column_labels(columns = everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"), 
        locations = cells_body(columns = everything())
      ) %>% 
      tab_header(title = "Past and Future Proportions") %>% 
      tab_options(
        table.font.size = 12, data_row.padding = 24, 
        table.width = pct(100),
        table.font.color = "#00001a")
    
    t1
    
    # # Find Similarity
    # # Target personal_id
    # hitter_id <- table_df %>%
    #   filter(player_name %in% input$hitter_var) %>%
    #   distinct(batter_id) %>%
    #   pull(batter_id)
    # 
    # # Filter out the target's row
    # target_data <- table_df %>%
    #   filter(batter_id == hitter_id,
    #          game_year == 2024) %>%
    #   select(OS, BB, FB)
    # 
    # # Compute Euclidean distances
    # dist_df <- table_df %>%
    #   filter(game_year == 2024, num_pitches >= 1000) %>%
    #   mutate(
    #     distance = sqrt((OS - target_data$OS)^2 + (BB - target_data$BB)^2 +(FB - target_data$FB)^2)
    #   ) %>%
    #   ungroup()
    # 
    # # Remove the target personal_id from the results
    # table2 <- dist_df %>%
    #   filter(batter_id != hitter_id)
    # 
    # # Find the personal_id with the smallest distance
    # most_similar_id <- table2 %>%
    #   slice_min(order_by = distance, n = 3) %>%
    #   pull(player_name)
    # 
    # least_similar_id <- table2 %>%
    #   slice_max(order_by = distance, n = 3) %>%
    #   pull(player_name)
    # 
    # most_sim <- tibble(
    #   player_1 = most_similar_id[1],
    #   player_2 = most_similar_id[2],
    #   player_3 = most_similar_id[3]
    # ) %>%
    #   mutate(
    #     value = "Most Similar"
    #   )
    # 
    # least_sim <- tibble(
    #   player_1 = least_similar_id[1],
    #   player_2 = least_similar_id[2],
    #   player_3 = least_similar_id[3]
    # ) %>%
    #   mutate(
    #     value = "Least Similar"
    #   )
    # 
    # # Output the result
    # t2 <- most_sim %>%
    #   bind_rows(least_sim) %>%
    #   select(value, everything()) %>%
    #   ungroup() %>% 
    #   gt() %>%
    #   gt_theme_savant() %>%
    #   cols_label(
    #     value = "",
    #     player_1 = md("Player 1"),
    #     player_2 = md("Player 2"),
    #     player_3 = md("Player 3"),
    #   ) %>%
    #   tab_footnote(footnote = "Similarity determined by Max/Min Euclidean Distance") %>%
    #   tab_header(title = "Players Most and least Similar To") %>%
    #   tab_style(
    #     style = cell_text(align = "center"), locations = cells_column_labels(columns = everything())
    #   ) %>%
    #   tab_style(
    #     style = cell_text(align = "center"),
    #     locations = cells_body(columns = everything())
    #   ) %>%
    #   tab_options(
    #     table.font.size = 12, data_row.padding = 12,
    #     table.width = pct(100),
    #     table.font.color = "#00001a")
    # 
    # t2

    # gt_stack_tables(list(t1, t2))
    
  })
  
  output$summary_gt <- render_gt({
    
    pitch_summary_overall %>% 
      left_join(pitch_group_tibble) %>% 
      filter(player_name %in% input$hitter_var) %>% 
      select(-c(batter_id, player_name, sz_top, sz_bot)) %>% 
      arrange(-total_pitches) %>% 
      mutate(
        prop = 100 * prop
      ) %>% 
      select(pitch_group, prop, everything(),
             -c(singles, doubles, triples, homeruns, ks, walks)) %>% 
      gt() %>% 
      gt_theme_savant() %>%
      
      gt_color_pills(prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(swing_prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(chase_prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = T) %>% 
      
      gt_color_pills(zone_swing_prop, digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(whiff_prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = T) %>% 
      
      gt_color_pills(putaway_prop, digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = T) %>% 
      
      gt_color_pills(sweetspot_prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(hardhit_prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(barrel_prop,  digits = 1, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(woba,  digits = 3, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(xwoba,  digits = 3, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_color_pills(exit_velo,  digits = 1, fill_type = "rank", format = "number",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = T) %>% 
      
      gt_color_pills(exit_angle,  digits = 1, fill_type = "rank", format = "number",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = T) %>% 
      
      gt_color_pills(win_prob_added,  digits = 3, suffix = "  ",
                     palette = c("dodgerblue2", "grey90", "indianred3"), reverse = F) %>% 
      
      gt_add_divider(columns = c(pitch_group), style = "solid", include_labels = F) %>%
      cols_hide(columns = c(pitch_hex, total_pitches, total_bases, exit_velo, exit_angle)) %>% 
      cols_label(
        pitch_group = md("Pitch"),
        prop = md("\\%"),
        swing_prop = md("Swing"),
        chase_prop = md("Chase"),
        zone_swing_prop = md("Z-Swing%"),
        whiff_prop = md("Whiff%"),
        putaway_prop = md("PutAway%"),
        woba = md("wOBA"),
        xwoba = md("xwOBA"),
        exit_velo = md("EV"),
        exit_angle = md("LA"),
        sweetspot_prop = md("SweetSpot%"),
        hardhit_prop = md("HardHit%"),
        barrel_prop = md("Barrel%"),
        win_prob_added = md("WPA")
      ) %>% 
      tab_style(
        style = cell_text(align = "center"), locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = list(
          cell_text(color = from_column("pitch_hex"), align = "center", weight = "bold")),
        locations = cells_body(columns = pitch_group)
      ) %>% 
      # cols_width(everything() ~ pct(40)) %>%
      tab_options(
                  table.font.size = 16, data_row.padding = 4, 
                  # data_row.padding.horizontal = 40,
                  table.width = pct(100),
                  table.font.color = "#00001a")
    
    
  })
  
}

# Deploy App ----
shinyApp(ui = ui, server = server)

