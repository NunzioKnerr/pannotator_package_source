#' annotation_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annotation_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "pannotator-annotation-table-shell",
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-end; gap: 12px; margin-bottom: 10px;",
        div(
          style = "flex: 1;",
          checkboxInput(
            inputId = ns("current_image_only"),
            label = "Show only the current image",
            value = FALSE
          )
        )
      ),
      uiOutput(ns("annotation_table_notice")),
      uiOutput(ns("annotation_table_status")),
      uiOutput(ns("annotation_table_container"))
    )
  )
}


editable_annotation_table_columns <- function(config = myEnv$config) {
  editable_columns <- "dd1"

  if (isTRUE(config$lookup2Enabled)) {
    editable_columns <- c(editable_columns, "dd2")
  }

  if (isTRUE(config$lookup3Enabled)) {
    editable_columns <- c(editable_columns, "dd3")
  }

  if (isTRUE(config$lookup4Enabled)) {
    editable_columns <- c(editable_columns, "dd4")
  }

  editable_columns
}


annotation_table_column_titles <- function(config = myEnv$config) {
  column_titles <- c(
    user = "User",
    id = "Annotation ID",
    sourcekmz = "Source KMZ",
    imagefile = "Image",
    feature_type = "Feature Type",
    radius = "Radius",
    geometry = "Geometry",
    dd1 = config$lookup1Label
  )

  if (isTRUE(config$lookup2Enabled)) {
    column_titles <- c(column_titles, dd2 = config$lookup2Label)
  }

  if (isTRUE(config$lookup3Enabled)) {
    column_titles <- c(column_titles, dd3 = config$lookup3Label)
  }

  if (isTRUE(config$lookup4Enabled)) {
    column_titles <- c(column_titles, dd4 = config$lookup4Label)
  }

  column_titles
}


annotation_table_lookup_choices <- function(
    column_name,
    lookup_choices = list(
      dd1 = myEnv$var_dropdown1,
      dd2 = myEnv$var_dropdown2,
      dd3 = myEnv$var_dropdown3,
      dd4 = myEnv$var_dropdown4
    )
) {
  lookup_values <- switch(
    column_name,
    dd1 = lookup_choices$dd1,
    dd2 = lookup_choices$dd2,
    dd3 = lookup_choices$dd3,
    dd4 = lookup_choices$dd4,
    NULL
  )

  if (is.null(lookup_values)) {
    return(character(0))
  }

  lookup_values <- as.character(unlist(lookup_values, use.names = FALSE))
  lookup_values <- lookup_values[!is.na(lookup_values) & nzchar(lookup_values)]
  unique(lookup_values)
}


annotation_table_id_sort_key <- function(annotation_ids) {
  id_text <- trimws(as.character(annotation_ids))
  digit_key <- gsub("[^0-9]", "", id_text)
  digit_key[!nzchar(digit_key)] <- NA_character_

  if (all(is.na(digit_key))) {
    return(rep(NA_character_, length(id_text)))
  }

  key_width <- max(nchar(digit_key[!is.na(digit_key)]))
  sort_key <- rep(NA_character_, length(id_text))
  has_digits <- !is.na(digit_key)
  sort_key[has_digits] <- paste0(
    strrep("0", pmax(0L, key_width - nchar(digit_key[has_digits]))),
    digit_key[has_digits]
  )
  sort_key
}


sort_annotation_table_by_id <- function(table_data) {
  if (!"id" %in% names(table_data) || nrow(table_data) == 0) {
    return(table_data)
  }

  sort_key <- annotation_table_id_sort_key(table_data$id)
  sortable_rows <- which(!is.na(sort_key))
  unsortable_rows <- which(is.na(sort_key))

  sorted_rows <- sortable_rows[
    order(sort_key[sortable_rows], decreasing = TRUE, method = "radix")
  ]

  table_data[c(sorted_rows, unsortable_rows), , drop = FALSE]
}


annotation_table_view_data <- function(annotation_data,
                                       config = myEnv$config,
                                       current_image = NULL,
                                       current_kmz_name = NULL,
                                       current_image_only = FALSE) {
  if (is.null(annotation_data)) {
    annotation_data <- create_user_dataframe()
  }

  table_data <- normalize_annotation_dataframe(annotation_data)

  if (isTRUE(current_image_only) && !is.null(current_image) && nzchar(current_image)) {
    table_data <- table_data[table_data$imagefile == current_image, , drop = FALSE]

    if (!is.null(current_kmz_name) && nzchar(current_kmz_name) && "sourcekmz" %in% names(table_data)) {
      source_values <- trimws(as.character(table_data$sourcekmz))
      table_data <- table_data[source_values == current_kmz_name | !nzchar(source_values), , drop = FALSE]
    }
  }

  keep_columns <- c(
    "user",
    "id",
    "sourcekmz",
    "imagefile",
    "feature_type",
    "radius",
    "geometry",
    editable_annotation_table_columns(config)
  )
  keep_columns <- keep_columns[keep_columns %in% names(table_data)]

  table_data <- table_data[, keep_columns, drop = FALSE]

  if ("id" %in% names(table_data)) {
    table_data$id <- as.character(table_data$id)
  }

  sort_annotation_table_by_id(table_data)
}


apply_annotation_table_edits <- function(existing_data,
                                         edited_rows,
                                         config = myEnv$config) {
  updated_data <- as.data.frame(existing_data, stringsAsFactors = FALSE)
  edited_rows <- as.data.frame(edited_rows, stringsAsFactors = FALSE)

  if (nrow(updated_data) == 0 || nrow(edited_rows) == 0 || !"id" %in% names(edited_rows)) {
    return(updated_data)
  }

  updated_data$id <- as.character(updated_data$id)
  edited_rows$id <- as.character(edited_rows$id)
  editable_columns <- intersect(editable_annotation_table_columns(config), names(edited_rows))

  for (row_index in seq_len(nrow(edited_rows))) {
    annotation_id <- edited_rows$id[[row_index]]
    matched_row <- match(annotation_id, updated_data$id)

    if (is.na(matched_row)) {
      next
    }

    for (column_name in editable_columns) {
      updated_data[matched_row, column_name] <- annotation_value_or_blank(edited_rows[row_index, column_name])
    }
  }

  updated_data
}


sync_annotation_cards_from_table_edits <- function(r, edited_rows, config = r$config) {
  if (nrow(edited_rows) == 0 || !"id" %in% names(edited_rows)) {
    return(invisible(NULL))
  }

  editable_columns <- intersect(editable_annotation_table_columns(config = config), names(edited_rows))

  for (row_index in seq_len(nrow(edited_rows))) {
    annotation_id <- as.character(edited_rows$id[[row_index]])

    if (is.null(r$annotation_cards[[annotation_id]])) {
      next
    }

    for (column_name in editable_columns) {
      update_annotation_card_field(
        r = r,
        myId = annotation_id,
        field = column_name,
        value = annotation_value_or_blank(edited_rows[row_index, column_name])
      )
    }
  }

  invisible(NULL)
}


build_annotation_table_hot <- function(
    table_data,
    config = myEnv$config,
    lookup_choices = list(
      dd1 = myEnv$var_dropdown1,
      dd2 = myEnv$var_dropdown2,
      dd3 = myEnv$var_dropdown3,
      dd4 = myEnv$var_dropdown4
    ),
    height = 340
) {
  column_titles <- annotation_table_column_titles(config = config)
  editable_columns <- editable_annotation_table_columns(config = config)

  hot <- rhandsontable::rhandsontable(
    table_data,
    stretchH = "all",
    rowHeaders = NULL,
    width = "100%",
    height = height
  )
  hot <- rhandsontable::hot_table(
    hot,
    highlightCol = TRUE,
    highlightRow = TRUE,
    contextMenu = FALSE,
    filters = TRUE
  )
  hot <- rhandsontable::hot_cols(
    hot,
    columnSorting = TRUE
  )

  for (column_name in names(table_data)) {
    column_title <- column_titles[[column_name]]

    if (column_name %in% editable_columns) {
      dropdown_choices <- annotation_table_lookup_choices(
        column_name,
        lookup_choices = lookup_choices
      )

      if (length(dropdown_choices) > 0) {
        hot <- rhandsontable::hot_col(
          hot,
          column_name,
          title = column_title,
          type = "dropdown",
          source = dropdown_choices,
          strict = FALSE,
          allowInvalid = FALSE
        )
      } else {
        hot <- rhandsontable::hot_col(
          hot,
          column_name,
          title = column_title,
          type = "text"
        )
      }
    } else {
      hot <- rhandsontable::hot_col(
        hot,
        column_name,
        title = column_title,
        readOnly = TRUE
      )
    }
  }

  hot
}


#' annotation_table Server Functions
#'
#' @noRd
mod_annotation_table_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    output$annotation_table_notice <- renderUI({
      notice <- r$annotation_table_notice
      if (is.null(notice)) {
        return(NULL)
      }

      panel_notice(
        title = notice$title,
        message = notice$message,
        type = notice$type
      )
    })

    filtered_table_data <- reactive({
      annotation_table_view_data(
        annotation_data = r$user_annotations_data,
        config = r$config,
        current_image = r$current_image,
        current_kmz_name = r$current_kmz_name,
        current_image_only = isTRUE(input$current_image_only)
      )
    })

    output$annotation_table_status <- renderUI({
      if (is.null(r$user_name) || !nzchar(r$user_name)) {
        return(panel_status_message(
          "Select a user name in the Annotation Panel to load the all-annotations table."
        ))
      }

      if (!requireNamespace("rhandsontable", quietly = TRUE)) {
        return(panel_status_message(
          "Install the 'rhandsontable' package to enable this editable annotation table."
        ))
      }

      if (isTRUE(input$current_image_only) && (is.null(r$current_image) || !nzchar(r$current_image))) {
        return(panel_status_message(
          "Choose an image first, or disable the current-image filter to see all annotations."
        ))
      }

      if (nrow(filtered_table_data()) == 0) {
        return(panel_status_message(
          "Annotation records will appear here as you create them."
        ))
      }

      NULL
    })

    output$annotation_table_container <- renderUI({
      if (is.null(r$user_name) || !nzchar(r$user_name)) {
        return(panel_empty_state(
          title = "Annotation Table",
          message = "Select a user name and start annotating to populate the table."
        ))
      }

      if (!requireNamespace("rhandsontable", quietly = TRUE)) {
        return(panel_empty_state(
          title = "rhandsontable Not Installed",
          message = "Install the 'rhandsontable' package in the same R library used by this app to enable the editable annotation table panel."
        ))
      }

      if (isTRUE(input$current_image_only) && (is.null(r$current_image) || !nzchar(r$current_image))) {
        return(panel_empty_state(
          title = "Current Image Filter Enabled",
          message = "Choose an image in the Image Panel or disable the filter to show all annotations."
        ))
      }

      if (nrow(filtered_table_data()) == 0) {
        return(panel_empty_state(
          title = "No Annotation Records",
          message = "Annotations created from the map, image viewer, or annotation panel will appear here."
        ))
      }

      div(
        class = "pannotator-annotation-table-widget",
        rhandsontable::rHandsontableOutput(
          session$ns("annotation_table"),
          width = "100%"
        )
      )
    })

    if (requireNamespace("rhandsontable", quietly = TRUE)) {
      output$annotation_table <- rhandsontable::renderRHandsontable({
        table_data <- filtered_table_data()
        req(nrow(table_data) > 0)

        build_annotation_table_hot(
          table_data,
          config = r$config,
          lookup_choices = list(
            dd1 = r$var_dropdown1,
            dd2 = r$var_dropdown2,
            dd3 = r$var_dropdown3,
            dd4 = r$var_dropdown4
          )
        )
      })
    }

    observeEvent(input$annotation_table, ignoreInit = TRUE, {
      req(requireNamespace("rhandsontable", quietly = TRUE))
      req(r$user_annotations_file_name, r$user_annotations_data)

      edited_rows <- rhandsontable::hot_to_r(input$annotation_table)
      req(edited_rows)

      updated_data <- apply_annotation_table_edits(
        existing_data = r$user_annotations_data,
        edited_rows = edited_rows
      )

      if (!identical(updated_data, r$user_annotations_data)) {
        r$user_annotations_data <- updated_data
        sync_annotation_cards_from_table_edits(r, edited_rows, config = r$config)
        save_annotations(
          myAnnotations = r$user_annotations_data,
          myAnnotationFileName = r$user_annotations_file_name
        )
        r$annotation_table_notice <- list(
          title = "Table Changes Saved",
          message = "Annotation lookup values edited in the table were saved.",
          type = "success"
        )
      }
    })

    list(
      filtered_rows = filtered_table_data
    )
  })
}
