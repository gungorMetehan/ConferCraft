# ============================================================
# CONFERCRAFT - CERTIFICATE GENERATOR
# Creator: Metehan Gungor
#
# Expected folder structure:
# apps/
#   certificate-generator/
#     app.R
#     templates/
#       whisper_current.svg
#       classic_flourish.svg
#       baroque_scroll.svg
#       deco_grid.svg
#       hellenic_key.svg
#       laureate_crest.svg
#       fine_line.svg
#       botanical_filigree.svg
#
# Excel structure:
#   Column 1: Full Name (required)
#   Column 2: Title (optional)
#
# Placeholders:
#   {name}  -> first Excel column
#   {title} -> second Excel column, when present
#
# Layout:
#   Logos: up to two, independently positioned in six top/bottom slots
#   Signatures: up to two, independently positioned in three bottom slots
#
# Optional navigation:
# Set CONFERCRAFT_ACCEPTANCE_URL in the deployment environment to the
# deployed Acceptance Letter Generator URL.
# ============================================================

library(shiny)
library(readxl)
library(zip)
library(grid)
library(png)
library(jpeg)
library(sysfonts)
library(showtext)
library(rsvg)

showtext::showtext_auto(enable = TRUE)

# ============================================================
# APP CONFIGURATION
# ============================================================

ACCEPTANCE_LETTER_URL <- Sys.getenv("CONFERCRAFT_ACCEPTANCE_URL", unset = "")
CONFERCRAFT_HOME_URL <- Sys.getenv("CONFERCRAFT_HOME_URL", unset = "https://gungormetehan-confercraft.share.connect.posit.cloud/")

OPEN_FONT_CHOICES <- c(
  "Alegreya" = "Alegreya",
  "Alegreya Sans" = "Alegreya Sans",
  "Bitter" = "Bitter",
  "Cormorant Garamond" = "Cormorant Garamond",
  "Crimson Pro" = "Crimson Pro",
  "EB Garamond" = "EB Garamond",
  "Fira Sans" = "Fira Sans",
  "IBM Plex Sans" = "IBM Plex Sans",
  "IBM Plex Serif" = "IBM Plex Serif",
  "Inter" = "Inter",
  "Lato" = "Lato",
  "Libre Baskerville" = "Libre Baskerville",
  "Libre Franklin" = "Libre Franklin",
  "Lora" = "Lora",
  "Merriweather" = "Merriweather",
  "Merriweather Sans" = "Merriweather Sans",
  "Montserrat" = "Montserrat",
  "Noto Sans" = "Noto Sans",
  "Noto Serif" = "Noto Serif",
  "Open Sans" = "Open Sans",
  "Playfair Display" = "Playfair Display",
  "Roboto" = "Roboto",
  "Roboto Slab" = "Roboto Slab",
  "Source Sans 3" = "Source Sans 3",
  "Source Serif 4" = "Source Serif 4",
  "Spectral" = "Spectral",
  "Ubuntu" = "Ubuntu",
  "Vollkorn" = "Vollkorn",
  "Work Sans" = "Work Sans"
)

OPEN_SERIF_FONTS <- c(
  "Alegreya", "Bitter", "Cormorant Garamond", "Crimson Pro",
  "EB Garamond", "IBM Plex Serif", "Libre Baskerville", "Lora",
  "Merriweather", "Noto Serif", "Playfair Display", "Roboto Slab",
  "Source Serif 4", "Spectral", "Vollkorn"
)

LOGO_POSITION_CHOICES <- c(
  "Top Left" = "top_left",
  "Top Center" = "top_center",
  "Top Right" = "top_right",
  "Bottom Left" = "bottom_left",
  "Bottom Center" = "bottom_center",
  "Bottom Right" = "bottom_right"
)

SIGNATURE_POSITION_CHOICES <- c(
  "Bottom Left" = "bottom_left",
  "Bottom Center" = "bottom_center",
  "Bottom Right" = "bottom_right"
)

CERTIFICATE_TEMPLATE_CHOICES <- c(
  "Whisper Current" = "whisper_current",
  "Classic Flourish" = "classic_flourish",
  "Baroque Scroll" = "baroque_scroll",
  "Deco Grid" = "deco_grid",
  "Hellenic Key" = "hellenic_key",
  "Laureate Crest" = "laureate_crest",
  "Fine Line" = "fine_line",
  "Botanical Filigree" = "botanical_filigree"
)

CERTIFICATE_TEMPLATE_IDS <- unname(CERTIFICATE_TEMPLATE_CHOICES)

.confercraft_font_cache <- new.env(parent = emptyenv())

ensure_open_font <- function(font_family) {
  font_family <- as.character(font_family)[1]

  if (!font_family %in% unname(OPEN_FONT_CHOICES)) {
    font_family <- "IBM Plex Sans"
  }

  if (exists(font_family, envir = .confercraft_font_cache, inherits = FALSE)) {
    return(get(font_family, envir = .confercraft_font_cache, inherits = FALSE))
  }

  loaded_families <- tryCatch(
    sysfonts::font_families(),
    error = function(error) character(0)
  )

  dependencies_ready <-
    requireNamespace("curl", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)

  if (!font_family %in% loaded_families && dependencies_ready) {
    tryCatch(
      sysfonts::font_add_google(
        name = font_family,
        family = font_family,
        regular.wt = 400,
        bold.wt = 700,
        repo = "https://fonts.gstatic.com/"
      ),
      error = function(error) NULL
    )
  }

  loaded_families <- tryCatch(
    sysfonts::font_families(),
    error = function(error) character(0)
  )

  resolved_family <- if (font_family %in% loaded_families) {
    font_family
  } else if (font_family %in% OPEN_SERIF_FONTS) {
    "serif"
  } else {
    "sans"
  }

  assign(font_family, resolved_family, envir = .confercraft_font_cache)
  resolved_family
}

# ============================================================
# GENERAL HELPERS
# ============================================================

has_text <- function(text) {
  !is.null(text) && length(text) == 1 && !is.na(text) && nzchar(trimws(text))
}

has_file <- function(path) {
  !is.null(path) && length(path) == 1 && !is.na(path) && nzchar(path) && file.exists(path)
}

is_valid_hex <- function(color) {
  color <- trimws(color)
  grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", color)
}

normalize_hex <- function(color) {
  color <- toupper(trimws(color))

  if (!is_valid_hex(color)) {
    stop(paste(color, "is not a valid HEX color."))
  }

  if (nchar(color) == 4) {
    chars <- strsplit(substring(color, 2), "")[[1]]
    color <- paste0("#", paste0(chars, chars, collapse = ""))
  }

  color
}

safe_filename <- function(text) {
  text <- trimws(as.character(text))
  text <- gsub("[\\\\/:*?\"<>|]", "_", text)
  text <- gsub("[[:cntrl:]]", "", text)
  text <- gsub("\\s+", "_", text)

  if (is.na(text) || nchar(text) == 0) {
    text <- "participant"
  }

  text
}

contains_placeholder <- function(text, placeholder) {
  has_text(text) && grepl(placeholder, text, fixed = TRUE)
}

fill_placeholders <- function(text, participant_name, participant_title) {
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return("")
  }

  text <- gsub("{name}", participant_name, text, fixed = TRUE)
  text <- gsub("{title}", participant_title, text, fixed = TRUE)
  text
}

fill_placeholders_styled <- function(text, participant_name, participant_title) {
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return("")
  }

  # {name} receives its own internal marker so its font size can be
  # controlled independently from the surrounding certificate text.
  text <- gsub(
    "{name}",
    paste0("[[NAME]]", participant_name, "[[/NAME]]"),
    text,
    fixed = TRUE
  )

  # {title} keeps the existing bold-placeholder behavior.
  text <- gsub(
    "{title}",
    paste0("[[B]]", participant_title, "[[/B]]"),
    text,
    fixed = TRUE
  )

  text
}

uploaded_file_fingerprint <- function(file_input) {
  if (is.null(file_input)) {
    return("")
  }

  paste(
    file_input$name,
    file_input$size,
    file_input$type,
    file_input$datapath,
    sep = "|"
  )
}

read_raster_image <- function(image_path) {
  extension <- tolower(tools::file_ext(image_path))

  if (extension == "png") {
    return(png::readPNG(image_path))
  }

  if (extension %in% c("jpg", "jpeg")) {
    return(jpeg::readJPEG(image_path))
  }

  stop("Unsupported image format. Please use PNG, JPG, or JPEG.")
}

# ============================================================
# SVG TEMPLATES
# Eight landscape certificate templates are stored in /templates.
# The selected template is recolored from the single Template Color control.
# No additional decorative background is drawn behind the SVG.
# ============================================================

is_certificate_template <- function(template_name) {
  is.character(template_name) &&
    length(template_name) == 1 &&
    !is.na(template_name) &&
    template_name %in% CERTIFICATE_TEMPLATE_IDS
}

get_template_path <- function(template_name) {
  if (!is_certificate_template(template_name)) {
    stop("Unknown certificate template.")
  }

  candidates <- c(
    file.path(getwd(), "templates", paste0(template_name, ".svg")),
    file.path(getwd(), "templates", paste0(template_name, ".SVG"))
  )

  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      paste0(
        "Template file not found: templates/",
        template_name,
        ".svg"
      )
    )
  }

  existing[[1]]
}

render_template_raster <- function(svg_path, width = 1754, height = 1240) {
  temporary_png <- tempfile(fileext = ".png")

  rsvg::rsvg_png(
    svg = svg_path,
    file = temporary_png,
    width = width,
    height = height
  )

  png::readPNG(temporary_png)
}

# Recolors visible template artwork while preserving white/near-white page areas,
# transparency, antialiasing, and the source artwork's light/dark hierarchy.
# This follows the same recoloring approach used by Acceptance Letter Generator.
recolor_template_artwork <- function(image, primary_color) {
  image_dimensions <- dim(image)

  if (
    is.null(image_dimensions) ||
    length(image_dimensions) != 3 ||
    image_dimensions[3] < 3
  ) {
    return(image)
  }

  target_rgb <- as.numeric(grDevices::col2rgb(primary_color)) / 255

  red_channel <- image[, , 1]
  green_channel <- image[, , 2]
  blue_channel <- image[, , 3]

  ink_strength <- pmax(
    1 - red_channel,
    1 - green_channel,
    1 - blue_channel
  )

  chroma <- pmax(red_channel, green_channel, blue_channel) -
    pmin(red_channel, green_channel, blue_channel)

  neutral_near_white <-
    chroma < 0.02 &
    red_channel > 0.90 &
    green_channel > 0.90 &
    blue_channel > 0.90

  artwork_mask <- ink_strength > 0.012 & !neutral_near_white

  if (image_dimensions[3] >= 4) {
    alpha_channel <- image[, , 4]
    artwork_mask <- artwork_mask & alpha_channel > 0.001
  }

  if (!any(artwork_mask)) {
    return(image)
  }

  reference_strength <- suppressWarnings(
    as.numeric(stats::quantile(
      ink_strength[artwork_mask],
      probs = 0.995,
      na.rm = TRUE,
      names = FALSE
    ))
  )

  if (!is.finite(reference_strength) || reference_strength <= 0) {
    reference_strength <- max(ink_strength[artwork_mask], na.rm = TRUE)
  }

  if (!is.finite(reference_strength) || reference_strength <= 0) {
    return(image)
  }

  tint_strength <- ink_strength / reference_strength
  tint_strength[tint_strength < 0] <- 0
  tint_strength[tint_strength > 1] <- 1

  recolored_image <- image

  for (channel_index in 1:3) {
    channel <- recolored_image[, , channel_index]
    channel[artwork_mask] <-
      1 - tint_strength[artwork_mask] * (1 - target_rgb[channel_index])
    recolored_image[, , channel_index] <- channel
  }

  recolored_image
}

draw_certificate_template <- function(template_name, template_color) {
  if (!is_certificate_template(template_name)) {
    template_name <- "whisper_current"
  }

  template <- render_template_raster(get_template_path(template_name))
  template <- recolor_template_artwork(template, template_color)

  grid.raster(
    image = template,
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
}

# ============================================================
# LIGHTWEIGHT RICH TEXT
# Supported: **bold**, *italic*, ***bold italic***.
# {title} is bold by default. {name} is bold and has an independent font size.
# ============================================================

combine_font_face <- function(base_face = "plain", bold = FALSE, italic = FALSE) {
  base_bold <- base_face %in% c("bold", "bold.italic", "bolditalic")
  base_italic <- base_face %in% c("italic", "bold.italic", "bolditalic")

  final_bold <- base_bold || bold
  final_italic <- base_italic || italic

  if (final_bold && final_italic) return("bold.italic")
  if (final_bold) return("bold")
  if (final_italic) return("italic")
  "plain"
}

parse_inline_segments <- function(text, base_face = "plain") {
  segments <- list()
  buffer <- ""
  markdown_bold <- FALSE
  markdown_italic <- FALSE
  forced_bold <- FALSE
  name_segment <- FALSE
  position <- 1L
  text_length <- nchar(text)

  current_face <- function() {
    combine_font_face(
      base_face = base_face,
      bold = markdown_bold || forced_bold || name_segment,
      italic = markdown_italic
    )
  }

  flush_buffer <- function() {
    if (nzchar(buffer)) {
      segments[[length(segments) + 1L]] <<- list(
        text = buffer,
        face = current_face(),
        is_name = name_segment
      )
      buffer <<- ""
    }
  }

  while (position <= text_length) {
    remaining <- substring(text, position)

    if (startsWith(remaining, "[[NAME]]")) {
      flush_buffer()
      name_segment <- TRUE
      position <- position + 8L
      next
    }

    if (startsWith(remaining, "[[/NAME]]")) {
      flush_buffer()
      name_segment <- FALSE
      position <- position + 9L
      next
    }

    if (startsWith(remaining, "[[B]]")) {
      flush_buffer()
      forced_bold <- TRUE
      position <- position + 5L
      next
    }

    if (startsWith(remaining, "[[/B]]")) {
      flush_buffer()
      forced_bold <- FALSE
      position <- position + 6L
      next
    }

    if (startsWith(remaining, "***")) {
      flush_buffer()
      markdown_bold <- !markdown_bold
      markdown_italic <- !markdown_italic
      position <- position + 3L
      next
    }

    if (startsWith(remaining, "**")) {
      flush_buffer()
      markdown_bold <- !markdown_bold
      position <- position + 2L
      next
    }

    if (startsWith(remaining, "*")) {
      flush_buffer()
      markdown_italic <- !markdown_italic
      position <- position + 1L
      next
    }

    buffer <- paste0(buffer, substring(text, position, position))
    position <- position + 1L
  }

  flush_buffer()
  segments
}

tokenize_inline <- function(text, base_face = "plain") {
  segments <- parse_inline_segments(text, base_face = base_face)
  tokens <- list()

  for (segment in segments) {
    if (!nzchar(segment$text)) next

    locations <- gregexpr("\\s+|\\S+", segment$text, perl = TRUE)[[1]]
    if (length(locations) == 1L && locations[1] == -1L) next

    values <- regmatches(segment$text, list(locations))[[1]]

    for (value in values) {
      tokens[[length(tokens) + 1L]] <- list(
        text = value,
        face = segment$face,
        is_name = isTRUE(segment$is_name)
      )
    }
  }

  tokens
}

measure_text_width <- function(text, font_size, font_family, font_face = "plain") {
  grob <- textGrob(
    text,
    gp = gpar(
      fontsize = font_size,
      fontfamily = font_family,
      fontface = font_face
    )
  )

  convertWidth(grobWidth(grob), "inches", valueOnly = TRUE)
}

wrap_rich_line <- function(
    text,
    maximum_width,
    font_size,
    font_family,
    base_face = "plain",
    name_font_size = NULL) {

  tokens <- tokenize_inline(text, base_face = base_face)

  if (length(tokens) == 0L) {
    return(list())
  }

  lines <- list()
  current_line <- list()
  current_width <- 0

  for (token in tokens) {
    is_space <- grepl("^\\s+$", token$text, perl = TRUE)

    if (is_space && length(current_line) == 0L) next

    token$font_size <- if (isTRUE(token$is_name) && !is.null(name_font_size)) {
      name_font_size
    } else {
      font_size
    }

    token$width <- measure_text_width(
      token$text,
      token$font_size,
      font_family,
      token$face
    )

    if (
      !is_space &&
      length(current_line) > 0L &&
      current_width + token$width > maximum_width
    ) {
      while (
        length(current_line) > 0L &&
        grepl("^\\s+$", current_line[[length(current_line)]]$text, perl = TRUE)
      ) {
        current_width <- current_width - current_line[[length(current_line)]]$width
        current_line <- current_line[-length(current_line)]
      }

      lines[[length(lines) + 1L]] <- current_line
      current_line <- list()
      current_width <- 0
    }

    if (is_space && current_width + token$width > maximum_width) {
      if (length(current_line) > 0L) {
        lines[[length(lines) + 1L]] <- current_line
      }
      current_line <- list()
      current_width <- 0
      next
    }

    current_line[[length(current_line) + 1L]] <- token
    current_width <- current_width + token$width
  }

  while (
    length(current_line) > 0L &&
    grepl("^\\s+$", current_line[[length(current_line)]]$text, perl = TRUE)
  ) {
    current_line <- current_line[-length(current_line)]
  }

  if (length(current_line) > 0L) {
    lines[[length(lines) + 1L]] <- current_line
  }

  lines
}

rich_block_layout <- function(
    text,
    maximum_width,
    font_size,
    font_family,
    base_face = "plain",
    line_spacing = 1.35,
    paragraph_spacing = 0.10,
    name_font_size = NULL) {

  raw_lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  base_line_height <- (font_size / 72) * line_spacing
  layout <- list()
  total_height <- 0

  for (raw_line in raw_lines) {
    visible_line <- gsub(
      "\\[\\[(?:/?B|/?NAME)\\]\\]",
      "",
      raw_line,
      perl = TRUE
    )

    if (!nzchar(trimws(visible_line))) {
      layout[[length(layout) + 1L]] <- list(
        type = "blank",
        height = base_line_height
      )
      total_height <- total_height + base_line_height
      next
    }

    wrapped <- wrap_rich_line(
      text = raw_line,
      maximum_width = maximum_width,
      font_size = font_size,
      font_family = font_family,
      base_face = base_face,
      name_font_size = name_font_size
    )

    for (tokens in wrapped) {
      token_sizes <- vapply(
        tokens,
        function(token) {
          if (is.null(token$font_size)) font_size else token$font_size
        },
        numeric(1)
      )

      line_height <- max(
        base_line_height,
        (max(token_sizes, na.rm = TRUE) / 72) * line_spacing
      )

      layout[[length(layout) + 1L]] <- list(
        type = "line",
        tokens = tokens,
        height = line_height
      )
      total_height <- total_height + line_height
    }

    total_height <- total_height + paragraph_spacing
    layout[[length(layout) + 1L]] <- list(
      type = "spacing",
      height = paragraph_spacing
    )
  }

  list(items = layout, height = total_height)
}

draw_centered_rich_block <- function(
    text,
    center_x,
    top_y,
    text_width,
    font_size,
    font_family,
    font_color = "#000000",
    base_face = "plain",
    line_spacing = 1.35,
    paragraph_spacing = 0.10,
    name_font_size = NULL) {

  layout <- rich_block_layout(
    text = text,
    maximum_width = text_width,
    font_size = font_size,
    font_family = font_family,
    base_face = base_face,
    line_spacing = line_spacing,
    paragraph_spacing = paragraph_spacing,
    name_font_size = name_font_size
  )

  current_y <- top_y

  for (item in layout$items) {
    if (item$type %in% c("blank", "spacing")) {
      current_y <- current_y - item$height
      next
    }

    line_width <- sum(vapply(item$tokens, function(token) token$width, numeric(1)))
    current_x <- center_x - line_width / 2

    for (token in item$tokens) {
      token_font_size <- if (is.null(token$font_size)) font_size else token$font_size

      grid.text(
        label = token$text,
        x = unit(current_x, "inches"),
        y = unit(current_y, "inches"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = token_font_size,
          fontfamily = font_family,
          fontface = token$face,
          col = font_color
        )
      )

      current_x <- current_x + token$width
    }

    current_y <- current_y - item$height
  }

  current_y
}

fit_font_size <- function(
    text,
    text_width,
    maximum_height,
    font_family,
    preferred_size,
    minimum_size,
    base_face = "plain",
    line_spacing = 1.35,
    paragraph_spacing = 0.10,
    name_font_size = NULL) {

  candidate <- preferred_size

  while (candidate > minimum_size) {
    layout <- rich_block_layout(
      text = text,
      maximum_width = text_width,
      font_size = candidate,
      font_family = font_family,
      base_face = base_face,
      line_spacing = line_spacing,
      paragraph_spacing = paragraph_spacing,
      name_font_size = name_font_size
    )

    if (layout$height <= maximum_height) {
      return(candidate)
    }

    candidate <- candidate - 0.5
  }

  minimum_size
}

# ============================================================
# IMAGE HELPERS
# ============================================================

fit_image_dimensions <- function(image_path, maximum_width, maximum_height) {
  raster <- read_raster_image(image_path)
  dims <- dim(raster)
  aspect_ratio <- dims[2] / dims[1]

  width <- maximum_width
  height <- width / aspect_ratio

  if (height > maximum_height) {
    height <- maximum_height
    width <- height * aspect_ratio
  }

  list(image = raster, width = width, height = height)
}

draw_logo <- function(
    logo_path,
    x_position,
    y_position,
    maximum_width,
    maximum_height,
    horizontal_justification = "left") {

  if (!has_file(logo_path)) return(invisible(NULL))

  fitted <- fit_image_dimensions(logo_path, maximum_width, maximum_height)

  grid.raster(
    image = fitted$image,
    x = unit(x_position, "inches"),
    y = unit(y_position, "inches"),
    width = unit(fitted$width, "inches"),
    height = unit(fitted$height, "inches"),
    just = c(horizontal_justification, "top"),
    interpolate = TRUE
  )

  invisible(NULL)
}

# Converts a user-selected logo slot into an anchor point on the certificate.
# All logo slots use the same size rules; only the anchor and justification change.
get_logo_layout <- function(position, page_width, left_margin, right_margin) {
  top_y <- 7.48
  bottom_y <- 2.15

  layouts <- list(
    top_left = list(x = left_margin, y = top_y, just = "left"),
    top_center = list(x = page_width / 2, y = top_y, just = "center"),
    top_right = list(x = page_width - right_margin, y = top_y, just = "right"),
    bottom_left = list(x = left_margin, y = bottom_y, just = "left"),
    bottom_center = list(x = page_width / 2, y = bottom_y, just = "center"),
    bottom_right = list(x = page_width - right_margin, y = bottom_y, just = "right")
  )

  if (!position %in% names(layouts)) {
    position <- "top_left"
  }

  layouts[[position]]
}

# Returns the left edge of one of the three signature slots.
get_signature_x <- function(
    position,
    page_width,
    left_margin,
    right_margin,
    block_width) {

  positions <- c(
    bottom_left = left_margin,
    bottom_center = (page_width - block_width) / 2,
    bottom_right = page_width - right_margin - block_width
  )

  if (!position %in% names(positions)) {
    position <- "bottom_right"
  }

  unname(positions[[position]])
}

draw_signature_block <- function(
    signature_text,
    signature_image,
    x_left,
    top_y,
    block_width,
    image_width,
    image_height,
    font_family,
    text_color = "#000000") {

  current_y <- top_y

  if (has_file(signature_image)) {
    fitted <- fit_image_dimensions(
      signature_image,
      maximum_width = min(image_width, block_width),
      maximum_height = image_height
    )

    grid.raster(
      image = fitted$image,
      x = unit(x_left + block_width / 2, "inches"),
      y = unit(current_y, "inches"),
      width = unit(fitted$width, "inches"),
      height = unit(fitted$height, "inches"),
      just = c("center", "top"),
      interpolate = TRUE
    )

    current_y <- current_y - fitted$height - 0.07
  }

  if (has_text(signature_text)) {
    draw_centered_rich_block(
      text = signature_text,
      center_x = x_left + block_width / 2,
      top_y = current_y,
      text_width = block_width,
      font_size = 9.5,
      font_family = font_family,
      font_color = text_color,
      base_face = "plain",
      line_spacing = 1.18,
      paragraph_spacing = 0.02
    )
  }

  invisible(NULL)
}

# ============================================================
# PDF GENERATOR
# One participant = one landscape, single-page certificate.
# ============================================================

create_certificate_pdf <- function(
    participant_name,
    participant_title = "",
    certificate_title,
    body_text,
    logo_count = 1,
    logo_image_1 = NULL,
    logo_position_1 = "top_left",
    logo_image_2 = NULL,
    logo_position_2 = "top_right",
    logo_width_cm = 4.5,
    logo_height_cm = 2.2,
    signature_count = 1,
    signature_text_1,
    signature_image_1 = NULL,
    signature_position_1 = "bottom_right",
    signature_text_2 = "",
    signature_image_2 = NULL,
    signature_position_2 = "bottom_left",
    signature_width_cm = 4,
    signature_height_cm = 2,
    template = "whisper_current",
    template_color = "#0F7A6C",
    title_font_family = "Cormorant Garamond",
    body_font_family = "IBM Plex Sans",
    name_font_size = 24,
    output_file) {

  page_width <- 11.69
  page_height <- 8.27
  left_margin <- 1.05
  right_margin <- 1.05
  content_width <- page_width - left_margin - right_margin

  participant_name <- trimws(as.character(participant_name))
  participant_title <- trimws(as.character(participant_title))

  title_text <- fill_placeholders_styled(
    certificate_title,
    participant_name,
    participant_title
  )

  certificate_body <- fill_placeholders_styled(
    body_text,
    participant_name,
    participant_title
  )

  signature_text_1 <- fill_placeholders(
    signature_text_1,
    participant_name,
    participant_title
  )

  signature_text_2 <- fill_placeholders(
    signature_text_2,
    participant_name,
    participant_title
  )

  title_font_family <- ensure_open_font(title_font_family)
  body_font_family <- ensure_open_font(body_font_family)

  if (capabilities("cairo")) {
    grDevices::cairo_pdf(
      filename = output_file,
      width = page_width,
      height = page_height,
      family = "sans"
    )
  } else {
    grDevices::pdf(
      file = output_file,
      width = page_width,
      height = page_height,
      family = "sans",
      useDingbats = FALSE
    )
  }

  on.exit(grDevices::dev.off(), add = TRUE)

  grid.newpage()
  draw_certificate_template(
    template_name = template,
    template_color = template_color
  )

  # ----------------------------------------------------------
  # LOGOS
  # Each logo can independently use one of six slots:
  # top-left, top-center, top-right, bottom-left, bottom-center, bottom-right.
  # ----------------------------------------------------------

  logo_width <- logo_width_cm / 2.54
  logo_height <- logo_height_cm / 2.54

  logo_layout_1 <- get_logo_layout(
    logo_position_1, page_width, left_margin, right_margin
  )

  draw_logo(
    logo_path = logo_image_1,
    x_position = logo_layout_1$x,
    y_position = logo_layout_1$y,
    maximum_width = logo_width,
    maximum_height = logo_height,
    horizontal_justification = logo_layout_1$just
  )

  if (logo_count == 2) {
    logo_layout_2 <- get_logo_layout(
      logo_position_2, page_width, left_margin, right_margin
    )

    draw_logo(
      logo_path = logo_image_2,
      x_position = logo_layout_2$x,
      y_position = logo_layout_2$y,
      maximum_width = logo_width,
      maximum_height = logo_height,
      horizontal_justification = logo_layout_2$just
    )
  }

  # Keep the title safely below any top-positioned logo, including large uploads.
  top_logo_bottoms <- numeric(0)

  if (has_file(logo_image_1) && grepl("^top_", logo_position_1)) {
    fitted_1 <- fit_image_dimensions(logo_image_1, logo_width, logo_height)
    top_logo_bottoms <- c(top_logo_bottoms, logo_layout_1$y - fitted_1$height)
  }

  if (
    logo_count == 2 &&
    has_file(logo_image_2) &&
    grepl("^top_", logo_position_2)
  ) {
    fitted_2 <- fit_image_dimensions(logo_image_2, logo_width, logo_height)
    top_logo_bottoms <- c(top_logo_bottoms, logo_layout_2$y - fitted_2$height)
  }

  title_top_y <- 6.55

  if (length(top_logo_bottoms) > 0) {
    title_top_y <- min(title_top_y, min(top_logo_bottoms) - 0.16)
  }

  # Do not let unusually tall logos push the certificate title below its usable area.
  title_top_y <- max(title_top_y, 5.55)

  # ----------------------------------------------------------
  # CERTIFICATE TITLE
  # ----------------------------------------------------------

  title_font_size <- fit_font_size(
    text = title_text,
    text_width = content_width - 1.00,
    maximum_height = 0.90,
    font_family = title_font_family,
    preferred_size = 30,
    minimum_size = 20,
    base_face = "plain",
    line_spacing = 1.12,
    paragraph_spacing = 0.02,
    name_font_size = name_font_size
  )

  title_bottom_y <- draw_centered_rich_block(
    text = title_text,
    center_x = page_width / 2,
    top_y = title_top_y,
    text_width = content_width - 1.00,
    font_size = title_font_size,
    font_family = title_font_family,
    font_color = template_color,
    base_face = "plain",
    line_spacing = 1.12,
    paragraph_spacing = 0.02,
    name_font_size = name_font_size
  )

  divider_y <- title_bottom_y - 0.05

  grid.lines(
    x = unit(c(3.65, page_width - 3.65), "inches"),
    y = unit(c(divider_y, divider_y), "inches"),
    gp = gpar(
      col = grDevices::adjustcolor(template_color, alpha.f = 0.55),
      lwd = 1.2
    )
  )

  # ----------------------------------------------------------
  # BODY
  # Body automatically shrinks to remain above the signature zone.
  # ----------------------------------------------------------

  body_top_y <- divider_y - 0.35
  signature_zone_top <- 2.25
  body_bottom_y <- signature_zone_top + 0.35
  available_body_height <- max(1.20, body_top_y - body_bottom_y)

  body_font_size <- fit_font_size(
    text = certificate_body,
    text_width = content_width - 1.25,
    maximum_height = available_body_height,
    font_family = body_font_family,
    preferred_size = 14,
    minimum_size = 9.5,
    base_face = "plain",
    line_spacing = 1.38,
    paragraph_spacing = 0.09,
    name_font_size = name_font_size
  )

  draw_centered_rich_block(
    text = certificate_body,
    center_x = page_width / 2,
    top_y = body_top_y,
    text_width = content_width - 1.25,
    font_size = body_font_size,
    font_family = body_font_family,
    font_color = "#151918",
    base_face = "plain",
    line_spacing = 1.38,
    paragraph_spacing = 0.09,
    name_font_size = name_font_size
  )

  # ----------------------------------------------------------
  # SIGNATURES
  # Each signature can independently use bottom-left, bottom-center, or bottom-right.
  # ----------------------------------------------------------

  signature_width <- signature_width_cm / 2.54
  signature_height <- signature_height_cm / 2.54
  signature_block_width <- 3.10
  signature_top_y <- signature_zone_top

  signature_1_x <- get_signature_x(
    signature_position_1,
    page_width,
    left_margin,
    right_margin,
    signature_block_width
  )

  draw_signature_block(
    signature_text = signature_text_1,
    signature_image = signature_image_1,
    x_left = signature_1_x,
    top_y = signature_top_y,
    block_width = signature_block_width,
    image_width = signature_width,
    image_height = signature_height,
    font_family = body_font_family
  )

  if (signature_count == 2) {
    signature_2_x <- get_signature_x(
      signature_position_2,
      page_width,
      left_margin,
      right_margin,
      signature_block_width
    )

    draw_signature_block(
      signature_text = signature_text_2,
      signature_image = signature_image_2,
      x_left = signature_2_x,
      top_y = signature_top_y,
      block_width = signature_block_width,
      image_width = signature_width,
      image_height = signature_height,
      font_family = body_font_family
    )
  }
}

# ============================================================
# USER INTERFACE
# ============================================================

acceptance_link_class <- if (nzchar(ACCEPTANCE_LETTER_URL)) {
  "tool-tab"
} else {
  "tool-tab tool-tab-disabled"
}

ui <- fluidPage(
  tags$head(
    tags$title("ConferCraft | Certificate Generator"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=DM+Serif+Display:ital@0;1&",
        "family=IBM+Plex+Sans:wght@400;500;600;700&display=swap"
      )
    ),
    tags$script(HTML(
      "
      (function() {
        try {
          if (localStorage.getItem('confercraft-theme') === 'dark') {
            document.documentElement.classList.add('confercraft-dark');
          }
        } catch (error) {}
      })();
      "
    )),
    tags$style(HTML(
      "
      :root {
        --ink: #151918;
        --paper: #fbfcfb;
        --card: #ffffff;
        --teal: #0f7a6c;
        --teal-dark: #0a5e54;
        --teal-soft: #e9f6f3;
        --muted: #69716f;
        --line: #dfe9e6;
        --line-strong: #cadbd6;
        --success-bg: #e8f6f0;
        --success-fg: #17634f;
        --warning-bg: #fff4dd;
        --warning-fg: #855d18;
        --shadow: 0 18px 55px rgba(16, 75, 65, 0.08);
        --shadow-soft: 0 8px 28px rgba(16, 75, 65, 0.055);
      }

      html, body {
        min-height: 100%;
        background:
          radial-gradient(circle at 88% 6%, rgba(15, 122, 108, 0.085), transparent 29%),
          radial-gradient(circle at 5% 64%, rgba(15, 122, 108, 0.045), transparent 25%),
          linear-gradient(180deg, #ffffff 0%, #fbfcfb 43%, #f5faf8 100%);
        background-attachment: fixed;
        font-family: 'IBM Plex Sans', system-ui, sans-serif;
        color: var(--ink);
      }

      body { min-height: 100vh; }
      button, input, select, textarea, .form-control, .btn,
      .selectize-input, .selectize-dropdown {
        font-family: 'IBM Plex Sans', system-ui, sans-serif;
      }

      .container-fluid { padding: 0; }
      .app-shell { max-width: 1640px; margin: 0 auto; padding: 22px 30px 54px; }

      .app-topbar, .sidebar-card, .workspace-card {
        position: relative;
        overflow: hidden;
        background: rgba(255,255,255,0.96);
        border: 1px solid var(--line);
        border-radius: 24px;
      }

      .app-topbar {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 24px;
        margin-bottom: 28px;
        padding: 18px 22px;
        box-shadow: var(--shadow-soft);
      }

      .app-topbar::before, .sidebar-card::before, .workspace-card::before,
      .app-topbar::after, .sidebar-card::after, .workspace-card::after {
        content: '';
        position: absolute;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, var(--teal), #5cb5a7, #b8ded6);
        pointer-events: none;
      }
      .app-topbar::before, .sidebar-card::before, .workspace-card::before { top: 0; }
      .app-topbar::after, .sidebar-card::after, .workspace-card::after {
        bottom: 0;
        background: linear-gradient(90deg, #b8ded6, #5cb5a7, var(--teal));
      }

      .brand-kicker {
        display: inline-flex;
        padding: 7px 12px;
        border-radius: 999px;
        background: var(--teal-soft);
        border: 1px solid #c7e7df;
        color: var(--teal-dark);
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        margin-bottom: 10px;
      }

      .brand-home-link {
        text-decoration: none;
        transition: transform 0.15s ease, background 0.15s ease, border-color 0.15s ease;
      }

      .brand-home-link:hover,
      .brand-home-link:focus {
        color: var(--teal-dark);
        text-decoration: none;
        background: #ddf1ec;
        border-color: #a9d8cd;
        transform: translateY(-1px);
        outline: none;
      }

      .brand-title {
        font-family: 'DM Serif Display', serif;
        font-size: clamp(30px, 3vw, 46px);
        font-weight: 400;
        line-height: 1.02;
        letter-spacing: -0.025em;
        margin: 0;
      }

      .brand-subtitle {
        max-width: 680px;
        color: var(--muted);
        font-size: 13px;
        line-height: 1.7;
        margin: 9px 0 0;
      }

      .topbar-right {
        display: flex;
        align-items: center;
        gap: 12px;
        flex-wrap: wrap;
        justify-content: flex-end;
      }

      .tool-switcher {
        display: inline-flex;
        padding: 4px;
        border-radius: 999px;
        background: #f4f8f7;
        border: 1px solid var(--line);
      }

      .tool-tab {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-height: 36px;
        padding: 0 13px;
        border-radius: 999px;
        color: #50605c;
        font-size: 12px;
        font-weight: 700;
        text-decoration: none;
        white-space: nowrap;
        transition: color 0.15s ease, background 0.15s ease, opacity 0.15s ease;
      }
      .tool-tab:hover,
      .tool-tab:focus {
        color: var(--teal-dark);
        text-decoration: none;
        outline: none;
      }
      .tool-tab-active { background: var(--teal); color: #ffffff !important; cursor: default; }
      .tool-tab-disabled { opacity: 0.48; cursor: not-allowed; pointer-events: none; }

      .topbar-actions {
        display: flex;
        align-items: center;
        gap: 10px;
        flex: 0 0 auto;
      }

      .topbar-icon-link {
        width: 46px;
        height: 46px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 auto;
        background: #ffffff;
        color: #18201e;
        border: 1px solid var(--line-strong);
        border-radius: 50%;
        box-shadow: 0 8px 22px rgba(13, 67, 58, 0.08);
        text-decoration: none;
        cursor: pointer;
        transition: transform 0.16s ease, box-shadow 0.16s ease, color 0.16s ease, border-color 0.16s ease;
      }
      .topbar-icon-link:hover,
      .topbar-icon-link:focus {
        color: var(--teal);
        border-color: #9fd0c5;
        text-decoration: none;
        transform: translateY(-2px);
        box-shadow: 0 12px 28px rgba(13, 67, 58, 0.13);
        outline: none;
      }
      .topbar-icon-link svg { width: 22px; height: 22px; display: block; fill: currentColor; }

      .sidebar-card { padding: 24px; box-shadow: var(--shadow); margin-bottom: 20px; }
      .workspace-card { padding: 24px; box-shadow: var(--shadow-soft); margin-bottom: 20px; }

      .sidebar-card h4 {
        margin-top: 24px;
        font-size: 14px;
        font-weight: 700;
      }
      .sidebar-card h4:first-of-type { margin-top: 2px; }
      .sidebar-card h4::after {
        content: '';
        display: block;
        width: 28px;
        height: 2px;
        margin-top: 7px;
        border-radius: 999px;
        background: var(--teal);
        opacity: .75;
      }

      .sidebar-card h5 {
        font-family: 'DM Serif Display', serif;
        font-size: 18px;
        font-weight: 400;
        margin: 4px 0 11px;
      }

      .sidebar-card label, .sidebar-card .control-label {
        color: #3f4846;
        font-size: 12px;
        font-weight: 600;
      }
      .sidebar-card .help-block { color: #7b8481; font-size: 11px; line-height: 1.55; }
      .sidebar-card hr { border: 0; border-top: 1px solid var(--line); margin: 23px 0; }

      .sidebar-card .form-control,
      .sidebar-card .selectize-input,
      .sidebar-card input[type='number'] {
        border: 1px solid #d9e5e1 !important;
        border-radius: 12px !important;
        background: #fbfdfc !important;
        color: var(--ink) !important;
        box-shadow: none !important;
      }
      .sidebar-card textarea.form-control { min-height: 112px; line-height: 1.55; resize: vertical; }

      .logo-box, .signature-box, .font-box, .color-box {
        background: linear-gradient(180deg, #f8fcfb 0%, #f2f9f7 100%);
        border: 1px solid #d9e9e4;
        border-radius: 17px;
        padding: 15px;
        margin: 11px 0 15px;
      }

      /* File uploads use the same compact joined control as Acceptance Letter Generator. */
      .sidebar-card .shiny-input-container .input-group {
        width: 100%;
        display: flex !important;
        align-items: stretch !important;
        overflow: visible;
        background: transparent;
        border: 0;
        border-radius: 0;
        box-shadow: none;
      }

      /* Keep the Browse half compact and exactly the same height as the filename field. */
      .sidebar-card .input-group-btn {
        display: flex !important;
        align-items: stretch !important;
        width: auto !important;
        height: 34px !important;
        min-height: 34px !important;
        flex: 0 0 auto;
        white-space: nowrap;
      }

      .sidebar-card .btn-file,
      .sidebar-card .input-group-btn .btn {
        position: relative;
        z-index: 2;
        display: inline-flex !important;
        align-items: center;
        justify-content: center;
        box-sizing: border-box !important;
        height: 34px !important;
        min-height: 34px !important;
        line-height: 1 !important;
        margin: 0 !important;
        padding: 0 12px !important;
        background: var(--teal);
        border: 1px solid var(--teal) !important;
        border-radius: 10px 0 0 10px !important;
        color: #ffffff !important;
        font-size: 12px;
        font-weight: 700;
        box-shadow: none !important;
        transition: background 0.15s ease, border-color 0.15s ease, transform 0.15s ease;
      }

      /* Keep the real file input transparent and clickable over Browse. */
      .sidebar-card .btn-file input[type='file'] {
        position: absolute;
        inset: 0;
        width: 100%;
        height: 100%;
        margin: 0;
        opacity: 0;
        cursor: pointer;
      }

      /* The filename half meets Browse with no gap, border, or doubled radius. */
      .sidebar-card .input-group .form-control {
        flex: 1 1 auto;
        width: 1%;
        min-width: 0;
        box-sizing: border-box !important;
        height: 34px !important;
        min-height: 34px !important;
        line-height: 1.2 !important;
        margin: 0 !important;
        padding: 0 11px !important;
        background: #ffffff !important;
        border: 1px solid #d9e5e1 !important;
        border-left: 0 !important;
        border-radius: 0 10px 10px 0 !important;
        box-shadow: none !important;
        color: #697370 !important;
        font-size: 12px;
      }

      .sidebar-card .btn-file:hover,
      .sidebar-card .btn-file:focus,
      .sidebar-card .input-group-btn .btn:hover,
      .sidebar-card .input-group-btn .btn:focus {
        background: var(--teal-dark);
        border-color: var(--teal-dark) !important;
        color: #ffffff !important;
        outline: none;
        transform: translateY(-1px);
      }

      .sidebar-card .input-group:focus-within .btn-file,
      .sidebar-card .input-group:focus-within .input-group-btn .btn {
        border-color: #72b9ab !important;
      }

      .sidebar-card .input-group:focus-within .form-control {
        border-color: #72b9ab !important;
        box-shadow: 0 0 0 3px rgba(15, 122, 108, 0.08) !important;
      }

      .sidebar-card .shiny-file-input-progress,
      .sidebar-card .progress {
        background: #e7f1ee;
        border-radius: 999px;
        box-shadow: none;
      }

      .sidebar-card .shiny-file-input-progress .progress-bar,
      .sidebar-card .progress-bar,
      .sidebar-card .progress-bar-info {
        background-color: var(--teal) !important;
        background-image: none !important;
        color: #ffffff !important;
        box-shadow: none !important;
      }

      .color-control-row { display: flex; align-items: stretch; gap: 9px; }
      .color-control-row .form-group { flex: 1 1 auto; margin-bottom: 0; }
      .color-picker-swatch {
        flex: 0 0 58px;
        width: 58px;
        height: 42px;
        padding: 3px;
        border: 1px solid #d9e5e1;
        border-radius: 12px;
        background: white;
      }
      .hex-warning { color: #a64040; font-weight: 600; font-size: 12px; margin-top: 8px; }

      .action-row { display: flex; gap: 10px; flex-wrap: wrap; margin-top: 18px; }
      #generate_certificates, #update_preview {
        border-radius: 999px;
        font-weight: 700;
        padding: 11px 17px;
      }
      #generate_certificates { background: var(--teal); color: white; border: 1px solid var(--teal); }
      #update_preview { background: var(--teal-soft); color: var(--teal-dark); border: 1px solid #c7e4de; }

      .workspace-card h3 {
        font-family: 'DM Serif Display', serif;
        font-size: 26px;
        font-weight: 400;
        margin-top: 0;
      }
      .workspace-card .table { font-size: 12px; margin-bottom: 0; }
      .preview-copy { color: var(--muted); font-size: 12px; line-height: 1.55; }

      .preview-placeholder {
        min-height: 520px;
        border: 1px dashed #bdd8d1;
        border-radius: 20px;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        background: #f8fcfb;
        color: #73807c;
        padding: 34px;
      }
      .preview-placeholder strong {
        display: block;
        font-family: 'DM Serif Display', serif;
        font-size: 23px;
        font-weight: 400;
        color: #26312e;
        margin-bottom: 8px;
      }

      .pdf-frame {
        width: 100%;
        height: 760px;
        border: 1px solid #d6e4e0;
        border-radius: 20px;
        background: #eef6f4;
      }

      .status-pill {
        display: inline-flex;
        border-radius: 999px;
        padding: 7px 11px;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 12px;
        border: 1px solid transparent;
      }
      .status-ready { background: var(--success-bg); color: var(--success-fg); border-color: #cce8dc; }
      .status-stale { background: var(--warning-bg); color: var(--warning-fg); border-color: #f0ddb8; }
      .status-idle { background: #f3f6f5; color: #68716f; border-color: #e0e7e5; }

      #download_certificates {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        column-gap: 8px;
        background: var(--teal);
        color: #ffffff;
        border: none;
        border-radius: 999px;
        padding: 11px 17px;
        font-weight: 700;
        text-decoration: none;
        margin-top: 12px;
        box-shadow: 0 8px 22px rgba(15, 122, 108, 0.18);
        transition: transform 0.15s ease, background 0.15s ease, box-shadow 0.15s ease;
      }

      #download_certificates:hover,
      #download_certificates:focus {
        color: #ffffff;
        background: var(--teal-dark);
        transform: translateY(-1px);
        box-shadow: 0 10px 26px rgba(10, 94, 84, 0.20);
        text-decoration: none;
      }

      html.confercraft-dark {
        color-scheme: dark;
        --ink: #f1f4f3;
        --paper: #0d1014;
        --card: #14191d;
        --teal: #25b6a0;
        --teal-dark: #73d3c3;
        --teal-soft: #15312d;
        --muted: #9ba6a3;
        --line: #29332f;
        --line-strong: #37443f;
      }
      html.confercraft-dark, html.confercraft-dark body {
        background: linear-gradient(180deg, #0d1014 0%, #0f1317 48%, #10161a 100%);
        color: var(--ink);
      }
      html.confercraft-dark .app-topbar,
      html.confercraft-dark .sidebar-card,
      html.confercraft-dark .workspace-card {
        background: rgba(17,22,26,.96);
      }
      html.confercraft-dark .brand-title,
      html.confercraft-dark .workspace-card h3,
      html.confercraft-dark .sidebar-card h5,
      html.confercraft-dark .preview-placeholder strong { color: #f5f6f6; }
      html.confercraft-dark .tool-switcher {
        background: #11171b;
        border-color: #34413e;
      }
      html.confercraft-dark .tool-tab { color: #b9c6c2; }
      html.confercraft-dark .tool-tab:hover,
      html.confercraft-dark .tool-tab:focus { color: #73d3c3; }
      html.confercraft-dark .tool-tab-active { background: var(--teal); color: #ffffff !important; }
      html.confercraft-dark .topbar-icon-link {
        background: #1a2126;
        color: #edf2f0;
        border-color: #35423e;
        box-shadow: 0 8px 22px rgba(0, 0, 0, 0.24);
      }
      html.confercraft-dark .topbar-icon-link:hover,
      html.confercraft-dark .topbar-icon-link:focus {
        color: #5bd0be;
        border-color: #3a8d80;
        background: #20292e;
        box-shadow: 0 12px 28px rgba(0, 0, 0, 0.30);
      }
      html.confercraft-dark .logo-box,
      html.confercraft-dark .signature-box,
      html.confercraft-dark .font-box,
      html.confercraft-dark .color-box {
        background: linear-gradient(180deg, #131d20 0%, #11191c 100%);
        border-color: #2c3b38;
      }
      html.confercraft-dark .sidebar-card .form-control,
      html.confercraft-dark .sidebar-card .selectize-input,
      html.confercraft-dark .sidebar-card input[type='number'] {
        background: #11171b !important;
        border-color: #34423e !important;
        color: #eef2f1 !important;
      }
      html.confercraft-dark .sidebar-card .input-group .form-control {
        background: #11171b !important;
        border-color: #34423e !important;
        border-left: 0 !important;
        color: #aeb9b6 !important;
      }
      html.confercraft-dark .preview-placeholder { background: #11191d; border-color: #35564f; }
      html.confercraft-dark .pdf-frame { background: #1a2226; border-color: #34413e; }

      .theme-icon-sun { display: none !important; }
      html.confercraft-dark .theme-icon-moon { display: none !important; }
      html.confercraft-dark .theme-icon-sun { display: block !important; }

      @media (max-width: 991px) {
        .app-shell { padding: 14px; }
        .app-topbar { align-items: flex-start; flex-direction: column; }
        .topbar-right { width: 100%; justify-content: flex-start; }
        .topbar-actions { flex-wrap: wrap; }
        .pdf-frame { height: 600px; }
      }
      "
    )),
    tags$script(HTML(
      "
      (function() {
        function isDark() {
          return document.documentElement.classList.contains('confercraft-dark');
        }
        function syncButton() {
          var button = document.getElementById('theme_toggle');
          if (!button) return;
          var label = isDark() ? 'Switch to light mode' : 'Switch to dark mode';
          button.setAttribute('title', label);
          button.setAttribute('aria-label', label);
        }
        document.addEventListener('DOMContentLoaded', syncButton);
        document.addEventListener('click', function(event) {
          var button = event.target.closest ? event.target.closest('#theme_toggle') : null;
          if (!button) return;
          event.preventDefault();
          var nextDark = !isDark();
          document.documentElement.classList.toggle('confercraft-dark', nextDark);
          try { localStorage.setItem('confercraft-theme', nextDark ? 'dark' : 'light'); } catch (error) {}
          syncButton();
        });
      })();
      "
    )),
    tags$script(HTML(
      "
      (function() {
        function normalizeHex(value) {
          if (!value) return null;
          var hex = value.trim().toUpperCase();
          if (/^#([0-9A-F]{3})$/.test(hex)) {
            var chars = hex.substring(1).split('');
            return '#' + chars.map(function(ch) { return ch + ch; }).join('');
          }
          if (/^#([0-9A-F]{6})$/.test(hex)) return hex;
          return null;
        }

        $(document).on('input change', '#template_color_picker', function() {
          $('#template_color').val($(this).val().toUpperCase()).trigger('change');
        });

        $(document).on('input change keyup paste', '#template_color', function() {
          var normalized = normalizeHex($(this).val());
          if (normalized) $('#template_color_picker').val(normalized);
        });
      })();
      "
    ))
  ),

  div(
    class = "app-shell",

    div(
      class = "app-topbar",
      div(
        tags$a(
          class = "brand-kicker brand-home-link",
          href = CONFERCRAFT_HOME_URL,
          title = "Back to ConferCraft home",
          `aria-label` = "Back to ConferCraft home",
          "ConferCraft"
        ),
        h1(class = "brand-title", "Certificate Generator"),
        p(
          class = "brand-subtitle",
          "Build, preview, customize, and export personalized certificates."
        )
      ),
      div(
        class = "topbar-right",
        div(
          class = "tool-switcher",
          tags$a(
            class = acceptance_link_class,
            href = if (nzchar(ACCEPTANCE_LETTER_URL)) ACCEPTANCE_LETTER_URL else NULL,
            title = if (nzchar(ACCEPTANCE_LETTER_URL)) {
              "Open Acceptance Letter Generator"
            } else {
              "Set CONFERCRAFT_ACCEPTANCE_URL to enable this link"
            },
            `aria-disabled` = if (nzchar(ACCEPTANCE_LETTER_URL)) "false" else "true",
            "Acceptance Letters"
          ),
          tags$a(
            class = "tool-tab tool-tab-active",
            href = "#",
            `aria-current` = "page",
            "Certificates"
          )
        ),
        div(
          class = "topbar-actions",
          tags$button(
          id = "theme_toggle",
          class = "topbar-icon-link",
          type = "button",
          title = "Switch to dark mode",
          HTML(paste0(
            '<svg class="theme-icon-moon" viewBox="0 0 24 24" aria-hidden="true"><path d="M20.15 15.42A8.1 8.1 0 0 1 8.58 3.85 8.65 8.65 0 1 0 20.15 15.42Zm-8.2 5.03A6.95 6.95 0 0 1 6.44 9.27a6.9 6.9 0 0 1 .47-3.34 9.25 9.25 0 0 0 10.66 10.66 6.91 6.91 0 0 1-5.62 3.86Z"/></svg>',
            '<svg class="theme-icon-sun" viewBox="0 0 24 24" aria-hidden="true"><path d="M12 7.25A4.75 4.75 0 1 0 12 16.75 4.75 4.75 0 0 0 12 7.25Zm0 8A3.25 3.25 0 1 1 12 8.75a3.25 3.25 0 0 1 0 6.5ZM12 1.5a.75.75 0 0 1 .75.75v2a.75.75 0 0 1-1.5 0v-2A.75.75 0 0 1 12 1.5Zm0 17.5a.75.75 0 0 1 .75.75v2a.75.75 0 0 1-1.5 0v-2A.75.75 0 0 1 12 19Z"/></svg>'
          ))
        ),
        tags$a(
          class = "topbar-icon-link",
          href = "https://github.com/gungorMetehan/ConferCraft",
          target = "_blank",
          rel = "noopener noreferrer",
          title = "ConferCraft on GitHub",
          HTML('<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 .7C5.65.7.5 5.85.5 12.2c0 5.08 3.29 9.39 7.86 10.91.58.1.79-.25.79-.56 0-.28-.01-1.2-.02-2.18-3.2.7-3.88-1.36-3.88-1.36-.52-1.33-1.28-1.68-1.28-1.68-1.05-.72.08-.7.08-.7 1.16.08 1.77 1.19 1.77 1.19 1.03 1.77 2.7 1.26 3.36.96.1-.75.4-1.26.73-1.55-2.55-.29-5.24-1.28-5.24-5.69 0-1.26.45-2.28 1.19-3.09-.12-.29-.52-1.46.11-3.05 0 0 .97-.31 3.17 1.18A11.1 11.1 0 0 1 12 6.2c.98 0 1.96.13 2.88.39 2.2-1.49 3.17-1.18 3.17-1.18.63 1.59.23 2.76.11 3.05.74.81 1.19 1.83 1.19 3.09 0 4.42-2.69 5.39-5.25 5.68.41.36.78 1.06.78 2.14 0 1.55-.01 2.79-.01 3.17 0 .31.21.67.79.56 4.56-1.52 7.85-5.83 7.85-10.9C23.5 5.85 18.35.7 12 .7Z"/></svg>')
        )
        )
      )
    ),

    fluidRow(
      column(
        width = 4,
        div(
          class = "sidebar-card",

          h4("1. Participant List"),
          fileInput(
            inputId = "excel_file",
            label = "Upload Excel File",
            accept = c(".xlsx", ".xls")
          ),
          helpText(
            "Column 1 must contain Full Name. Column 2 may contain Title and is optional."
          ),

          hr(),

          h4("2. Certificate Title"),
          textInput(
            inputId = "certificate_title",
            label = "Title",
            value = "**CERTIFICATE OF PARTICIPATION**"
          ),
          helpText("Markdown: **bold**, *italic*, ***bold italic***"),

          hr(),

          h4("3. Certificate Body"),
          helpText("{name} = Full Name | required placeholder"),
          helpText("{title} = Title | optional second Excel column"),
          textAreaInput(
            inputId = "body_text",
            label = "Body Text",
            value = paste(
              "This certificate is proudly presented to",
              "",
              "{name}",
              "",
              paste(
                "in recognition of their valued participation and contribution",
                "to the scientific program of the event."
              ),
              sep = "\n"
            ),
            rows = 10,
            width = "100%"
          ),
          numericInput(
            inputId = "name_font_size",
            label = "{name} Font Size (pt)",
            value = 24,
            min = 10,
            max = 48,
            step = 1
          ),
          helpText(
            "Controls only the participant name inserted by {name} in the Certificate Title or Body."
          ),

          hr(),

          h4("4. Logos"),
          selectInput(
            inputId = "logo_count",
            label = "Number of Logos",
            choices = c("1" = "1", "2" = "2"),
            selected = "1"
          ),
          helpText(
            "Each logo can be placed in one of six positions. Two logos cannot use the same position."
          ),

          div(
            class = "logo-box",
            h5("Logo 1"),
            fileInput(
              inputId = "logo_image_1",
              label = "Upload Logo",
              accept = c(".png", ".jpg", ".jpeg")
            ),
            selectInput(
              inputId = "logo_position_1",
              label = "Logo Position",
              choices = LOGO_POSITION_CHOICES,
              selected = "top_left"
            )
          ),

          conditionalPanel(
            condition = "input.logo_count == '2'",
            div(
              class = "logo-box",
              h5("Logo 2"),
              fileInput(
                inputId = "logo_image_2",
                label = "Upload Logo",
                accept = c(".png", ".jpg", ".jpeg")
              ),
              selectInput(
                inputId = "logo_position_2",
                label = "Logo Position",
                choices = LOGO_POSITION_CHOICES,
                selected = "top_right"
              )
            )
          ),

          numericInput(
            inputId = "logo_width",
            label = "Maximum Logo Width (cm)",
            value = 4.5,
            min = 1,
            max = 8,
            step = 0.5
          ),
          numericInput(
            inputId = "logo_height",
            label = "Maximum Logo Height (cm)",
            value = 2.2,
            min = 0.5,
            max = 5,
            step = 0.25
          ),

          hr(),

          h4("5. Signatures"),
          selectInput(
            inputId = "signature_count",
            label = "Number of Signatures",
            choices = c("1" = "1", "2" = "2"),
            selected = "1"
          ),

          div(
            class = "signature-box",
            h5("Signature 1"),
            fileInput(
              inputId = "signature_image_1",
              label = "Upload Signature Image",
              accept = c(".png", ".jpg", ".jpeg")
            ),
            textAreaInput(
              inputId = "signature_text_1",
              label = "Signature Text",
              value = paste(
                "Prof. Dr. Name Surname",
                "Congress Chair",
                sep = "\n"
              ),
              rows = 4,
              width = "100%"
            ),
            selectInput(
              inputId = "signature_position_1",
              label = "Signature Position",
              choices = SIGNATURE_POSITION_CHOICES,
              selected = "bottom_right"
            )
          ),

          conditionalPanel(
            condition = "input.signature_count == '2'",
            div(
              class = "signature-box",
              h5("Signature 2"),
              fileInput(
                inputId = "signature_image_2",
                label = "Upload Signature Image",
                accept = c(".png", ".jpg", ".jpeg")
              ),
              textAreaInput(
                inputId = "signature_text_2",
                label = "Signature Text",
                value = paste(
                  "Prof. Dr. Second Name Surname",
                  "Scientific Committee Chair",
                  sep = "\n"
                ),
                rows = 4,
                width = "100%"
              ),
              selectInput(
                inputId = "signature_position_2",
                label = "Signature Position",
                choices = SIGNATURE_POSITION_CHOICES,
                selected = "bottom_left"
              )
            )
          ),

          helpText(
            "Signatures can use Bottom Left, Bottom Center, or Bottom Right. Two signatures cannot use the same position."
          ),

          numericInput(
            inputId = "signature_width",
            label = "Maximum Signature Width (cm)",
            value = 4,
            min = 1,
            max = 8,
            step = 0.5
          ),
          numericInput(
            inputId = "signature_height",
            label = "Maximum Signature Height (cm)",
            value = 2,
            min = 0.5,
            max = 4,
            step = 0.25
          ),

          hr(),

          h4("6. PDF Design"),
          selectInput(
            inputId = "template",
            label = "Template",
            choices = CERTIFICATE_TEMPLATE_CHOICES,
            selected = "whisper_current"
          ),
          helpText(
            "Choose one of the eight named SVG certificate templates stored in the templates folder."
          ),

          div(
            class = "font-box",
            h5("PDF Fonts"),
            selectInput(
              inputId = "title_font_family",
              label = "Certificate Title Font",
              choices = OPEN_FONT_CHOICES,
              selected = "Cormorant Garamond"
            ),
            selectInput(
              inputId = "body_font_family",
              label = "Body & Signature Font",
              choices = OPEN_FONT_CHOICES,
              selected = "IBM Plex Sans"
            )
          ),

          div(
            class = "color-box",
            h5("Template Color"),
            div(
              class = "color-control-row",
              tags$input(
                id = "template_color_picker",
                class = "color-picker-swatch",
                type = "color",
                value = "#0F7A6C",
                title = "Choose template color"
              ),
              textInput(
                inputId = "template_color",
                label = NULL,
                value = "#0F7A6C",
                placeholder = "#0F7A6C",
                width = "100%"
              )
            ),
            uiOutput("color_validation")
          ),

          div(
            class = "action-row",
            actionButton(
              inputId = "generate_certificates",
              label = "Generate PDF Certificates"
            ),
            actionButton(
              inputId = "update_preview",
              label = "Update Preview"
            )
          ),
          helpText(
            "Generate once, then use Update Preview after changing the template, color, text, logos, signatures, or fonts."
          )
        )
      ),

      column(
        width = 8,
        div(
          class = "workspace-card",
          h3("Participant Data"),
          tableOutput("excel_preview")
        ),
        div(
          class = "workspace-card",
          h3("PDF Preview"),
          p(
            class = "preview-copy",
            "Generate the certificates, inspect any participant, then update the preview after making changes."
          ),
          uiOutput("preview_status"),
          uiOutput("preview_selector_ui"),
          uiOutput("pdf_preview"),
          uiOutput("download_button_ui")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  excel_data <- reactive({
    req(input$excel_file)

    participant_data <- readxl::read_excel(input$excel_file$datapath)

    shiny::validate(
      shiny::need(
        ncol(participant_data) >= 1,
        "The Excel file must contain at least one column for Full Name."
      ),
      shiny::need(
        nrow(participant_data) > 0,
        "No participant records were found."
      )
    )

    participant_data
  })

  output$excel_preview <- renderTable({
    if (is.null(input$excel_file)) return(NULL)
    head(excel_data(), 10)
  })

  output$color_validation <- renderUI({
    if (!is_valid_hex(input$template_color)) {
      return(
        div(
          class = "hex-warning",
          "Please enter a valid HEX color, for example #0F7A6C."
        )
      )
    }

    NULL
  })

  preview_root <- file.path(
    tempdir(),
    paste0("certificate_preview_", session$token)
  )

  dir.create(preview_root, recursive = TRUE, showWarnings = FALSE)

  preview_prefix <- paste0(
    "certificate_preview_",
    gsub("[^A-Za-z0-9_-]", "", session$token)
  )

  addResourcePath(preview_prefix, preview_root)

  session$onSessionEnded(function() {
    try(removeResourcePath(preview_prefix), silent = TRUE)
    unlink(preview_root, recursive = TRUE, force = TRUE)
  })

  preview_state <- reactiveValues(
    generated = FALSE,
    stale = FALSE,
    building = FALSE,
    files = character(0),
    labels = character(0),
    zip_file = NULL,
    revision = 0L
  )

  preview_configuration <- reactive({
    list(
      excel = uploaded_file_fingerprint(input$excel_file),
      certificate_title = input$certificate_title,
      body_text = input$body_text,
      name_font_size = input$name_font_size,
      logo_count = input$logo_count,
      logo_image_1 = uploaded_file_fingerprint(input$logo_image_1),
      logo_position_1 = input$logo_position_1,
      logo_image_2 = uploaded_file_fingerprint(input$logo_image_2),
      logo_position_2 = input$logo_position_2,
      logo_width = input$logo_width,
      logo_height = input$logo_height,
      signature_count = input$signature_count,
      signature_image_1 = uploaded_file_fingerprint(input$signature_image_1),
      signature_text_1 = input$signature_text_1,
      signature_position_1 = input$signature_position_1,
      signature_image_2 = uploaded_file_fingerprint(input$signature_image_2),
      signature_text_2 = input$signature_text_2,
      signature_position_2 = input$signature_position_2,
      signature_width = input$signature_width,
      signature_height = input$signature_height,
      template = input$template,
      title_font_family = input$title_font_family,
      body_font_family = input$body_font_family,
      template_color = input$template_color
    )
  })

  observeEvent(
    preview_configuration(),
    {
      if (isTRUE(preview_state$generated) && !isTRUE(preview_state$building)) {
        preview_state$stale <- TRUE
      }
    },
    ignoreInit = TRUE
  )

  build_preview_batch <- function() {
    if (!is_certificate_template(input$template)) {
      stop("Please select a valid certificate template.")
    }

    # Resolve the SVG early so a missing deployed template gives a clear message.
    get_template_path(input$template)

    if (!is_valid_hex(input$template_color)) {
      stop("Template Color must be a valid HEX color.")
    }

    name_font_size <- suppressWarnings(as.numeric(input$name_font_size))

    if (!is.finite(name_font_size) || name_font_size < 10 || name_font_size > 48) {
      stop("{name} Font Size must be between 10 and 48 pt.")
    }

    participant_data <- excel_data()

    if (
      !contains_placeholder(input$certificate_title, "{name}") &&
      !contains_placeholder(input$body_text, "{name}")
    ) {
      stop("Use {name} in the Certificate Title or Certificate Body. Full Name is required for every certificate.")
    }

    uses_title <- any(c(
      contains_placeholder(input$certificate_title, "{title}"),
      contains_placeholder(input$body_text, "{title}"),
      contains_placeholder(input$signature_text_1, "{title}"),
      contains_placeholder(input$signature_text_2, "{title}")
    ))

    if (uses_title && ncol(participant_data) < 2) {
      stop("Your certificate uses {title}, but the uploaded Excel file has no second column.")
    }

    template_color <- normalize_hex(input$template_color)
    logo_count <- as.integer(input$logo_count)
    signature_count <- as.integer(input$signature_count)

    logo_positions <- input$logo_position_1
    if (logo_count == 2) {
      logo_positions <- c(logo_positions, input$logo_position_2)
    }

    signature_positions <- input$signature_position_1
    if (signature_count == 2) {
      signature_positions <- c(signature_positions, input$signature_position_2)
    }

    if (anyDuplicated(logo_positions)) {
      stop("Logo 1 and Logo 2 must use different positions.")
    }

    if (anyDuplicated(signature_positions)) {
      stop("Signature 1 and Signature 2 must use different positions.")
    }

    bottom_logo_positions <- logo_positions[grepl("^bottom_", logo_positions)]
    shared_bottom_positions <- intersect(bottom_logo_positions, signature_positions)

    if (length(shared_bottom_positions) > 0) {
      readable_position <- switch(
        shared_bottom_positions[[1]],
        bottom_left = "Bottom Left",
        bottom_center = "Bottom Center",
        bottom_right = "Bottom Right",
        shared_bottom_positions[[1]]
      )

      stop(
        paste0(
          "A logo and a signature cannot share the same bottom position (",
          readable_position,
          "). Please choose a different position."
        )
      )
    }

    logo_path_1 <- if (!is.null(input$logo_image_1)) input$logo_image_1$datapath else NULL
    logo_path_2 <- if (logo_count == 2 && !is.null(input$logo_image_2)) input$logo_image_2$datapath else NULL

    signature_path_1 <- if (!is.null(input$signature_image_1)) input$signature_image_1$datapath else NULL
    signature_path_2 <- if (signature_count == 2 && !is.null(input$signature_image_2)) input$signature_image_2$datapath else NULL

    preview_state$building <- TRUE
    on.exit({ preview_state$building <- FALSE }, add = TRUE)

    existing_items <- list.files(
      preview_root,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )

    if (length(existing_items) > 0) {
      unlink(existing_items, recursive = TRUE, force = TRUE)
    }

    generated_files <- character(0)
    participant_labels <- character(0)

    withProgress(
      message = "Generating certificate preview...",
      value = 0,
      {
        total_rows <- nrow(participant_data)

        for (row_number in seq_len(total_rows)) {
          participant_name <- as.character(participant_data[[1]][row_number])

          participant_title <- if (ncol(participant_data) >= 2) {
            as.character(participant_data[[2]][row_number])
          } else {
            ""
          }

          if (is.na(participant_name) || trimws(participant_name) == "") {
            incProgress(1 / total_rows)
            next
          }

          if (is.na(participant_title)) participant_title <- ""

          pdf_name <- sprintf(
            "%03d_Certificate_%s.pdf",
            row_number,
            safe_filename(participant_name)
          )

          output_file <- file.path(preview_root, pdf_name)

          create_certificate_pdf(
            participant_name = participant_name,
            participant_title = participant_title,
            certificate_title = input$certificate_title,
            body_text = input$body_text,
            logo_count = logo_count,
            logo_image_1 = logo_path_1,
            logo_position_1 = input$logo_position_1,
            logo_image_2 = logo_path_2,
            logo_position_2 = if (logo_count == 2) input$logo_position_2 else "top_right",
            logo_width_cm = input$logo_width,
            logo_height_cm = input$logo_height,
            signature_count = signature_count,
            signature_text_1 = input$signature_text_1,
            signature_image_1 = signature_path_1,
            signature_position_1 = input$signature_position_1,
            signature_text_2 = if (signature_count == 2) input$signature_text_2 else "",
            signature_image_2 = signature_path_2,
            signature_position_2 = if (signature_count == 2) input$signature_position_2 else "bottom_left",
            signature_width_cm = input$signature_width,
            signature_height_cm = input$signature_height,
            template = input$template,
            template_color = template_color,
            title_font_family = input$title_font_family,
            body_font_family = input$body_font_family,
            name_font_size = name_font_size,
            output_file = output_file
          )

          generated_files <- c(generated_files, pdf_name)
          participant_labels <- c(participant_labels, participant_name)

          incProgress(1 / total_rows)
        }
      }
    )

    if (length(generated_files) == 0) {
      stop("No certificates could be generated. Check the Full Name column.")
    }

    zip_path <- file.path(
      preview_root,
      paste0("Certificates_", Sys.Date(), ".zip")
    )

    zip::zipr(
      zipfile = zip_path,
      files = generated_files,
      root = preview_root
    )

    preview_state$files <- generated_files
    preview_state$labels <- participant_labels
    preview_state$zip_file <- zip_path
    preview_state$generated <- TRUE
    preview_state$stale <- FALSE
    preview_state$revision <- preview_state$revision + 1L

    showNotification(
      "Certificate preview generated successfully.",
      type = "message",
      duration = 3
    )
  }

  observeEvent(input$generate_certificates, {
    tryCatch(
      build_preview_batch(),
      error = function(error) {
        showNotification(
          conditionMessage(error),
          type = "error",
          duration = 8
        )
      }
    )
  })

  observeEvent(input$update_preview, {
    if (!isTRUE(preview_state$generated)) {
      showNotification(
        "Generate the first PDF preview before using Update Preview.",
        type = "warning",
        duration = 4
      )
      return(invisible(NULL))
    }

    tryCatch(
      build_preview_batch(),
      error = function(error) {
        showNotification(
          conditionMessage(error),
          type = "error",
          duration = 8
        )
      }
    )
  })

  output$preview_status <- renderUI({
    if (!isTRUE(preview_state$generated)) {
      return(div(class = "status-pill status-idle", "No preview generated yet"))
    }

    if (isTRUE(preview_state$stale)) {
      return(div(class = "status-pill status-stale", "Changes pending - click Update Preview"))
    }

    div(class = "status-pill status-ready", "Preview is up to date")
  })

  output$preview_selector_ui <- renderUI({
    if (!isTRUE(preview_state$generated)) return(NULL)

    choices <- stats::setNames(
      preview_state$files,
      preview_state$labels
    )

    selectInput(
      inputId = "preview_file",
      label = "Preview Participant",
      choices = choices,
      selected = preview_state$files[1]
    )
  })

  output$pdf_preview <- renderUI({
    if (!isTRUE(preview_state$generated)) {
      return(
        div(
          class = "preview-placeholder",
          div(
            strong("Your certificate preview will appear here."),
            span("Upload Excel, configure the certificate, and click Generate PDF Certificates.")
          )
        )
      )
    }

    selected_file <- input$preview_file

    if (is.null(selected_file) || !(selected_file %in% preview_state$files)) {
      selected_file <- preview_state$files[1]
    }

    preview_url <- paste0(
      "/",
      preview_prefix,
      "/",
      utils::URLencode(selected_file, reserved = TRUE),
      "?v=",
      preview_state$revision,
      "#toolbar=1&navpanes=0&scrollbar=1"
    )

    tags$iframe(
      class = "pdf-frame",
      src = preview_url,
      title = "Certificate PDF Preview"
    )
  })

  output$download_button_ui <- renderUI({
    if (!isTRUE(preview_state$generated) || isTRUE(preview_state$stale)) {
      return(NULL)
    }

    downloadButton(
      outputId = "download_certificates",
      label = "Download PDF Certificates (ZIP)"
    )
  })

  output$download_certificates <- downloadHandler(
    filename = function() {
      paste0("Certificates_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(
        preview_state$generated,
        !preview_state$stale,
        has_file(preview_state$zip_file)
      )

      file.copy(
        preview_state$zip_file,
        file,
        overwrite = TRUE
      )
    }
  )
}

shinyApp(ui = ui, server = server)
