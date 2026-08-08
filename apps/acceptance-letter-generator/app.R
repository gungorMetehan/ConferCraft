# ============================================================
# CONFERCRAFT - ACCEPTANCE LETTER GENERATOR
# Creator: Metehan Güngör
# Rich text: Markdown in the letter title, plus Markdown, bullets, and LaTeX-style mathematics in body/signatures
# Templates: eight named SVG backgrounds from the templates folder with one customizable template color
# PDF fonts: open-licensed Google Fonts loaded on demand with sysfonts/showtext
# ============================================================

library(shiny)
library(readxl)
library(zip)
library(grid)
library(png)
library(jpeg)
library(sysfonts)
library(showtext)

# Use showtext for PDF graphics so downloaded open-source fonts render consistently
# on local machines and cloud deployments without relying on proprietary system fonts.
showtext::showtext_auto(enable = TRUE)

# Open-licensed Google Fonts offered in the PDF font menus.
# The list is intentionally broad and alphabetized. These families provide Latin/Latin-Extended
# coverage suitable for Turkish characters such as ç, ğ, ı, İ, ö, ş, and ü.
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
  "Alegreya",
  "Bitter",
  "Cormorant Garamond",
  "Crimson Pro",
  "EB Garamond",
  "IBM Plex Serif",
  "Libre Baskerville",
  "Lora",
  "Merriweather",
  "Noto Serif",
  "Playfair Display",
  "Roboto Slab",
  "Source Serif 4",
  "Spectral",
  "Vollkorn"
)

# Cache resolved font names so each Google Font is downloaded at most once per R process.
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

  # font_add_google() uses curl and jsonlite internally. Keeping these
  # requireNamespace() calls explicit also lets rsconnect detect both
  # packages when it generates manifest.json for cloud deployment.
  google_font_dependencies_ready <-
    requireNamespace("curl", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)

  if (!font_family %in% loaded_families && google_font_dependencies_ready) {
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
# COLOR AND INPUT HELPERS
# These functions validate user input and prepare reusable values.
# ============================================================

# Validates a three- or six-digit HEX color such as #ABC or #7FB8A6.
is_valid_hex <- function(color) {
  color <- trimws(color)
  grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", color)
}

# Converts short HEX colors such as #ABC to the standard #AABBCC form.
normalize_hex <- function(color) {
  color <- toupper(trimws(color))

  if (!is_valid_hex(color)) {
    stop(paste(color, "is not a valid HEX color."))
  }

  if (nchar(color) == 4) {
    characters <- strsplit(substring(color, 2), "")[[1]]
    color <- paste0("#", paste0(characters, characters, collapse = ""))
  }

  color
}

# Adds transparency to a color while keeping the original HEX color unchanged.
alpha_color <- function(color, alpha = 1) {
  grDevices::adjustcolor(color, alpha.f = alpha)
}

# Returns TRUE when a text field contains visible content.
has_text <- function(text) {
  !is.null(text) && length(text) == 1 && !is.na(text) && nzchar(trimws(text))
}

# Returns TRUE when a file path exists and can be used by the PDF renderer.
has_file <- function(path) {
  !is.null(path) && length(path) == 1 && !is.na(path) && nzchar(path) && file.exists(path)
}

# Creates file-system-safe participant names for generated PDF filenames.
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

# Replaces the {name} and {paper} placeholders with Excel values.
fill_placeholders <- function(text, participant_name, paper_title) {
  text <- gsub("{name}", participant_name, text, fixed = TRUE)
  text <- gsub("{paper}", paper_title, text, fixed = TRUE)
  text
}

# Replaces placeholders while marking their inserted values for inline bold rendering.
# The markers are internal only and are removed by the mixed-style PDF text renderer.
fill_placeholders_bold <- function(text, participant_name, paper_title) {
  text <- gsub(
    "{name}",
    paste0("[[B]]", participant_name, "[[/B]]"),
    text,
    fixed = TRUE
  )

  text <- gsub(
    "{paper}",
    paste0("[[B]]", paper_title, "[[/B]]"),
    text,
    fixed = TRUE
  )

  text
}

# Reads PNG and JPEG images for logos and signatures.
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


# Returns the first existing path from a list of candidate file locations.
get_first_existing_path <- function(paths) {
  existing_paths <- paths[file.exists(paths)]

  if (length(existing_paths) == 0) {
    return(NA_character_)
  }

  existing_paths[[1]]
}


# ============================================================
# TEXT LAYOUT HELPERS
# These functions calculate line wrapping before text is drawn on a PDF page.
# ============================================================

# Measures the width of a text string on the active PDF graphics device.
measure_text_width <- function(
    text,
    font_size = 11,
    font_family = "sans",
    font_face = "plain") {

  text_grob <- textGrob(
    text,
    gp = gpar(
      fontsize = font_size,
      fontfamily = font_family,
      fontface = font_face
    )
  )

  convertWidth(
    grobWidth(text_grob),
    unitTo = "inches",
    valueOnly = TRUE
  )
}

# Wraps a paragraph into multiple lines so it stays inside the content area.
wrap_paragraph <- function(
    paragraph,
    maximum_width,
    font_size = 11,
    font_family = "sans",
    font_face = "plain") {

  paragraph <- trimws(paragraph)

  if (nchar(paragraph) == 0) {
    return("")
  }

  words <- strsplit(paragraph, "\\s+")[[1]]

  if (length(words) == 1) {
    return(words)
  }

  lines <- character(0)
  current_line <- words[1]

  for (word in words[-1]) {
    candidate_line <- paste(current_line, word)

    candidate_width <- measure_text_width(
      text = candidate_line,
      font_size = font_size,
      font_family = font_family,
      font_face = font_face
    )

    if (candidate_width <= maximum_width) {
      current_line <- candidate_line
    } else {
      lines <- c(lines, current_line)
      current_line <- word
    }
  }

  c(lines, current_line)
}

# Estimates how much vertical space a multiline text block will need.
estimate_multiline_text_height <- function(
    text,
    text_width,
    font_size = 10.5,
    font_family = "sans",
    font_face = "plain",
    line_spacing = 1.25,
    paragraph_spacing = 0.04) {

  if (!has_text(text)) {
    return(0)
  }

  paragraphs <- strsplit(text, "\n", fixed = TRUE)[[1]]
  line_height <- (font_size / 72) * line_spacing
  total_height <- 0

  for (paragraph in paragraphs) {
    if (trimws(paragraph) == "") {
      total_height <- total_height + line_height
    } else {
      wrapped_lines <- wrap_paragraph(
        paragraph = paragraph,
        maximum_width = text_width,
        font_size = font_size,
        font_family = font_family,
        font_face = font_face
      )

      total_height <- total_height +
        length(wrapped_lines) * line_height +
        paragraph_spacing
    }
  }

  total_height
}


# ============================================================
# IMAGE HELPERS
# These functions place uploaded logos and signatures on the PDF.
# ============================================================

# Draws a logo while preserving its original aspect ratio.
draw_logo <- function(
    logo_path,
    x_position,
    y_position,
    maximum_width = 1.45,
    maximum_height = 0.85,
    horizontal_justification = "right") {

  if (!has_file(logo_path)) {
    return(invisible(NULL))
  }

  logo <- read_raster_image(logo_path)
  logo_dimensions <- dim(logo)
  aspect_ratio <- logo_dimensions[2] / logo_dimensions[1]

  logo_width <- maximum_width
  logo_height <- logo_width / aspect_ratio

  if (logo_height > maximum_height) {
    logo_height <- maximum_height
    logo_width <- logo_height * aspect_ratio
  }

  grid.raster(
    image = logo,
    x = unit(x_position, "inches"),
    y = unit(y_position, "inches"),
    width = unit(logo_width, "inches"),
    height = unit(logo_height, "inches"),
    just = c(horizontal_justification, "top"),
    interpolate = TRUE
  )
}


# ============================================================
# PAGE BACKGROUND
# Every template starts with a clean white A4 background.
# ============================================================

# Paints a white page before template graphics and document content are added.
draw_white_background <- function() {
  grid.rect(
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    gp = gpar(fill = "white", col = NA)
  )
}


# ============================================================
# SVG TEMPLATES
# ConferCraft currently offers eight named SVG backgrounds stored under /templates.
# The selected template is recolored from the single Template Color control.
# ============================================================

render_svg_to_raster <- function(svg_path, width = 1240, height = 1754) {
  if (!has_file(svg_path)) {
    return(NULL)
  }

  if (requireNamespace("rsvg", quietly = TRUE)) {
    temporary_png <- tempfile(fileext = ".png")
    rsvg::rsvg_png(
      svg = svg_path,
      file = temporary_png,
      width = width,
      height = height
    )
    return(png::readPNG(temporary_png))
  }

  if (requireNamespace("magick", quietly = TRUE)) {
    svg_image <- magick::image_read(svg_path)
    raster_png <- magick::image_convert(svg_image, format = "png")
    temporary_png <- tempfile(fileext = ".png")
    magick::image_write(raster_png, path = temporary_png, format = "png")
    return(png::readPNG(temporary_png))
  }

  NULL
}

# The internal template IDs match the SVG filenames stored under /templates.
svg_template_ids <- c(
  "linear_horizon",
  "contour_flow",
  "diamond_edge",
  "watercolor_bloom",
  "canvas_wash",
  "silken_waves",
  "prism_dots",
  "origami_fold"
)

# Returns TRUE only for the eight template IDs exposed by the UI.
is_svg_template <- function(template_name) {
  is.character(template_name) &&
    length(template_name) == 1 &&
    !is.na(template_name) &&
    template_name %in% svg_template_ids
}

# Resolves templates/<template_name>.svg.
# Upper-case .SVG is also accepted so deployment is not fragile on case-sensitive systems.
get_template_svg_path <- function(template_name) {
  if (!is_svg_template(template_name)) {
    return(NA_character_)
  }

  candidate_paths <- c(
    file.path(getwd(), "templates", paste0(template_name, ".svg")),
    file.path(getwd(), "templates", paste0(template_name, ".SVG"))
  )

  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) == 0) {
    return(NA_character_)
  }

  existing_paths[[1]]
}

# Recolors the artwork of any supplied template while preserving white space,
# transparency, antialiasing, and the source template's light/dark hierarchy.
# This works across the original teal SVG as well as blue, pink, and purple designs.
recolor_template_artwork <- function(image, primary_color) {
  image_dimensions <- dim(image)

  if (is.null(image_dimensions) || length(image_dimensions) != 3 || image_dimensions[3] < 3) {
    return(image)
  }

  target_rgb <- as.numeric(grDevices::col2rgb(primary_color)) / 255

  red_channel <- image[, , 1]
  green_channel <- image[, , 2]
  blue_channel <- image[, , 3]

  # Distance from pure white captures both saturated artwork and pale/gray tinted artwork.
  # Near-white pixels remain untouched so the document's white writing area stays white.
  ink_strength <- pmax(
    1 - red_channel,
    1 - green_channel,
    1 - blue_channel
  )

  chroma <- pmax(red_channel, green_channel, blue_channel) -
    pmin(red_channel, green_channel, blue_channel)

  # Preserve neutral near-white page backgrounds such as #F8F8F8, while still
  # recoloring darker gray waves/borders and all genuinely colored artwork.
  neutral_near_white <-
    chroma < 0.02 &
    red_channel > 0.90 &
    green_channel > 0.90 &
    blue_channel > 0.90

  artwork_mask <- ink_strength > 0.012 & !neutral_near_white

  if (!any(artwork_mask)) {
    return(image)
  }

  # Normalize against the strongest visible source artwork. This maps the darkest source
  # accent close to the user's selected color while keeping pale washes and gradients pale.
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
    channel[artwork_mask] <- 1 - tint_strength[artwork_mask] * (1 - target_rgb[channel_index])
    recolored_image[, , channel_index] <- channel
  }

  recolored_image
}

# A neutral fallback keeps PDF generation alive if an SVG file is missing or cannot be rendered.
draw_svg_template_fallback <- function(primary_color) {
  draw_white_background()

  grid.rect(
    width = unit(0.94, "npc"),
    height = unit(0.94, "npc"),
    gp = gpar(
      fill = NA,
      col = alpha_color(primary_color, 0.30),
      lwd = 1.1
    )
  )
}

# Loads the selected SVG from the templates folder, recolors it, and paints it full-page.
draw_svg_template <- function(template_name, primary_color) {
  svg_path <- get_template_svg_path(template_name)

  if (!is.na(svg_path)) {
    background_raster <- tryCatch(
      render_svg_to_raster(svg_path),
      error = function(error) NULL
    )

    if (!is.null(background_raster)) {
      background_raster <- recolor_template_artwork(
        image = background_raster,
        primary_color = primary_color
      )

      draw_white_background()
      grid.raster(
        image = background_raster,
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        width = unit(1, "npc"),
        height = unit(1, "npc"),
        interpolate = TRUE
      )
      return(invisible(TRUE))
    }
  }

  draw_svg_template_fallback(primary_color)
  invisible(FALSE)
}

# ============================================================
# TEMPLATE DISPATCHER
# The eight named SVG files are the selectable PDF backgrounds.
# ============================================================

draw_page_template <- function(template_name, primary_color, secondary_color) {
  if (!is_svg_template(template_name)) {
    template_name <- "linear_horizon"
  }

  draw_svg_template(
    template_name = template_name,
    primary_color = primary_color
  )
}


# ============================================================
# OPTIONAL CONGRESS BRANDING
# The same optional congress title and logo can be used with both templates.
# ============================================================

draw_standard_branding_panel <- function(
    header_caption,
    logo_image,
    primary_color,
    secondary_color,
    page_width,
    left_margin,
    right_margin,
    title_font_family = "IBM Plex Sans",
    template_name = "linear_horizon") {

  caption_exists <- has_text(header_caption)
  logo_exists <- has_file(logo_image)

  if (!caption_exists && !logo_exists) {
    return(FALSE)
  }

  if (is_svg_template(template_name)) {
    reserved_right_space <- 1.70
    caption_x <- if (logo_exists) left_margin + 1.25 else left_margin + 0.18
    caption_width <- page_width - caption_x - right_margin - reserved_right_space
    caption_font_size <- 10.5

    if (caption_exists) {
      caption_lines <- wrap_paragraph(
        paragraph = header_caption,
        maximum_width = caption_width,
        font_size = caption_font_size,
        font_family = title_font_family,
        font_face = "bold"
      )

      if (length(caption_lines) > 3) {
        caption_font_size <- 9.25
        caption_lines <- wrap_paragraph(
          paragraph = header_caption,
          maximum_width = caption_width,
          font_size = caption_font_size,
          font_family = title_font_family,
          font_face = "bold"
        )
      }

      caption_y <- 10.93

      for (caption_line in caption_lines) {
        grid.text(
          label = caption_line,
          x = unit(caption_x, "inches"),
          y = unit(caption_y, "inches"),
          just = c("left", "top"),
          gp = gpar(
            fontsize = caption_font_size,
            fontfamily = title_font_family,
            fontface = "bold",
            col = "#123A41"
          )
        )

        caption_y <- caption_y - 0.18
      }
    }

    if (logo_exists) {
      draw_logo(
        logo_path = logo_image,
        x_position = left_margin + 0.16,
        y_position = 11.00,
        maximum_width = 0.92,
        maximum_height = 0.70,
        horizontal_justification = "left"
      )
    }

    return(TRUE)
  }

  panel_width <- page_width - left_margin - right_margin
  panel_center_y <- 10.30
  panel_height <- 0.95

  grid.roundrect(
    x = unit(page_width / 2, "inches"),
    y = unit(panel_center_y, "inches"),
    width = unit(panel_width, "inches"),
    height = unit(panel_height, "inches"),
    r = unit(0.08, "inches"),
    gp = gpar(
      fill = alpha_color("#FFFFFF", 0.97),
      col = alpha_color(primary_color, 0.18),
      lwd = 0.9
    )
  )

  grid.lines(
    x = unit(
      c(left_margin + 0.18, left_margin + 0.78),
      "inches"
    ),
    y = unit(c(9.90, 9.90), "inches"),
    gp = gpar(col = secondary_color, lwd = 2.4)
  )

  if (caption_exists) {
    caption_width <- if (logo_exists) {
      panel_width - 1.90
    } else {
      panel_width - 0.40
    }

    caption_font_size <- 9.5

    caption_lines <- wrap_paragraph(
      paragraph = header_caption,
      maximum_width = caption_width,
      font_size = caption_font_size,
      font_family = title_font_family,
      font_face = "bold"
    )

    if (length(caption_lines) > 3) {
      caption_font_size <- 8.5
      caption_lines <- wrap_paragraph(
        paragraph = header_caption,
        maximum_width = caption_width,
        font_size = caption_font_size,
        font_family = title_font_family,
        font_face = "bold"
      )
    }

    caption_y <- 10.58

    for (caption_line in caption_lines) {
      grid.text(
        label = caption_line,
        x = unit(left_margin + 0.18, "inches"),
        y = unit(caption_y, "inches"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = caption_font_size,
          fontfamily = title_font_family,
          fontface = "bold",
          col = "#000000"
        )
      )

      caption_y <- caption_y - 0.16
    }
  }

  if (logo_exists) {
    draw_logo(
      logo_path = logo_image,
      x_position = page_width - right_margin - 0.15,
      y_position = 10.62,
      maximum_width = 1.25,
      maximum_height = 0.58,
      horizontal_justification = "right"
    )
  }

  TRUE
}



# ============================================================
# MULTILINE TEXT RENDERER
# Draws paragraphs line by line and automatically creates a new page if needed.
# ============================================================

draw_multiline_text <- function(
    text,
    x_position,
    y_position,
    text_width,
    font_size = 11,
    font_family = "sans",
    font_face = "plain",
    font_color = "#000000",
    line_spacing = 1.4,
    paragraph_spacing = 0.12,
    page_height = 11.69,
    top_margin = 1.45,
    bottom_margin = 1.05,
    page_decorator = NULL) {

  paragraphs <- strsplit(text, "\n", fixed = TRUE)[[1]]
  line_height <- (font_size / 72) * line_spacing
  current_y <- y_position

  for (paragraph in paragraphs) {
    if (trimws(paragraph) == "") {
      current_y <- current_y - line_height
      next
    }

    wrapped_lines <- wrap_paragraph(
      paragraph = paragraph,
      maximum_width = text_width,
      font_size = font_size,
      font_family = font_family,
      font_face = font_face
    )

    for (line in wrapped_lines) {
      if (current_y - line_height < bottom_margin) {
        grid.newpage()

        if (is.function(page_decorator)) {
          page_decorator()
        }

        current_y <- page_height - top_margin
      }

      grid.text(
        label = line,
        x = unit(x_position, "inches"),
        y = unit(current_y, "inches"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = font_size,
          fontfamily = font_family,
          fontface = font_face,
          col = font_color
        )
      )

      current_y <- current_y - line_height
    }

    current_y <- current_y - paragraph_spacing
  }

  current_y
}

# Parses internal [[B]]...[[/B]] markers into plain and bold text segments.
parse_inline_bold_segments <- function(text) {
  start_marker <- "[[B]]"
  end_marker <- "[[/B]]"
  remaining <- text
  is_bold <- FALSE
  segments <- list()

  while (nchar(remaining) > 0) {
    marker <- if (is_bold) end_marker else start_marker
    marker_position <- regexpr(marker, remaining, fixed = TRUE)[1]

    if (marker_position == -1) {
      segments[[length(segments) + 1]] <- list(
        text = remaining,
        face = if (is_bold) "bold" else "plain"
      )
      break
    }

    if (marker_position > 1) {
      segments[[length(segments) + 1]] <- list(
        text = substr(remaining, 1, marker_position - 1),
        face = if (is_bold) "bold" else "plain"
      )
    }

    remaining <- substring(
      remaining,
      marker_position + nchar(marker)
    )
    is_bold <- !is_bold
  }

  segments
}

# Converts styled segments into whitespace/word tokens so wrapping can preserve bold placeholders.
tokenize_inline_bold <- function(text) {
  segments <- parse_inline_bold_segments(text)
  tokens <- list()

  for (segment in segments) {
    if (!nzchar(segment$text)) {
      next
    }

    token_locations <- gregexpr("\\s+|\\S+", segment$text, perl = TRUE)[[1]]

    if (length(token_locations) == 1 && token_locations[1] == -1) {
      next
    }

    token_values <- regmatches(segment$text, list(token_locations))[[1]]

    for (token_value in token_values) {
      tokens[[length(tokens) + 1]] <- list(
        text = token_value,
        face = segment$face
      )
    }
  }

  tokens
}

# Wraps a paragraph while respecting the different widths of plain and bold text.
wrap_inline_bold_paragraph <- function(
    paragraph,
    maximum_width,
    font_size = 11,
    font_family = "sans") {

  tokens <- tokenize_inline_bold(paragraph)

  if (length(tokens) == 0) {
    return(list())
  }

  lines <- list()
  current_line <- list()
  current_width <- 0

  append_current_line <- function() {
    current_line
  }

  for (token in tokens) {
    token_is_space <- grepl("^\\s+$", token$text, perl = TRUE)

    if (token_is_space && length(current_line) == 0) {
      next
    }

    token_width <- measure_text_width(
      text = token$text,
      font_size = font_size,
      font_family = font_family,
      font_face = token$face
    )

    if (
      !token_is_space &&
      length(current_line) > 0 &&
      current_width + token_width > maximum_width
    ) {
      # Remove trailing whitespace before closing the line.
      while (
        length(current_line) > 0 &&
        grepl("^\\s+$", current_line[[length(current_line)]]$text, perl = TRUE)
      ) {
        current_width <- current_width - current_line[[length(current_line)]]$width
        current_line <- current_line[-length(current_line)]
      }

      lines[[length(lines) + 1]] <- append_current_line()
      current_line <- list()
      current_width <- 0
    }

    if (token_is_space && current_width + token_width > maximum_width) {
      if (length(current_line) > 0) {
        lines[[length(lines) + 1]] <- append_current_line()
      }
      current_line <- list()
      current_width <- 0
      next
    }

    token$width <- token_width
    current_line[[length(current_line) + 1]] <- token
    current_width <- current_width + token_width
  }

  while (
    length(current_line) > 0 &&
    grepl("^\\s+$", current_line[[length(current_line)]]$text, perl = TRUE)
  ) {
    current_line <- current_line[-length(current_line)]
  }

  if (length(current_line) > 0) {
    lines[[length(lines) + 1]] <- current_line
  }

  lines
}

# Draws multiline text with bold placeholder values without requiring an additional R package.
draw_multiline_text_with_bold_placeholders <- function(
    text,
    x_position,
    y_position,
    text_width,
    font_size = 11,
    font_family = "sans",
    font_color = "#000000",
    line_spacing = 1.4,
    paragraph_spacing = 0.12,
    page_height = 11.69,
    top_margin = 1.45,
    bottom_margin = 1.05,
    page_decorator = NULL) {

  paragraphs <- strsplit(text, "\n", fixed = TRUE)[[1]]
  line_height <- (font_size / 72) * line_spacing
  current_y <- y_position

  for (paragraph in paragraphs) {
    if (trimws(gsub("\\[\\[/?B\\]\\]", "", paragraph)) == "") {
      current_y <- current_y - line_height
      next
    }

    wrapped_lines <- wrap_inline_bold_paragraph(
      paragraph = paragraph,
      maximum_width = text_width,
      font_size = font_size,
      font_family = font_family
    )

    for (line_tokens in wrapped_lines) {
      if (current_y - line_height < bottom_margin) {
        grid.newpage()

        if (is.function(page_decorator)) {
          page_decorator()
        }

        current_y <- page_height - top_margin
      }

      current_x <- x_position

      for (token in line_tokens) {
        grid.text(
          label = token$text,
          x = unit(current_x, "inches"),
          y = unit(current_y, "inches"),
          just = c("left", "top"),
          gp = gpar(
            fontsize = font_size,
            fontfamily = font_family,
            fontface = token$face,
            col = font_color
          )
        )

        current_x <- current_x + token$width
      }

      current_y <- current_y - line_height
    }

    current_y <- current_y - paragraph_spacing
  }

  current_y
}


# ============================================================
# RICH TEXT / MARKDOWN / MATH HELPERS
# Body Text and Signature Text support a controlled Markdown subset:
# **bold**, *italic*, ***bold italic***, bullet lines, $inline math$, and $$display math$$.
# LaTeX math uses latex2exp when that optional package is installed; otherwise a
# lightweight plotmath fallback handles common expressions and Greek symbols.
# ============================================================

# Combines a base face with Markdown states while preserving bold signatures.
combine_font_face <- function(base_face = "plain", bold = FALSE, italic = FALSE) {
  base_bold <- base_face %in% c("bold", "bold.italic", "bolditalic")
  base_italic <- base_face %in% c("italic", "bold.italic", "bolditalic")

  final_bold <- base_bold || bold
  final_italic <- base_italic || italic

  if (final_bold && final_italic) {
    return("bold.italic")
  }

  if (final_bold) {
    return("bold")
  }

  if (final_italic) {
    return("italic")
  }

  "plain"
}

# Converts a small, common subset of LaTeX to plotmath when latex2exp is unavailable.
# The optional latex2exp package is used automatically when present and supports
# substantially more LaTeX syntax than this fallback.
latex_math_expression <- function(latex_text) {
  latex_text <- trimws(latex_text)

  if (!nzchar(latex_text)) {
    return(NULL)
  }

  if (requireNamespace("latex2exp", quietly = TRUE)) {
    converted <- try(
      latex2exp::TeX(paste0("$", latex_text, "$")),
      silent = TRUE
    )

    if (!inherits(converted, "try-error")) {
      return(converted)
    }
  }

  fallback <- latex_text
  fallback <- gsub("\\\\left|\\\\right", "", fallback, perl = TRUE)

  greek_names <- c(
    "alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta",
    "eta", "theta", "vartheta", "iota", "kappa", "lambda", "mu", "nu",
    "xi", "pi", "varpi", "rho", "varrho", "sigma", "varsigma", "tau",
    "upsilon", "phi", "varphi", "chi", "psi", "omega",
    "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon",
    "Phi", "Psi", "Omega"
  )

  for (greek_name in greek_names) {
    fallback <- gsub(
      paste0("\\\\", greek_name, "(?![A-Za-z])"),
      greek_name,
      fallback,
      perl = TRUE
    )
  }

  fallback <- gsub("\\\\infty", "infinity", fallback, perl = TRUE)
  fallback <- gsub("\\\\leq?", "<=", fallback, perl = TRUE)
  fallback <- gsub("\\\\geq?", ">=", fallback, perl = TRUE)
  fallback <- gsub("\\\\neq", "!=", fallback, perl = TRUE)
  fallback <- gsub("\\\\cdot", " %*% ", fallback, perl = TRUE)
  fallback <- gsub("\\\\times", " %*% ", fallback, perl = TRUE)

  # Common single-level constructs. More complex/nested LaTeX is handled best by latex2exp.
  for (i in seq_len(8)) {
    previous <- fallback
    fallback <- gsub(
      "\\\\frac\\{([^{}]+)\\}\\{([^{}]+)\\}",
      "frac(\\1,\\2)",
      fallback,
      perl = TRUE
    )
    fallback <- gsub(
      "\\\\sqrt\\{([^{}]+)\\}",
      "sqrt(\\1)",
      fallback,
      perl = TRUE
    )
    fallback <- gsub(
      "\\\\bar\\{([^{}]+)\\}",
      "bar(\\1)",
      fallback,
      perl = TRUE
    )
    fallback <- gsub(
      "\\\\hat\\{([^{}]+)\\}",
      "hat(\\1)",
      fallback,
      perl = TRUE
    )
    fallback <- gsub("\\^\\{([^{}]+)\\}", "^(\\1)", fallback, perl = TRUE)

    if (identical(previous, fallback)) {
      break
    }
  }

  # Plotmath treats == as an equality relation; convert a simple LaTeX/plain equals sign.
  fallback <- gsub("(?<![<>=!])=(?!=)", "==", fallback, perl = TRUE)

  parsed <- try(parse(text = fallback)[[1]], silent = TRUE)

  if (inherits(parsed, "try-error")) {
    return(NULL)
  }

  parsed
}

# Measures either styled text or a math expression on the active PDF device.
measure_rich_token <- function(
    token,
    font_size = 11,
    font_family = "sans") {

  if (identical(token$type, "math") && !is.null(token$expression)) {
    math_grob <- textGrob(
      token$expression,
      gp = gpar(
        fontsize = font_size,
        fontfamily = font_family
      )
    )

    return(list(
      width = convertWidth(grobWidth(math_grob), "inches", valueOnly = TRUE),
      height = convertHeight(grobHeight(math_grob), "inches", valueOnly = TRUE)
    ))
  }

  text_grob <- textGrob(
    token$text,
    gp = gpar(
      fontsize = font_size,
      fontfamily = font_family,
      fontface = token$face
    )
  )

  list(
    width = convertWidth(grobWidth(text_grob), "inches", valueOnly = TRUE),
    height = convertHeight(grobHeight(text_grob), "inches", valueOnly = TRUE)
  )
}

# Parses one line into text/math segments while respecting Markdown and internal
# [[B]] placeholder markers used to keep {name} and {paper} bold by default.
parse_rich_inline_segments <- function(text, base_face = "plain") {
  segments <- list()
  buffer <- ""
  markdown_bold <- FALSE
  markdown_italic <- FALSE
  forced_bold <- FALSE
  position <- 1L
  text_length <- nchar(text)

  current_face <- function() {
    combine_font_face(
      base_face = base_face,
      bold = markdown_bold || forced_bold,
      italic = markdown_italic
    )
  }

  flush_buffer <- function() {
    if (nzchar(buffer)) {
      segments[[length(segments) + 1L]] <<- list(
        type = "text",
        text = buffer,
        face = current_face()
      )
      buffer <<- ""
    }
  }

  while (position <= text_length) {
    remaining <- substring(text, position)

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

    # Inline math uses a single pair of dollar signs. Display math is parsed as a block.
    if (startsWith(remaining, "$") && !startsWith(remaining, "$$")) {
      closing_relative <- regexpr("$", substring(text, position + 1L), fixed = TRUE)[1]

      if (closing_relative != -1) {
        closing_position <- position + closing_relative
        latex_text <- substring(text, position + 1L, closing_position - 1L)
        math_expression <- latex_math_expression(latex_text)

        if (!is.null(math_expression)) {
          flush_buffer()
          segments[[length(segments) + 1L]] <- list(
            type = "math",
            text = latex_text,
            expression = math_expression,
            face = current_face()
          )
          position <- closing_position + 1L
          next
        }
      }
    }

    buffer <- paste0(buffer, substring(text, position, position))
    position <- position + 1L
  }

  flush_buffer()
  segments
}

# Converts styled segments into word/space tokens while preserving inline mathematics.
tokenize_rich_inline <- function(text, base_face = "plain") {
  segments <- parse_rich_inline_segments(text, base_face = base_face)
  tokens <- list()

  for (segment in segments) {
    if (identical(segment$type, "math")) {
      tokens[[length(tokens) + 1L]] <- segment
      next
    }

    if (!nzchar(segment$text)) {
      next
    }

    token_locations <- gregexpr("\\s+|\\S+", segment$text, perl = TRUE)[[1]]

    if (length(token_locations) == 1L && token_locations[1] == -1L) {
      next
    }

    token_values <- regmatches(segment$text, list(token_locations))[[1]]

    for (token_value in token_values) {
      tokens[[length(tokens) + 1L]] <- list(
        type = "text",
        text = token_value,
        face = segment$face
      )
    }
  }

  tokens
}

# Wraps a rich-text paragraph while retaining font faces and inline math.
wrap_rich_paragraph <- function(
    paragraph,
    maximum_width,
    font_size = 11,
    font_family = "sans",
    base_face = "plain") {

  tokens <- tokenize_rich_inline(paragraph, base_face = base_face)

  if (length(tokens) == 0L) {
    return(list())
  }

  lines <- list()
  current_line <- list()
  current_width <- 0

  for (token in tokens) {
    token_is_space <- identical(token$type, "text") && grepl("^\\s+$", token$text, perl = TRUE)

    if (token_is_space && length(current_line) == 0L) {
      next
    }

    dimensions <- measure_rich_token(
      token = token,
      font_size = font_size,
      font_family = font_family
    )

    token$width <- dimensions$width
    token$height <- dimensions$height

    if (
      !token_is_space &&
      length(current_line) > 0L &&
      current_width + token$width > maximum_width
    ) {
      while (
        length(current_line) > 0L &&
        identical(current_line[[length(current_line)]]$type, "text") &&
        grepl("^\\s+$", current_line[[length(current_line)]]$text, perl = TRUE)
      ) {
        current_width <- current_width - current_line[[length(current_line)]]$width
        current_line <- current_line[-length(current_line)]
      }

      lines[[length(lines) + 1L]] <- current_line
      current_line <- list()
      current_width <- 0
    }

    if (token_is_space && current_width + token$width > maximum_width) {
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
    identical(current_line[[length(current_line)]]$type, "text") &&
    grepl("^\\s+$", current_line[[length(current_line)]]$text, perl = TRUE)
  ) {
    current_line <- current_line[-length(current_line)]
  }

  if (length(current_line) > 0L) {
    lines[[length(lines) + 1L]] <- current_line
  }

  lines
}

# Breaks a textarea into paragraphs, bullets, blank lines, and display-math blocks.
parse_rich_blocks <- function(text) {
  lines <- strsplit(text, "\\n", perl = TRUE)[[1]]
  blocks <- list()
  index <- 1L

  while (index <= length(lines)) {
    line <- lines[index]
    trimmed <- trimws(line)

    if (!nzchar(trimmed)) {
      blocks[[length(blocks) + 1L]] <- list(type = "blank")
      index <- index + 1L
      next
    }

    if (identical(trimmed, "$$")) {
      math_lines <- character(0)
      index <- index + 1L

      while (index <= length(lines) && !identical(trimws(lines[index]), "$$")) {
        math_lines <- c(math_lines, lines[index])
        index <- index + 1L
      }

      blocks[[length(blocks) + 1L]] <- list(
        type = "math_block",
        text = paste(math_lines, collapse = " ")
      )

      if (index <= length(lines) && identical(trimws(lines[index]), "$$")) {
        index <- index + 1L
      }

      next
    }

    if (grepl("^\\$\\$.+\\$\\$$", trimmed, perl = TRUE)) {
      blocks[[length(blocks) + 1L]] <- list(
        type = "math_block",
        text = sub("^\\$\\$(.*)\\$\\$$", "\\1", trimmed, perl = TRUE)
      )
      index <- index + 1L
      next
    }

    if (grepl("^[-+*]\\s+", trimmed, perl = TRUE)) {
      blocks[[length(blocks) + 1L]] <- list(
        type = "bullet",
        text = sub("^[-+*]\\s+", "", trimmed, perl = TRUE)
      )
      index <- index + 1L
      next
    }

    blocks[[length(blocks) + 1L]] <- list(
      type = "paragraph",
      text = line
    )
    index <- index + 1L
  }

  blocks
}

# Draws one wrapped rich-text line and returns its required line height.
draw_rich_line <- function(
    line_tokens,
    x_position,
    y_position,
    font_size,
    font_family,
    font_color,
    default_line_height) {

  current_x <- x_position
  token_heights <- default_line_height

  for (token in line_tokens) {
    if (identical(token$type, "math") && !is.null(token$expression)) {
      grid.text(
        label = token$expression,
        x = unit(current_x, "inches"),
        y = unit(y_position, "inches"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = font_size,
          fontfamily = font_family,
          col = font_color
        )
      )
    } else {
      grid.text(
        label = token$text,
        x = unit(current_x, "inches"),
        y = unit(y_position, "inches"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = font_size,
          fontfamily = font_family,
          fontface = token$face,
          col = font_color
        )
      )
    }

    current_x <- current_x + token$width
    token_heights <- c(token_heights, token$height)
  }

  max(token_heights, na.rm = TRUE)
}

# Main rich-text renderer used by Body Text and Signature Text.
draw_rich_text <- function(
    text,
    x_position,
    y_position,
    text_width,
    font_size = 11,
    font_family = "sans",
    base_face = "plain",
    font_color = "#000000",
    line_spacing = 1.4,
    paragraph_spacing = 0.12,
    page_height = 11.69,
    top_margin = 1.45,
    bottom_margin = 1.05,
    page_decorator = NULL) {

  blocks <- parse_rich_blocks(text)
  base_line_height <- (font_size / 72) * line_spacing
  current_y <- y_position
  bullet_indent <- 0.24

  ensure_space <- function(required_height) {
    if (current_y - required_height < bottom_margin) {
      grid.newpage()

      if (is.function(page_decorator)) {
        page_decorator()
      }

      current_y <<- page_height - top_margin
    }
  }

  for (block in blocks) {
    if (identical(block$type, "blank")) {
      ensure_space(base_line_height)
      current_y <- current_y - base_line_height
      next
    }

    if (identical(block$type, "math_block")) {
      expression_value <- latex_math_expression(block$text)

      if (is.null(expression_value)) {
        expression_value <- block$text
      }

      math_grob <- textGrob(
        expression_value,
        gp = gpar(
          fontsize = font_size + 1,
          fontfamily = font_family,
          col = font_color
        )
      )

      math_height <- max(
        base_line_height * 1.35,
        convertHeight(grobHeight(math_grob), "inches", valueOnly = TRUE) * 1.25
      )

      ensure_space(math_height + paragraph_spacing)

      grid.text(
        label = expression_value,
        x = unit(x_position + text_width / 2, "inches"),
        y = unit(current_y, "inches"),
        just = c("center", "top"),
        gp = gpar(
          fontsize = font_size + 1,
          fontfamily = font_family,
          col = font_color
        )
      )

      current_y <- current_y - math_height - paragraph_spacing
      next
    }

    is_bullet <- identical(block$type, "bullet")
    content_x <- if (is_bullet) x_position + bullet_indent else x_position
    content_width <- if (is_bullet) text_width - bullet_indent else text_width

    wrapped_lines <- wrap_rich_paragraph(
      paragraph = block$text,
      maximum_width = content_width,
      font_size = font_size,
      font_family = font_family,
      base_face = base_face
    )

    if (length(wrapped_lines) == 0L) {
      next
    }

    first_line <- TRUE

    for (line_tokens in wrapped_lines) {
      line_token_height <- max(
        c(base_line_height, vapply(line_tokens, function(token) token$height, numeric(1))),
        na.rm = TRUE
      )
      required_height <- max(base_line_height, line_token_height * 1.08)
      ensure_space(required_height)

      if (is_bullet && first_line) {
        grid.text(
          label = "\u2022",
          x = unit(x_position + 0.03, "inches"),
          y = unit(current_y, "inches"),
          just = c("left", "top"),
          gp = gpar(
            fontsize = font_size,
            fontfamily = font_family,
            fontface = combine_font_face(base_face, bold = TRUE),
            col = font_color
          )
        )
      }

      draw_rich_line(
        line_tokens = line_tokens,
        x_position = content_x,
        y_position = current_y,
        font_size = font_size,
        font_family = font_family,
        font_color = font_color,
        default_line_height = base_line_height
      )

      current_y <- current_y - required_height
      first_line <- FALSE
    }

    current_y <- current_y - paragraph_spacing
  }

  current_y
}

# Draws the letter title centered on the page while honoring inline Markdown.
# Plain text stays plain; **bold**, *italic*, and ***bold italic*** are optional.
draw_centered_rich_title <- function(
    text,
    center_x,
    y_position,
    text_width,
    font_size = 18,
    font_family = "serif",
    font_color = "#000000",
    line_spacing = 1.36) {

  blocks <- parse_rich_blocks(text)
  base_line_height <- (font_size / 72) * line_spacing
  current_y <- y_position

  for (block in blocks) {
    if (identical(block$type, "blank")) {
      current_y <- current_y - base_line_height
      next
    }

    if (identical(block$type, "math_block")) {
      expression_value <- latex_math_expression(block$text)

      if (is.null(expression_value)) {
        expression_value <- block$text
      }

      grid.text(
        label = expression_value,
        x = unit(center_x, "inches"),
        y = unit(current_y, "inches"),
        just = c("center", "top"),
        gp = gpar(
          fontsize = font_size,
          fontfamily = font_family,
          col = font_color
        )
      )

      current_y <- current_y - base_line_height
      next
    }

    wrapped_lines <- wrap_rich_paragraph(
      paragraph = block$text,
      maximum_width = text_width,
      font_size = font_size,
      font_family = font_family,
      base_face = "plain"
    )

    for (line_tokens in wrapped_lines) {
      line_width <- sum(
        vapply(line_tokens, function(token) token$width, numeric(1))
      )

      line_token_height <- max(
        c(base_line_height, vapply(line_tokens, function(token) token$height, numeric(1))),
        na.rm = TRUE
      )
      required_height <- max(base_line_height, line_token_height * 1.08)

      draw_rich_line(
        line_tokens = line_tokens,
        x_position = center_x - line_width / 2,
        y_position = current_y,
        font_size = font_size,
        font_family = font_family,
        font_color = font_color,
        default_line_height = base_line_height
      )

      current_y <- current_y - required_height
    }
  }

  current_y
}


# Estimates rich-text height for signature placement before the signature is drawn.
estimate_rich_text_height <- function(
    text,
    text_width,
    font_size = 10.5,
    font_family = "sans",
    base_face = "bold",
    line_spacing = 1.25,
    paragraph_spacing = 0.04) {

  if (!has_text(text)) {
    return(0)
  }

  blocks <- parse_rich_blocks(text)
  base_line_height <- (font_size / 72) * line_spacing
  total_height <- 0
  bullet_indent <- 0.24

  for (block in blocks) {
    if (identical(block$type, "blank")) {
      total_height <- total_height + base_line_height
      next
    }

    if (identical(block$type, "math_block")) {
      expression_value <- latex_math_expression(block$text)

      if (is.null(expression_value)) {
        expression_value <- block$text
      }

      math_grob <- textGrob(
        expression_value,
        gp = gpar(
          fontsize = font_size + 1,
          fontfamily = font_family
        )
      )

      math_height <- max(
        base_line_height * 1.35,
        convertHeight(grobHeight(math_grob), "inches", valueOnly = TRUE) * 1.25
      )

      total_height <- total_height + math_height + paragraph_spacing
      next
    }

    is_bullet <- identical(block$type, "bullet")
    content_width <- if (is_bullet) text_width - bullet_indent else text_width

    wrapped_lines <- wrap_rich_paragraph(
      paragraph = block$text,
      maximum_width = content_width,
      font_size = font_size,
      font_family = font_family,
      base_face = base_face
    )

    if (length(wrapped_lines) == 0L) {
      next
    }

    for (line_tokens in wrapped_lines) {
      line_token_height <- max(
        c(base_line_height, vapply(line_tokens, function(token) token$height, numeric(1))),
        na.rm = TRUE
      )
      total_height <- total_height + max(base_line_height, line_token_height * 1.08)
    }

    total_height <- total_height + paragraph_spacing
  }

  total_height
}


# ============================================================
# SIGNATURE HELPERS
# These functions size, estimate, and draw one reusable signature block.
# ============================================================

# Scales a signature image to fit its available column without distortion.
get_signature_dimensions <- function(
    signature_width_cm,
    signature_height_cm,
    available_width) {

  signature_width <- signature_width_cm / 2.54
  signature_height <- signature_height_cm / 2.54

  if (signature_width > available_width) {
    scale_factor <- available_width / signature_width
    signature_width <- signature_width * scale_factor
    signature_height <- signature_height * scale_factor
  }

  list(
    width = signature_width,
    height = signature_height
  )
}

# Estimates the full height of a signature image plus its accompanying text.
estimate_signature_block_height <- function(
    signature_text,
    signature_image,
    signature_width_cm,
    signature_height_cm,
    block_width,
    font_family = "IBM Plex Sans") {

  dimensions <- get_signature_dimensions(
    signature_width_cm = signature_width_cm,
    signature_height_cm = signature_height_cm,
    available_width = block_width
  )

  image_height <- 0

  if (has_file(signature_image)) {
    image_height <- dimensions$height + 0.10
  }

  text_height <- estimate_rich_text_height(
    text = signature_text,
    text_width = block_width,
    font_size = 10.5,
    font_family = font_family,
    base_face = "bold",
    line_spacing = 1.25,
    paragraph_spacing = 0.04
  )

  image_height + text_height
}

# Draws one signature block at the requested x/y coordinate.
draw_signature_block <- function(
    signature_text,
    signature_image,
    x_position,
    y_position,
    block_width,
    signature_width_cm,
    signature_height_cm,
    text_color,
    font_family = "IBM Plex Sans") {

  current_y <- y_position

  dimensions <- get_signature_dimensions(
    signature_width_cm = signature_width_cm,
    signature_height_cm = signature_height_cm,
    available_width = block_width
  )

  if (has_file(signature_image)) {
    signature_raster <- read_raster_image(signature_image)

    grid.raster(
      image = signature_raster,
      x = unit(x_position, "inches"),
      y = unit(current_y, "inches"),
      width = unit(dimensions$width, "inches"),
      height = unit(dimensions$height, "inches"),
      just = c("left", "top"),
      interpolate = TRUE
    )

    current_y <- current_y - dimensions$height - 0.10
  }

  draw_rich_text(
    text = signature_text,
    x_position = x_position,
    y_position = current_y,
    text_width = block_width,
    font_size = 10.5,
    font_family = font_family,
    base_face = "bold",
    font_color = text_color,
    line_spacing = 1.25,
    paragraph_spacing = 0.04,
    bottom_margin = -100
  )
}


# ============================================================
# OPTIONAL FOOTER NOTE
# Draws the optional third Excel column as a small gray note at the bottom-right of every page.
# ============================================================

draw_footer_note <- function(
    footer_text,
    page_width,
    right_margin,
    font_family = "IBM Plex Sans") {

  if (!has_text(footer_text)) {
    return(invisible(NULL))
  }

  footer_font_size <- 7.5
  footer_width <- 3.35
  footer_line_height <- (footer_font_size / 72) * 1.18
  footer_x <- page_width - max(right_margin, 1.30)
  footer_bottom_y <- 0.14

  footer_lines <- wrap_paragraph(
    paragraph = footer_text,
    maximum_width = footer_width,
    font_size = footer_font_size,
    font_family = font_family,
    font_face = "plain"
  )

  # Build upward from the lower part of the bottom template band.
  footer_top_y <- footer_bottom_y + length(footer_lines) * footer_line_height

  for (footer_line in footer_lines) {
    grid.text(
      label = footer_line,
      x = unit(footer_x, "inches"),
      y = unit(footer_top_y, "inches"),
      just = c("right", "top"),
      gp = gpar(
        fontsize = footer_font_size,
        fontfamily = font_family,
        fontface = "plain",
        col = "#7A7F7D"
      )
    )

    footer_top_y <- footer_top_y - footer_line_height
  }

  invisible(NULL)
}


# ============================================================
# PDF LETTER GENERATOR
# Creates one complete acceptance letter for one participant.
# ============================================================

create_acceptance_letter_pdf <- function(
    participant_name,
    paper_title,
    footer_text = "",
    letter_title,
    body_text,
    signature_count = 1,
    signature_text_1,
    signature_image_1 = NULL,
    signature_text_2 = "",
    signature_image_2 = NULL,
    signature_text_3 = "",
    signature_image_3 = NULL,
    signature_width_cm = 4,
    signature_height_cm = 2,
    template,
    primary_color,
    secondary_color,
    title_font_family = "IBM Plex Sans",
    body_font_family = "IBM Plex Sans",
    header_caption = "",
    logo_image = NULL,
    output_file) {

  # A4 portrait dimensions are expressed in inches for the PDF graphics device.
  page_width <- 8.27
  page_height <- 11.69
  left_margin <- 1.00
  right_margin <- 1.00
  bottom_margin <- 1.00
  content_top_margin <- 1.45
  text_width <- page_width - left_margin - right_margin

  # Personalize the title while preserving bold placeholders for the rich-text title renderer.
  letter_title <- fill_placeholders_bold(
    letter_title,
    participant_name,
    paper_title
  )

  # Preserve {name} and {paper} as inline-bold values inside the letter body.
  body_text <- fill_placeholders_bold(
    body_text,
    participant_name,
    paper_title
  )

  signature_text_1 <- fill_placeholders(
    signature_text_1,
    participant_name,
    paper_title
  )

  signature_text_2 <- fill_placeholders(
    signature_text_2,
    participant_name,
    paper_title
  )

  signature_text_3 <- fill_placeholders(
    signature_text_3,
    participant_name,
    paper_title
  )

  footer_text <- fill_placeholders(
    footer_text,
    participant_name,
    paper_title
  )

  # Resolve the selected open-source fonts before opening the PDF device.
  # Fonts are downloaded from Google Fonts only when first used in the current R process.
  # If downloading is unavailable, the renderer falls back to generic serif/sans families.
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

  # Reuses the selected template whenever a new page is created.
  decorate_page <- function() {
    draw_page_template(
      template_name = template,
      primary_color = primary_color,
      secondary_color = secondary_color
    )

    draw_footer_note(
      footer_text = footer_text,
      page_width = page_width,
      right_margin = right_margin,
      font_family = body_font_family
    )
  }

  grid.newpage()
  decorate_page()

  # Optional congress branding works identically with both available templates.
  branding_present <- draw_standard_branding_panel(
    header_caption = header_caption,
    logo_image = logo_image,
    primary_color = primary_color,
    secondary_color = secondary_color,
    page_width = page_width,
    left_margin = left_margin,
    right_margin = right_margin,
    title_font_family = title_font_family,
    template_name = template
  )

  # Move the document title down only when branding occupies the top area.
  # The title uses normal weight by default; Markdown controls bold/italic styling.
  title_font_size <- 18
  title_y <- if (is_svg_template(template)) {
    if (branding_present) 8.28 else 8.36
  } else {
    if (branding_present) 9.45 else page_height - content_top_margin
  }

  title_y <- draw_centered_rich_title(
    text = letter_title,
    center_x = page_width / 2,
    y_position = title_y,
    text_width = text_width,
    font_size = title_font_size,
    font_family = title_font_family,
    font_color = "#000000"
  )

  # A single divider line creates visual hierarchy below the letter title.
  divider_y <- title_y - 0.08

  grid.lines(
    x = unit(c(left_margin, page_width - right_margin), "inches"),
    y = unit(c(divider_y, divider_y), "inches"),
    gp = gpar(
      lwd = 1,
      col = alpha_color(primary_color, 0.40)
    )
  )

  # Draw the main acceptance text and return the y-position where it ends.
  body_start_y <- divider_y - 0.43

  current_y <- draw_rich_text(
    text = body_text,
    x_position = left_margin,
    y_position = body_start_y,
    text_width = text_width,
    font_size = 11,
    font_family = body_font_family,
    base_face = "plain",
    font_color = "#000000",
    line_spacing = 1.45,
    paragraph_spacing = 0.12,
    page_height = page_height,
    top_margin = content_top_margin,
    bottom_margin = bottom_margin,
    page_decorator = decorate_page
  )

  current_y <- current_y - 0.30

  # ----------------------------------------------------------
  # ONE SIGNATURE
  # A single signature is placed on the left side of the content area.
  # ----------------------------------------------------------

  if (signature_count == 1) {
    signature_block_width <- min(3.70, text_width)

    required_height <- estimate_signature_block_height(
      signature_text = signature_text_1,
      signature_image = signature_image_1,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      block_width = signature_block_width,
      font_family = body_font_family
    )

    if (current_y - required_height < bottom_margin) {
      grid.newpage()
      decorate_page()
      current_y <- page_height - content_top_margin
    }

    draw_signature_block(
      signature_text = signature_text_1,
      signature_image = signature_image_1,
      x_position = left_margin,
      y_position = current_y,
      block_width = signature_block_width,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      text_color = "#000000",
      font_family = body_font_family
    )
  }

  # ----------------------------------------------------------
  # TWO SIGNATURES
  # Two signatures are aligned side by side on the same row.
  # ----------------------------------------------------------

  if (signature_count == 2) {
    signature_gap <- 0.55
    signature_block_width <- (text_width - signature_gap) / 2

    signature_1_height <- estimate_signature_block_height(
      signature_text = signature_text_1,
      signature_image = signature_image_1,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      block_width = signature_block_width,
      font_family = body_font_family
    )

    signature_2_height <- estimate_signature_block_height(
      signature_text = signature_text_2,
      signature_image = signature_image_2,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      block_width = signature_block_width,
      font_family = body_font_family
    )

    required_height <- max(signature_1_height, signature_2_height)

    if (current_y - required_height < bottom_margin) {
      grid.newpage()
      decorate_page()
      current_y <- page_height - content_top_margin
    }

    signature_1_x <- left_margin
    signature_2_x <- left_margin + signature_block_width + signature_gap

    draw_signature_block(
      signature_text = signature_text_1,
      signature_image = signature_image_1,
      x_position = signature_1_x,
      y_position = current_y,
      block_width = signature_block_width,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      text_color = "#000000",
      font_family = body_font_family
    )

    draw_signature_block(
      signature_text = signature_text_2,
      signature_image = signature_image_2,
      x_position = signature_2_x,
      y_position = current_y,
      block_width = signature_block_width,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      text_color = "#000000",
      font_family = body_font_family
    )
  }

  # ----------------------------------------------------------
  # THREE SIGNATURES
  # The first two are side by side; the third is centered below them.
  # ----------------------------------------------------------

  if (signature_count == 3) {
    signature_gap <- 0.55
    vertical_signature_gap <- 0.28
    signature_block_width <- (text_width - signature_gap) / 2

    signature_1_height <- estimate_signature_block_height(
      signature_text = signature_text_1,
      signature_image = signature_image_1,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      block_width = signature_block_width,
      font_family = body_font_family
    )

    signature_2_height <- estimate_signature_block_height(
      signature_text = signature_text_2,
      signature_image = signature_image_2,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      block_width = signature_block_width,
      font_family = body_font_family
    )

    signature_3_height <- estimate_signature_block_height(
      signature_text = signature_text_3,
      signature_image = signature_image_3,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      block_width = signature_block_width,
      font_family = body_font_family
    )

    first_row_height <- max(signature_1_height, signature_2_height)
    required_height <- first_row_height + vertical_signature_gap + signature_3_height

    if (current_y - required_height < bottom_margin) {
      grid.newpage()
      decorate_page()
      current_y <- page_height - content_top_margin
    }

    signature_1_x <- left_margin
    signature_2_x <- left_margin + signature_block_width + signature_gap
    signature_3_x <- left_margin + (text_width - signature_block_width) / 2
    signature_3_y <- current_y - first_row_height - vertical_signature_gap

    draw_signature_block(
      signature_text = signature_text_1,
      signature_image = signature_image_1,
      x_position = signature_1_x,
      y_position = current_y,
      block_width = signature_block_width,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      text_color = "#000000",
      font_family = body_font_family
    )

    draw_signature_block(
      signature_text = signature_text_2,
      signature_image = signature_image_2,
      x_position = signature_2_x,
      y_position = current_y,
      block_width = signature_block_width,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      text_color = "#000000",
      font_family = body_font_family
    )

    draw_signature_block(
      signature_text = signature_text_3,
      signature_image = signature_image_3,
      x_position = signature_3_x,
      y_position = signature_3_y,
      block_width = signature_block_width,
      signature_width_cm = signature_width_cm,
      signature_height_cm = signature_height_cm,
      text_color = "#000000",
      font_family = body_font_family
    )
  }
}


# ============================================================
# USER INTERFACE
# A custom CSS layer gives the Shiny app a modern web-product appearance.
# ============================================================

ui <- fluidPage(

  # Load DM Serif Display for headings and IBM Plex Sans for the general interface.
  # IBM Plex Sans gives menus, controls, helper text, tables, and status information a crisp technical feel.
  tags$head(
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
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

    # Restore the user's saved appearance before the body is painted to avoid a light-mode flash.
    tags$script(
      HTML(
        "
        (function() {
          try {
            if (localStorage.getItem('confercraft-theme') === 'dark') {
              document.documentElement.classList.add('confercraft-dark');
            }
          } catch (error) {
            // Local storage can be unavailable in restrictive browser contexts.
          }
        })();
        "
      )
    ),

    # The CSS below borrows the visual language of modern academic web products:
    # white space, editorial serif headings, teal accents, fine borders, and soft mint surfaces.
    tags$style(
      HTML(
        "
        :root {
          --ink: #151918;
          --ink-soft: #2b302f;
          --paper: #fbfcfb;
          --card: #ffffff;
          --teal: #0f7a6c;
          --teal-dark: #0a5e54;
          --teal-soft: #e9f6f3;
          --teal-soft-2: #f3faf8;
          --sage: #dcece7;
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

        body {
          min-height: 100vh;
        }

        /* Keep all interface controls in the same IBM Plex Sans family. */
        button,
        input,
        select,
        textarea,
        .form-control,
        .btn,
        .selectize-input,
        .selectize-dropdown {
          font-family: 'IBM Plex Sans', system-ui, sans-serif;
        }

        .container-fluid {
          padding: 0;
        }

        .app-shell {
          max-width: 1640px;
          margin: 0 auto;
          padding: 22px 30px 54px 30px;
        }

        /* The top bar echoes a clean research-site navigation header. */
        .app-topbar {
          position: relative;
          overflow: hidden;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 24px;
          margin-bottom: 28px;
          padding: 18px 22px;
          background: rgba(255, 255, 255, 0.90);
          border: 1px solid var(--line);
          border-radius: 22px;
          box-shadow: var(--shadow-soft);
          backdrop-filter: blur(12px);
          -webkit-backdrop-filter: blur(12px);
        }

        .brand-kicker {
          display: inline-flex;
          align-items: center;
          width: fit-content;
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

        .brand-title {
          font-family: 'DM Serif Display', serif;
          font-size: clamp(30px, 3vw, 46px);
          font-weight: 400;
          line-height: 1.02;
          letter-spacing: -0.025em;
          color: #121413;
          margin: 0;
        }

        .brand-subtitle {
          max-width: 680px;
          color: var(--muted);
          font-size: 13px;
          line-height: 1.7;
          margin-top: 9px;
          margin-bottom: 0;
        }

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

        .topbar-icon-link svg {
          width: 22px;
          height: 22px;
          display: block;
          fill: currentColor;
        }

        /* The control panel is light and crisp instead of default Shiny gray. */
        .sidebar-card {
          position: relative;
          overflow: hidden;
          background: rgba(255, 255, 255, 0.96);
          color: var(--ink);
          border: 1px solid var(--line);
          border-radius: 24px;
          padding: 24px;
          box-shadow: var(--shadow);
          margin-bottom: 20px;
        }

        /* Every main app card uses the same ConferCraft accent on both edges. */
        .app-topbar::before,
        .sidebar-card::before,
        .workspace-card::before,
        .app-topbar::after,
        .sidebar-card::after,
        .workspace-card::after {
          content: '';
          position: absolute;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, var(--teal), #5cb5a7, #b8ded6);
          pointer-events: none;
          z-index: 1;
        }

        .app-topbar::before,
        .sidebar-card::before,
        .workspace-card::before {
          top: 0;
        }

        .app-topbar::after,
        .sidebar-card::after,
        .workspace-card::after {
          bottom: 0;
          background: linear-gradient(90deg, #b8ded6, #5cb5a7, var(--teal));
        }

        .sidebar-card h4,
        .sidebar-card h5 {
          color: var(--ink);
        }

        .sidebar-card h4 {
          position: relative;
          margin-top: 24px;
          padding-top: 2px;
          font-size: 14px;
          font-weight: 700;
          letter-spacing: -0.015em;
        }

        .sidebar-card h4:first-of-type {
          margin-top: 2px;
        }

        .sidebar-card h4::after {
          content: '';
          display: block;
          width: 28px;
          height: 2px;
          margin-top: 7px;
          border-radius: 999px;
          background: var(--teal);
          opacity: 0.75;
        }

        .sidebar-card h5 {
          font-family: 'DM Serif Display', serif;
          font-size: 18px;
          font-weight: 400;
          color: #26312e;
          margin-top: 4px;
          margin-bottom: 11px;
        }

        .sidebar-card label,
        .sidebar-card .control-label {
          color: #3f4846;
          font-size: 12px;
          font-weight: 600;
          margin-bottom: 7px;
        }

        .sidebar-card .help-block {
          color: #7b8481;
          font-size: 11px;
          line-height: 1.6;
        }

        .sidebar-card hr {
          border: 0;
          border-top: 1px solid var(--line);
          margin: 23px 0;
        }

        .sidebar-card .form-control,
        .sidebar-card .selectize-input,
        .sidebar-card input[type='number'] {
          min-height: 42px;
          border: 1px solid #d9e5e1 !important;
          border-radius: 12px !important;
          background: #fbfdfc !important;
          color: var(--ink) !important;
          box-shadow: none !important;
          transition: border-color 0.16s ease, box-shadow 0.16s ease, background 0.16s ease;
        }

        .sidebar-card .form-control:focus,
        .sidebar-card .selectize-input.focus,
        .sidebar-card input[type='number']:focus {
          border-color: #72b9ab !important;
          background: #ffffff !important;
          box-shadow: 0 0 0 3px rgba(15, 122, 108, 0.09) !important;
        }

        .sidebar-card textarea.form-control {
          min-height: 112px;
          line-height: 1.55;
          resize: vertical;
        }

        .sidebar-card .selectize-dropdown,
        .sidebar-card .selectize-dropdown-content {
          background: #ffffff;
          color: var(--ink);
          border-color: var(--line);
        }

        .sidebar-card .selectize-input input {
          color: var(--ink) !important;
        }

        /* File uploads use two compact, perfectly aligned joined boxes. */
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

        /* The real file input remains transparent and clickable over the Browse button. */
        .sidebar-card .btn-file input[type='file'] {
          position: absolute;
          inset: 0;
          width: 100%;
          height: 100%;
          margin: 0;
          opacity: 0;
          cursor: pointer;
        }

        /* Override Bootstrap's global form-control min-height so both halves stay identical. */
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

        /* Give the complete file control a soft focus ring when either half receives focus. */
        .sidebar-card .input-group:focus-within .btn-file,
        .sidebar-card .input-group:focus-within .input-group-btn .btn {
          border-color: #72b9ab !important;
        }

        .sidebar-card .input-group:focus-within .form-control {
          border-color: #72b9ab !important;
          box-shadow: 0 0 0 3px rgba(15, 122, 108, 0.08) !important;
        }

        /* Shiny file-upload progress bars use the same ConferCraft teal as Browse buttons. */
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

        .signature-box,
        .branding-box,
        .color-box,
        .font-box {
          background: linear-gradient(180deg, #f8fcfb 0%, #f2f9f7 100%);
          border: 1px solid #d9e9e4;
          border-radius: 17px;
          padding: 15px;
          margin: 11px 0 15px 0;
        }

        /* Keep the optional branding card compact, especially after a logo upload. */
        .branding-box {
          padding: 13px 15px 12px 15px;
        }

        .branding-box .form-group,
        .branding-box .shiny-input-container {
          margin-bottom: 6px !important;
        }

        .branding-box .shiny-file-input-progress,
        .branding-box .progress {
          margin-top: 4px !important;
          margin-bottom: 2px !important;
        }

        .branding-box .help-block {
          margin-top: 0 !important;
          margin-bottom: 0 !important;
          padding-top: 0 !important;
          line-height: 1.35;
        }

        /* Each color combines a clickable native color picker with an editable HEX field. */
        .color-control {
          margin-bottom: 14px;
        }

        .color-control > label {
          display: block;
          margin-bottom: 7px;
        }

        .color-control-row {
          display: flex;
          align-items: stretch;
          gap: 9px;
        }

        .color-control-row .form-group {
          flex: 1 1 auto;
          min-width: 0;
          margin-bottom: 0;
        }

        .color-control-row .form-control {
          height: 44px;
          min-height: 44px;
        }

        .color-picker-swatch {
          flex: 0 0 58px;
          width: 58px;
          height: 44px;
          padding: 3px;
          border: 1px solid #d9e5e1;
          border-radius: 12px;
          background: #ffffff;
          cursor: pointer;
          box-shadow: none;
          transition: border-color 0.16s ease, box-shadow 0.16s ease, transform 0.16s ease;
        }

        .color-picker-swatch:hover,
        .color-picker-swatch:focus {
          border-color: #72b9ab;
          box-shadow: 0 0 0 3px rgba(15, 122, 108, 0.08);
          outline: none;
          transform: translateY(-1px);
        }

        .color-picker-swatch::-webkit-color-swatch-wrapper {
          padding: 0;
        }

        .color-picker-swatch::-webkit-color-swatch {
          border: 0;
          border-radius: 8px;
        }

        .color-picker-swatch::-moz-color-swatch {
          border: 0;
          border-radius: 8px;
        }


        .hex-warning {
          color: #a64040;
          font-weight: 600;
          font-size: 12px;
          margin-top: 8px;
        }

        .action-row {
          display: flex;
          gap: 10px;
          flex-wrap: wrap;
          margin-top: 18px;
        }

        #generate_letters,
        #update_preview {
          border: 1px solid transparent;
          border-radius: 999px;
          font-weight: 700;
          padding: 11px 17px;
          transition: transform 0.15s ease, box-shadow 0.15s ease, background 0.15s ease;
        }

        #generate_letters {
          background: var(--teal);
          color: #ffffff;
          box-shadow: 0 8px 20px rgba(15, 122, 108, 0.18);
        }

        #update_preview {
          background: var(--teal-soft);
          border-color: #c7e4de;
          color: var(--teal-dark);
        }

        #generate_letters:hover,
        #update_preview:hover {
          transform: translateY(-1px);
          box-shadow: 0 11px 25px rgba(15, 122, 108, 0.18);
        }

        /* Main workspace cards keep the visual hierarchy light and editorial. */
        .workspace-card {
          position: relative;
          overflow: hidden;
          background: rgba(255, 255, 255, 0.96);
          border: 1px solid var(--line);
          border-radius: 24px;
          padding: 24px;
          box-shadow: var(--shadow-soft);
          margin-bottom: 20px;
        }

        .workspace-card h3 {
          font-family: 'DM Serif Display', serif;
          font-size: 26px;
          font-weight: 400;
          line-height: 1.08;
          letter-spacing: -0.015em;
          color: #131716;
          margin-top: 0;
          margin-bottom: 15px;
        }

        .workspace-card .table {
          font-size: 12px;
          margin-bottom: 0;
          background: #ffffff;
        }

        .workspace-card .table > thead > tr > th {
          border-bottom: 1px solid var(--line-strong);
          color: #53605c;
          font-weight: 700;
          background: #f7fbfa;
        }

        .workspace-card .table > tbody > tr > td {
          border-top-color: #edf3f1;
        }

        .preview-header {
          display: flex;
          align-items: flex-start;
          justify-content: space-between;
          gap: 14px;
          margin-bottom: 12px;
        }

        .preview-copy {
          color: var(--muted);
          font-size: 12px;
          line-height: 1.55;
          margin: 4px 0 0 0;
        }

        .preview-placeholder {
          min-height: 520px;
          border: 1px dashed #bdd8d1;
          border-radius: 20px;
          display: flex;
          align-items: center;
          justify-content: center;
          text-align: center;
          background:
            radial-gradient(circle at 50% 35%, rgba(15, 122, 108, 0.06), transparent 33%),
            #f8fcfb;
          color: #73807c;
          padding: 34px;
        }

        .preview-placeholder strong {
          display: block;
          color: #26312e;
          font-family: 'DM Serif Display', serif;
          font-size: 23px;
          font-weight: 400;
          margin-bottom: 8px;
        }

        .pdf-frame {
          width: 100%;
          height: 880px;
          border: 1px solid #d6e4e0;
          border-radius: 20px;
          background: #eef6f4;
          box-shadow: inset 0 0 0 1px rgba(255,255,255,0.55);
        }

        .status-pill {
          display: inline-flex;
          align-items: center;
          gap: 7px;
          border-radius: 999px;
          padding: 7px 11px;
          font-size: 11px;
          font-weight: 700;
          margin-bottom: 12px;
          border: 1px solid transparent;
        }

        .status-ready {
          background: var(--success-bg);
          color: var(--success-fg);
          border-color: #cce8dc;
        }

        .status-stale {
          background: var(--warning-bg);
          color: var(--warning-fg);
          border-color: #f0ddb8;
        }

        .status-idle {
          background: #f3f6f5;
          color: #68716f;
          border-color: #e0e7e5;
        }

        .preview-selector .form-control,
        .preview-selector .selectize-input {
          border-radius: 12px !important;
          border-color: #d8e5e1 !important;
          box-shadow: none !important;
          background: #fbfdfc !important;
        }

        #download_letters {
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

        #download_letters:hover,
        #download_letters:focus {
          color: #ffffff;
          background: var(--teal-dark);
          transform: translateY(-1px);
          box-shadow: 0 10px 26px rgba(10, 94, 84, 0.20);
          text-decoration: none;
        }

        /* Appearance toggle uses the same circular visual language as Help and GitHub. */
        .theme-toggle {
          padding: 0;
          cursor: pointer;
          appearance: none;
          -webkit-appearance: none;
        }

        .theme-icon-sun {
          display: none !important;
        }

        html.confercraft-dark .theme-icon-moon {
          display: none !important;
        }

        html.confercraft-dark .theme-icon-sun {
          display: block !important;
        }

        /* Dark mode keeps the teal identity while moving the entire interface onto deep neutral surfaces. */
        html.confercraft-dark {
          color-scheme: dark;
          --ink: #f1f4f3;
          --ink-soft: #d7dddb;
          --paper: #0d1014;
          --card: #14191d;
          --teal: #25b6a0;
          --teal-dark: #138777;
          --teal-soft: #132b28;
          --teal-soft-2: #101f1d;
          --sage: #1b302d;
          --muted: #9ba6a3;
          --line: #29332f;
          --line-strong: #37443f;
          --success-bg: #102821;
          --success-fg: #79d3b0;
          --warning-bg: #302817;
          --warning-fg: #e5c577;
          --shadow: 0 20px 58px rgba(0, 0, 0, 0.30);
          --shadow-soft: 0 10px 32px rgba(0, 0, 0, 0.24);
        }

        html.confercraft-dark,
        html.confercraft-dark body {
          background:
            radial-gradient(circle at 86% 4%, rgba(37, 182, 160, 0.10), transparent 29%),
            radial-gradient(circle at 4% 66%, rgba(37, 182, 160, 0.055), transparent 25%),
            linear-gradient(180deg, #0d1014 0%, #0f1317 48%, #10161a 100%);
          color: var(--ink);
        }

        html.confercraft-dark .app-topbar,
        html.confercraft-dark .sidebar-card,
        html.confercraft-dark .workspace-card {
          background: rgba(17, 22, 26, 0.96);
          border-color: var(--line);
        }

        html.confercraft-dark .brand-kicker {
          background: rgba(37, 182, 160, 0.12);
          border-color: rgba(66, 200, 180, 0.25);
          color: #5bd0be;
        }

        html.confercraft-dark .brand-title,
        html.confercraft-dark .workspace-card h3,
        html.confercraft-dark .sidebar-card h5,
        html.confercraft-dark .preview-placeholder strong {
          color: #f5f6f6;
        }

        html.confercraft-dark .brand-subtitle,
        html.confercraft-dark .preview-copy,
        html.confercraft-dark .sidebar-card .help-block {
          color: var(--muted);
        }

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

        html.confercraft-dark .sidebar-card h4,
        html.confercraft-dark .sidebar-card label,
        html.confercraft-dark .sidebar-card .control-label {
          color: #dce3e1;
        }

        html.confercraft-dark .sidebar-card hr {
          border-top-color: var(--line);
        }

        html.confercraft-dark .sidebar-card .form-control,
        html.confercraft-dark .sidebar-card .selectize-input,
        html.confercraft-dark .sidebar-card input[type='number'],
        html.confercraft-dark .sidebar-card .input-group .form-control,
        html.confercraft-dark .preview-selector .form-control,
        html.confercraft-dark .preview-selector .selectize-input {
          background: #11171b !important;
          border-color: #34423e !important;
          color: #eef2f1 !important;
        }

        html.confercraft-dark .sidebar-card .form-control:focus,
        html.confercraft-dark .sidebar-card .selectize-input.focus,
        html.confercraft-dark .sidebar-card input[type='number']:focus,
        html.confercraft-dark .preview-selector .form-control:focus,
        html.confercraft-dark .preview-selector .selectize-input.focus {
          background: #151d21 !important;
          border-color: #3e9d8d !important;
          box-shadow: 0 0 0 3px rgba(37, 182, 160, 0.12) !important;
        }

        html.confercraft-dark input::placeholder,
        html.confercraft-dark textarea::placeholder,
        html.confercraft-dark .form-control::placeholder {
          color: #77827f !important;
          opacity: 1;
        }

        html.confercraft-dark .sidebar-card .selectize-dropdown,
        html.confercraft-dark .sidebar-card .selectize-dropdown-content,
        html.confercraft-dark .preview-selector .selectize-dropdown,
        html.confercraft-dark .preview-selector .selectize-dropdown-content {
          background: #151c20;
          color: #edf2f0;
          border-color: #34423e;
        }

        html.confercraft-dark .selectize-dropdown .option,
        html.confercraft-dark .selectize-dropdown .optgroup-header {
          color: #e6ecea;
          background: transparent;
        }

        html.confercraft-dark .selectize-dropdown .active,
        html.confercraft-dark .selectize-dropdown .option:hover {
          background: #20302e;
          color: #ffffff;
        }

        html.confercraft-dark .signature-box,
        html.confercraft-dark .branding-box,
        html.confercraft-dark .color-box,
        html.confercraft-dark .font-box {
          background: linear-gradient(180deg, #131d20 0%, #11191c 100%);
          border-color: #2c3b38;
        }

        html.confercraft-dark .color-picker-swatch {
          background: #151c20;
          border-color: #34423e;
        }

        html.confercraft-dark .hex-warning {
          color: #ef8d8d;
        }

        html.confercraft-dark #update_preview {
          background: #15312d;
          border-color: #285a52;
          color: #73d3c3;
        }

        html.confercraft-dark .workspace-card .table {
          background: #11171b;
          color: #e7ecea;
        }

        html.confercraft-dark .workspace-card .table > thead > tr > th {
          background: #172024;
          color: #c9d2cf;
          border-bottom-color: #37443f;
        }

        html.confercraft-dark .workspace-card .table > tbody > tr > td {
          border-top-color: #26312e;
          color: #dde4e2;
        }

        html.confercraft-dark .preview-placeholder {
          border-color: #35564f;
          background:
            radial-gradient(circle at 50% 35%, rgba(37, 182, 160, 0.09), transparent 34%),
            #11191d;
          color: #98a4a1;
        }

        html.confercraft-dark .pdf-frame {
          border-color: #34413e;
          background: #1a2226;
          box-shadow: inset 0 0 0 1px rgba(255,255,255,0.025);
        }

        html.confercraft-dark .status-ready {
          background: #102821;
          color: #7bd4b2;
          border-color: #24493e;
        }

        html.confercraft-dark .status-stale {
          background: #302817;
          color: #e6c77a;
          border-color: #51452a;
        }

        html.confercraft-dark .status-idle {
          background: #1a2125;
          color: #a8b2af;
          border-color: #333e3b;
        }

        html.confercraft-dark #shiny-notification-panel .shiny-notification {
          background: #172024;
          color: #e9efed;
          border-color: #33413d;
        }

        @media (max-width: 991px) {
          .app-shell {
            padding: 14px;
          }

          .app-topbar {
            align-items: flex-start;
            padding: 17px;
          }

          .brand-title {
            font-size: 32px;
          }

          .pdf-frame {
            height: 650px;
          }
        }
        "
      )
    ),

    # Toggle light/dark appearance from the top-right icon and remember the preference locally.
    tags$script(
      HTML(
        "
        (function() {
          var storageKey = 'confercraft-theme';

          function darkModeIsOn() {
            return document.documentElement.classList.contains('confercraft-dark');
          }

          function syncThemeButton() {
            var button = document.getElementById('theme_toggle');
            if (!button) return;

            var dark = darkModeIsOn();
            var label = dark ? 'Switch to light mode' : 'Switch to dark mode';
            button.setAttribute('title', label);
            button.setAttribute('aria-label', label);
            button.setAttribute('aria-pressed', dark ? 'true' : 'false');
          }

          function setTheme(dark) {
            document.documentElement.classList.toggle('confercraft-dark', dark);

            try {
              localStorage.setItem(storageKey, dark ? 'dark' : 'light');
            } catch (error) {
              // The visual switch still works even when storage is unavailable.
            }

            syncThemeButton();
          }

          document.addEventListener('DOMContentLoaded', syncThemeButton);

          document.addEventListener('click', function(event) {
            var target = event.target;
            var button = target && target.closest ? target.closest('#theme_toggle') : null;
            if (!button) return;

            event.preventDefault();
            setTheme(!darkModeIsOn());
          });
        })();
        "
      )
    ),

    # Keep the visual color pickers and HEX text fields synchronized in both directions.
    tags$script(
      HTML(
        "
        (function() {
          function normalizePickerHex(value) {
            if (!value) return null;
            var hex = value.trim().toUpperCase();
            var shortHex = /^#([0-9A-F]{3})$/;
            var longHex = /^#([0-9A-F]{6})$/;

            if (shortHex.test(hex)) {
              var chars = hex.substring(1).split('');
              return '#' + chars.map(function(ch) { return ch + ch; }).join('');
            }

            if (longHex.test(hex)) {
              return hex;
            }

            return null;
          }

          function pickerToText(pickerId, textId) {
            var value = $(pickerId).val();
            if (!value) return;
            $(textId).val(value.toUpperCase()).trigger('change');
          }

          function textToPicker(textId, pickerId) {
            var normalized = normalizePickerHex($(textId).val());
            if (normalized) {
              $(pickerId).val(normalized);
            }
          }

          $(document).on('input change', '#primary_color_picker', function() {
            pickerToText('#primary_color_picker', '#primary_color');
          });


          $(document).on('input change keyup paste', '#primary_color', function() {
            textToPicker('#primary_color', '#primary_color_picker');
          });


          $(document).on('shiny:connected', function() {
            textToPicker('#primary_color', '#primary_color_picker');
          });
        })();
        "
      )
    )
  ),

  div(
    class = "app-shell",

    # A compact top bar gives the app a modern product-like identity.
    div(
      class = "app-topbar",
      div(
        div(class = "brand-kicker", "ConferCraft"),
        h1(class = "brand-title", "Acceptance Letter Generator"),
        p(
          class = "brand-subtitle",
          "Build, preview, update, and export personalized acceptance letters."
        )
      ),
      # Appearance, Help, and GitHub controls share the same compact circular button style.
      div(
        class = "topbar-actions",
        tags$button(
          id = "theme_toggle",
          class = "topbar-icon-link theme-toggle",
          type = "button",
          title = "Switch to dark mode",
          `aria-label` = "Switch to dark mode",
          `aria-pressed` = "false",
          HTML(
            paste0(
              '<svg class="theme-icon-moon" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M20.15 15.42A8.1 8.1 0 0 1 8.58 3.85 8.65 8.65 0 1 0 20.15 15.42Zm-8.2 5.03A6.95 6.95 0 0 1 6.44 9.27a6.9 6.9 0 0 1 .47-3.34 9.25 9.25 0 0 0 10.66 10.66 6.91 6.91 0 0 1-5.62 3.86Z"/></svg>',
              '<svg class="theme-icon-sun" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M12 7.25A4.75 4.75 0 1 0 12 16.75 4.75 4.75 0 0 0 12 7.25Zm0 8A3.25 3.25 0 1 1 12 8.75a3.25 3.25 0 0 1 0 6.5ZM12 1.5a.75.75 0 0 1 .75.75v2a.75.75 0 0 1-1.5 0v-2A.75.75 0 0 1 12 1.5Zm0 17.5a.75.75 0 0 1 .75.75v2a.75.75 0 0 1-1.5 0v-2A.75.75 0 0 1 12 19ZM4.58 3.52a.75.75 0 0 1 1.06 0l1.42 1.42A.75.75 0 1 1 6 6L4.58 4.58a.75.75 0 0 1 0-1.06Zm12.36 12.36a.75.75 0 0 1 1.06 0l1.42 1.42a.75.75 0 1 1-1.06 1.06l-1.42-1.42a.75.75 0 0 1 0-1.06ZM1.5 12a.75.75 0 0 1 .75-.75h2a.75.75 0 0 1 0 1.5h-2A.75.75 0 0 1 1.5 12Zm17.5 0a.75.75 0 0 1 .75-.75h2a.75.75 0 0 1 0 1.5h-2A.75.75 0 0 1 19 12ZM4.58 20.48a.75.75 0 0 1 0-1.06L6 18a.75.75 0 1 1 1.06 1.06l-1.42 1.42a.75.75 0 0 1-1.06 0Zm12.36-12.36a.75.75 0 0 1 0-1.06l1.42-1.42a.75.75 0 1 1 1.06 1.06L18 8.12a.75.75 0 0 1-1.06 0Z"/></svg>'
            )
          )
        ),
        tags$a(
          class = "topbar-icon-link",
          href = "https://www.google.com",
          target = "_blank",
          rel = "noopener noreferrer",
          title = "Help",
          `aria-label` = "Open Help",
          HTML(
            '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M12 2.25a9.75 9.75 0 1 0 0 19.5 9.75 9.75 0 0 0 0-19.5Zm0 17.75a8 8 0 1 1 0-16 8 8 0 0 1 0 16Zm.08-4.35a1.05 1.05 0 1 0 0 2.1 1.05 1.05 0 0 0 0-2.1Zm.22-9.4c-2.16 0-3.55 1.19-3.65 3.13h1.88c.08-.93.71-1.5 1.69-1.5 1.01 0 1.67.55 1.67 1.4 0 .7-.34 1.11-1.35 1.79-1.24.83-1.74 1.56-1.65 3.05l.01.3h1.84l-.01-.28c-.03-.87.25-1.28 1.3-1.99 1.22-.81 1.77-1.67 1.77-2.93 0-1.6-1.36-2.67-3.5-2.67Z"/></svg>'
          )
        ),
        tags$a(
          class = "topbar-icon-link",
          href = "https://github.com/gungorMetehan",
          target = "_blank",
          rel = "noopener noreferrer",
          title = "Metehan Güngör on GitHub",
          `aria-label` = "Open Metehan Güngör GitHub profile",
          HTML(
            '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M12 .7C5.65.7.5 5.85.5 12.2c0 5.08 3.29 9.39 7.86 10.91.58.1.79-.25.79-.56 0-.28-.01-1.2-.02-2.18-3.2.7-3.88-1.36-3.88-1.36-.52-1.33-1.28-1.68-1.28-1.68-1.05-.72.08-.7.08-.7 1.16.08 1.77 1.19 1.77 1.19 1.03 1.77 2.7 1.26 3.36.96.1-.75.4-1.26.73-1.55-2.55-.29-5.24-1.28-5.24-5.69 0-1.26.45-2.28 1.19-3.09-.12-.29-.52-1.46.11-3.05 0 0 .97-.31 3.17 1.18A11.1 11.1 0 0 1 12 6.2c.98 0 1.96.13 2.88.39 2.2-1.49 3.17-1.18 3.17-1.18.63 1.59.23 2.76.11 3.05.74.81 1.19 1.83 1.19 3.09 0 4.42-2.69 5.39-5.25 5.68.41.36.78 1.06.78 2.14 0 1.55-.01 2.79-.01 3.17 0 .31.21.67.79.56 4.56-1.52 7.85-5.83 7.85-10.9C23.5 5.85 18.35.7 12 .7Z"/></svg>'
          )
        )
      )
    ),

    fluidRow(

      # ======================================================
      # LEFT CONTROL PANEL
      # ======================================================

      column(
        width = 4,

        div(
          class = "sidebar-card",

          # 1. Upload the participant Excel file.
          h4("1. Participant List"),
          fileInput(
            inputId = "excel_file",
            label = "Upload Excel File",
            accept = c(".xlsx", ".xls")
          ),
          helpText(
            "Please make sure the first column in your uploaded Excel file is Full Name and the second column is Paper Title. The third column is used as optional footer text."
          ),

          hr(),

          # 2. Set the main acceptance-letter title.
          h4("2. Letter Title"),
          textInput(
            inputId = "letter_title",
            label = "Title",
            value = "**ACCEPTANCE LETTER**"
          ),

          hr(),

          # 3. Edit the body and use placeholders for Excel values.
          h4("3. Letter Body"),
          helpText("{name} = participant's full name | first column in your uploaded Excel"),
          helpText("{paper} = paper title | second column in your uploaded Excel"),
          textAreaInput(
            inputId = "body_text",
            label = "Body Text",
            value = paste(
              "Dear {name},",
              "",
              paste(
                "On behalf of the Organizing and Scientific Committees, we are pleased to inform you",
                "that your paper entitled \"{paper}\" has been **accepted for presentation** at our congress.",
                "Following *scientific evaluation*, your submission was considered relevant to the congress scope",
                "and of sufficient academic merit for inclusion in the ***official scientific program***."
              ),
              "",
              paste(
                "We congratulate you on the **acceptance of your work** and thank you for contributing to the",
                "*scholarly exchange* of the congress. We look forward to your participation and to welcoming",
                "you among the researchers, professionals, and delegates joining this ***scientific event***."
              ),
              sep = "\n"
            ),
            rows = 13,
            width = "100%"
          ),

          hr(),

          # 4. Select one, two, or three independent signature blocks.
          h4("4. Signatures"),
          selectInput(
            inputId = "signature_count",
            label = "Number of Signatures",
            choices = c(
              "1" = "1",
              "2" = "2",
              "3" = "3"
            ),
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
              # Supports the same Markdown, bullets, and math syntax as Body Text.
              value = paste(
                "Sincerely,",
                "Prof. Dr. Name Surname",
                "Congress Chair",
                "Scientific Committee",
                sep = "\n"
              ),
              rows = 5,
              width = "100%"
            )
          ),

          conditionalPanel(
            condition = "input.signature_count >= '2'",
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
                  "Sincerely,",
                  "Prof. Dr. Second Name Surname",
                  "Congress Co-Chair",
                  "Scientific Committee",
                  sep = "\n"
                ),
                rows = 5,
                width = "100%"
              )
            )
          ),

          conditionalPanel(
            condition = "input.signature_count == '3'",
            div(
              class = "signature-box",
              h5("Signature 3"),
              fileInput(
                inputId = "signature_image_3",
                label = "Upload Signature Image",
                accept = c(".png", ".jpg", ".jpeg")
              ),
              textAreaInput(
                inputId = "signature_text_3",
                label = "Signature Text",
                value = paste(
                  "Sincerely,",
                  "Prof. Dr. Third Name Surname",
                  "Scientific Committee Member",
                  "Congress Organizing Committee",
                  sep = "\n"
                ),
                rows = 5,
                width = "100%"
              )
            )
          ),

          numericInput(
            inputId = "signature_width",
            label = "Signature Image Width (cm)",
            value = 4,
            min = 1,
            max = 10,
            step = 0.5
          ),

          numericInput(
            inputId = "signature_height",
            label = "Signature Image Height (cm)",
            value = 2,
            min = 0.5,
            max = 8,
            step = 0.5
          ),

          hr(),

          # 5. Choose one of the eight SVG templates stored in the templates folder.
          h4("5. PDF Design"),
          selectInput(
            inputId = "template",
            label = "Template",
            choices = c(
              "Linear Horizon" = "linear_horizon",
              "Contour Flow" = "contour_flow",
              "Diamond Edge" = "diamond_edge",
              "Watercolor Bloom" = "watercolor_bloom",
              "Canvas Wash" = "canvas_wash",
              "Silken Waves" = "silken_waves",
              "Prism Dots" = "prism_dots",
              "Origami Fold" = "origami_fold"
            ),
            selected = "linear_horizon"
          ),


          # Users can select one font for titles/branding and another for all other PDF text.
          div(
            class = "font-box",
            h5("PDF Fonts"),
            selectInput(
              inputId = "title_font_family",
              label = "Title & Branding Font",
              choices = OPEN_FONT_CHOICES,
              selected = "IBM Plex Sans"
            ),
            selectInput(
              inputId = "body_font_family",
              label = "Body & Signature Font",
              choices = OPEN_FONT_CHOICES,
              selected = "IBM Plex Sans"
            )
          ),

          # Optional congress branding is available for the current template.
          div(
            class = "branding-box",
            h5("Congress Branding (Optional)"),
            textInput(
              inputId = "header_caption",
              label = "Congress Header / Organization Name",
              value = "",
              placeholder = "e.g. International Congress of Psychology 2026"
            ),
            fileInput(
              inputId = "logo_image",
              label = "Congress Logo",
              accept = c(".png", ".jpg", ".jpeg")
            ),
            helpText(
              paste(
                "Both fields are optional.",
                "Leave them blank if no congress branding is needed."
              )
            )
          ),

          # Every SVG template uses the same single editable accent color.
          # Decorative artwork is recolored from this value while white areas stay white.
          div(
            class = "color-box",
            h5("Custom Color"),

            div(
              class = "color-control",
              tags$label(
                `for` = "primary_color",
                "Template Color"
              ),
              div(
                class = "color-control-row",
                tags$input(
                  id = "primary_color_picker",
                  class = "color-picker-swatch",
                  type = "color",
                  value = "#9BD3D4",
                  title = "Choose template color",
                  `aria-label` = "Choose template color"
                ),
                textInput(
                  inputId = "primary_color",
                  label = NULL,
                  value = "#9BD3D4",
                  placeholder = "#9BD3D4",
                  width = "100%"
                )
              )
            ),

            uiOutput("color_preview")
          ),

          # Generate creates the first preview; Update always remains visible for later edits.
          div(
            class = "action-row",
            actionButton(
              inputId = "generate_letters",
              label = "Generate PDF Letters"
            ),
            actionButton(
              inputId = "update_preview",
              label = "Update Preview"
            )
          ),
          helpText(
            "Generate once, then use Update Preview after changing the template, colors, text, branding, or signatures."
          )
        )
      ),

      # ======================================================
      # RIGHT WORKSPACE
      # ======================================================

      column(
        width = 8,

        # Show a small Excel preview before PDF generation.
        div(
          class = "workspace-card",
          h3("Participant Data"),
          tableOutput("excel_preview")
        ),

        # The PDF preview is displayed here before the ZIP can be downloaded.
        div(
          class = "workspace-card",
          div(
            class = "preview-header",
            div(
              h3("PDF Preview"),
              p(
                class = "preview-copy",
                paste(
                  "Generate the letters, inspect any participant,",
                  "then change the template or colors and click Update Preview."
                )
              )
            )
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
# The server reads Excel data, generates previews, updates them, and exports ZIP files.
# ============================================================

server <- function(input, output, session) {

  # Read and validate the uploaded Excel file only when it is available.
  excel_data <- reactive({
    req(input$excel_file)

    participant_data <- read_excel(
      input$excel_file$datapath
    )

    shiny::validate(
      shiny::need(
        ncol(participant_data) >= 2,
        "The Excel file must contain at least two columns: Full Name and Paper Title."
      ),
      shiny::need(
        nrow(participant_data) > 0,
        "No participant records were found."
      )
    )

    participant_data
  })

  # Show the first ten rows so the user can verify the uploaded file.
  output$excel_preview <- renderTable({
    if (is.null(input$excel_file)) {
      return(NULL)
    }

    head(excel_data(), 10)
  })


  # Show a validation message only when one of the manually entered HEX values is invalid.
  # The clickable color swatches themselves are rendered directly in the sidebar.
  output$color_preview <- renderUI({
    primary_valid <- is_valid_hex(input$primary_color)

    if (!primary_valid) {
      return(
        div(
          class = "hex-warning",
          "Please enter a valid HEX color, for example #9BD3D4."
        )
      )
    }

    NULL
  })

  # Create a session-specific folder that Shiny can safely expose for PDF previews.
  preview_root <- file.path(
    tempdir(),
    paste0("acceptance_preview_", session$token)
  )

  dir.create(
    preview_root,
    recursive = TRUE,
    showWarnings = FALSE
  )

  preview_prefix <- paste0(
    "acceptance_preview_",
    gsub("[^A-Za-z0-9_-]", "", session$token)
  )

  addResourcePath(
    preview_prefix,
    preview_root
  )

  # Clean up preview files and the temporary web resource when the session closes.
  session$onSessionEnded(function() {
    try(removeResourcePath(preview_prefix), silent = TRUE)
    unlink(preview_root, recursive = TRUE, force = TRUE)
  })

  # Store generated preview information without forcing an immediate download.
  preview_state <- reactiveValues(
    generated = FALSE,
    stale = FALSE,
    building = FALSE,
    files = character(0),
    labels = character(0),
    zip_file = NULL,
    generated_at = NULL,
    # Incremented after every successful rebuild so the PDF iframe URL changes.
    # This prevents the browser's built-in PDF viewer from showing a cached file.
    revision = 0L
  )

  # Creates a small fingerprint so replacing an upload also marks the preview as outdated.
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

  # Track settings that affect the PDF. Any later change marks the preview as outdated.
  preview_configuration <- reactive({
    list(
      excel = uploaded_file_fingerprint(input$excel_file),
      letter_title = input$letter_title,
      body_text = input$body_text,
      signature_count = input$signature_count,
      signature_image_1 = uploaded_file_fingerprint(input$signature_image_1),
      signature_text_1 = input$signature_text_1,
      signature_image_2 = uploaded_file_fingerprint(input$signature_image_2),
      signature_text_2 = input$signature_text_2,
      signature_image_3 = uploaded_file_fingerprint(input$signature_image_3),
      signature_text_3 = input$signature_text_3,
      signature_width = input$signature_width,
      signature_height = input$signature_height,
      template = input$template,
      title_font_family = input$title_font_family,
      body_font_family = input$body_font_family,
      header_caption = input$header_caption,
      logo_image = uploaded_file_fingerprint(input$logo_image),
      primary_color = input$primary_color
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

  # Builds all participant PDFs in the preview folder and also creates a ZIP archive.
  build_preview_batch <- function() {
    if (!is_valid_hex(input$primary_color)) {
      stop("Template Color must be a valid HEX color.")
    }

    participant_data <- excel_data()
    primary_color <- normalize_hex(input$primary_color)
    # Keep the existing renderer API stable; every former secondary accent now follows
    # the same user-selected template color.
    secondary_color <- primary_color
    signature_count <- as.integer(input$signature_count)

    signature_path_1 <- if (!is.null(input$signature_image_1)) {
      input$signature_image_1$datapath
    } else {
      NULL
    }

    signature_path_2 <- if (
      signature_count >= 2 &&
      !is.null(input$signature_image_2)
    ) {
      input$signature_image_2$datapath
    } else {
      NULL
    }

    signature_path_3 <- if (
      signature_count == 3 &&
      !is.null(input$signature_image_3)
    ) {
      input$signature_image_3$datapath
    } else {
      NULL
    }

    logo_path <- if (!is.null(input$logo_image)) {
      input$logo_image$datapath
    } else {
      NULL
    }

    preview_state$building <- TRUE
    on.exit({
      preview_state$building <- FALSE
    }, add = TRUE)

    # Remove the previous preview batch so Update Preview always shows current settings.
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
      message = "Generating PDF preview...",
      value = 0,
      {
        total_rows <- nrow(participant_data)

        for (row_number in seq_len(total_rows)) {
          participant_name <- as.character(
            participant_data[[1]][row_number]
          )

          paper_title <- as.character(
            participant_data[[2]][row_number]
          )

          # The third Excel column is optional. If it is missing, no footer is drawn.
          footer_text <- if (ncol(participant_data) >= 3) {
            as.character(participant_data[[3]][row_number])
          } else {
            ""
          }

          if (
            is.na(participant_name) ||
            trimws(participant_name) == ""
          ) {
            incProgress(1 / total_rows)
            next
          }

          if (is.na(paper_title)) {
            paper_title <- ""
          }

          if (is.na(footer_text)) {
            footer_text <- ""
          }

          participant_file_name <- safe_filename(
            participant_name
          )

          pdf_name <- sprintf(
            "%03d_Acceptance_Letter_%s.pdf",
            row_number,
            participant_file_name
          )

          output_file <- file.path(
            preview_root,
            pdf_name
          )

          create_acceptance_letter_pdf(
            participant_name = participant_name,
            paper_title = paper_title,
            footer_text = footer_text,
            letter_title = input$letter_title,
            body_text = input$body_text,
            signature_count = signature_count,
            signature_text_1 = input$signature_text_1,
            signature_image_1 = signature_path_1,
            signature_text_2 = if (signature_count >= 2) {
              input$signature_text_2
            } else {
              ""
            },
            signature_image_2 = signature_path_2,
            signature_text_3 = if (signature_count == 3) {
              input$signature_text_3
            } else {
              ""
            },
            signature_image_3 = signature_path_3,
            signature_width_cm = input$signature_width,
            signature_height_cm = input$signature_height,
            template = input$template,
            primary_color = primary_color,
            secondary_color = secondary_color,
            title_font_family = input$title_font_family,
            body_font_family = input$body_font_family,
            header_caption = input$header_caption,
            logo_image = logo_path,
            output_file = output_file
          )

          generated_files <- c(
            generated_files,
            pdf_name
          )

          participant_labels <- c(
            participant_labels,
            participant_name
          )

          incProgress(1 / total_rows)
        }
      }
    )

    if (length(generated_files) == 0) {
      stop("No PDF letters could be generated.")
    }

    # Create the ZIP only after all preview PDFs were generated successfully.
    zip_path <- file.path(
      preview_root,
      paste0(
        "Acceptance_Letters_",
        Sys.Date(),
        ".zip"
      )
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
    preview_state$generated_at <- Sys.time()
    # Force the currently displayed PDF to refresh even when its filename is unchanged.
    preview_state$revision <- preview_state$revision + 1L

    showNotification(
      "PDF preview generated successfully.",
      type = "message",
      duration = 3
    )
  }

  # Generate the first preview batch without downloading anything automatically.
  observeEvent(input$generate_letters, {
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

  # Rebuild the same preview area after the user changes template, colors, text, or signatures.
  # The Update button stays visible at all times so the workflow is easy to discover.
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

  # Clearly tell the user whether the preview matches the current settings.
  output$preview_status <- renderUI({
    if (!isTRUE(preview_state$generated)) {
      return(
        div(
          class = "status-pill status-idle",
          "No preview generated yet"
        )
      )
    }

    if (isTRUE(preview_state$stale)) {
      return(
        div(
          class = "status-pill status-stale",
          "Changes pending - click Update Preview"
        )
      )
    }

    div(
      class = "status-pill status-ready",
      "Preview is up to date"
    )
  })

  # After generation, let the user choose which participant PDF to inspect.
  output$preview_selector_ui <- renderUI({
    if (!isTRUE(preview_state$generated)) {
      return(NULL)
    }

    choices <- stats::setNames(
      preview_state$files,
      preview_state$labels
    )

    div(
      class = "preview-selector",
      selectInput(
        inputId = "preview_file",
        label = "Preview Participant",
        choices = choices,
        selected = preview_state$files[1]
      )
    )
  })

  # Embed the selected PDF directly inside the right side of the Shiny app.
  output$pdf_preview <- renderUI({
    if (!isTRUE(preview_state$generated)) {
      return(
        div(
          class = "preview-placeholder",
          div(
            strong("Your PDF preview will appear here."),
            span(
              paste(
                "Upload the Excel file, configure the letter,",
                "and click Generate PDF Letters."
              )
            )
          )
        )
      )
    }

    selected_file <- input$preview_file

    if (
      is.null(selected_file) ||
      !(selected_file %in% preview_state$files)
    ) {
      selected_file <- preview_state$files[1]
    }

    # Add a revision query parameter to defeat browser/PDF-viewer caching.
    # When Update Preview finishes, revision changes and Shiny immediately
    # rebuilds this iframe with a new URL for the same participant PDF.
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
      title = "Acceptance Letter PDF Preview"
    )
  })

  # Only offer the ZIP download when the displayed preview matches current settings.
  output$download_button_ui <- renderUI({
    if (
      !isTRUE(preview_state$generated) ||
      isTRUE(preview_state$stale)
    ) {
      return(NULL)
    }

    downloadButton(
      outputId = "download_letters",
      label = "Download PDF Letters (ZIP)"
    )
  })

  # Copy the already-previewed ZIP into the browser download when requested.
  output$download_letters <- downloadHandler(
    filename = function() {
      paste0(
        "Acceptance_Letters_",
        Sys.Date(),
        ".zip"
      )
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


# ============================================================
# RUN APPLICATION
# Starts the complete Shiny app defined above.
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
