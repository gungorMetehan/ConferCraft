# ============================================================
# CONFERCRAFT - HOME PAGE
# ============================================================

library(shiny)

# ============================================================
# APP CONFIGURATION
# ============================================================

ACCEPTANCE_LETTER_URL <- Sys.getenv(
  "CONFERCRAFT_ACCEPTANCE_URL",
  unset = "https://gungormetehan-acceptance-letter-generator.share.connect.posit.cloud/"
)

CERTIFICATE_GENERATOR_URL <- Sys.getenv(
  "CONFERCRAFT_CERTIFICATE_URL",
  unset = "https://gungormetehan-certificate-generator.share.connect.posit.cloud/"
)

GITHUB_URL <- "https://github.com/gungorMetehan/ConferCraft"

# ============================================================
# SMALL UI HELPERS
# ============================================================

tool_icon <- function(type = c("letter", "certificate")) {
  type <- match.arg(type)

  if (identical(type, "letter")) {
    return(
      HTML(
        paste0(
          "<svg viewBox='0 0 24 24' aria-hidden='true'>",
          "<path d='M6.5 2.75h7.1L18.5 7.6v13.65H6.5V2.75Zm7.75 1.9v3.6h3.55l-3.55-3.6ZM8.25 10.3h8.5v1.35h-8.5V10.3Zm0 3.25h8.5v1.35h-8.5v-1.35Zm0 3.25h5.8v1.35h-5.8V16.8Z'/>",
          "</svg>"
        )
      )
    )
  }

  HTML(
    paste0(
      "<svg viewBox='0 0 24 24' aria-hidden='true'>",
      "<path d='M12 2.25a5.1 5.1 0 1 0 0 10.2 5.1 5.1 0 0 0 0-10.2Zm0 1.5a3.6 3.6 0 1 1 0 7.2 3.6 3.6 0 0 1 0-7.2Zm-3.05 8.9-1.3 8.1 4.35-2.5 4.35 2.5-1.3-8.1a6.5 6.5 0 0 1-1.42.66l.72 4.54L12 16l-2.35 1.35.72-4.54a6.5 6.5 0 0 1-1.42-.66Z'/>",
      "</svg>"
    )
  )
}

feature_chip <- function(text) {
  span(class = "feature-chip", text)
}

tool_card <- function(
    href,
    type,
    eyebrow,
    title,
    description,
    chips) {

  tags$a(
    class = paste("tool-card", paste0("tool-card-", type)),
    href = href,
    `aria-label` = paste("Open", title),

    div(
      class = "tool-card-top",
      div(class = "tool-icon", tool_icon(type)),
      div(class = "tool-arrow", HTML("&rarr;"))
    ),

    div(class = "tool-eyebrow", eyebrow),
    h2(class = "tool-title", title),
    p(class = "tool-description", description),

    div(
      class = "feature-list",
      lapply(chips, feature_chip)
    ),

    div(
      class = "tool-open",
      span("Open tool"),
      span(class = "tool-open-arrow", HTML("&rarr;"))
    )
  )
}

# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$title("ConferCraft | Academic Document Tools"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$meta(
      name = "description",
      content = "ConferCraft creates polished, personalized academic event documents from structured participant data."
    ),

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

    # Restore the user's last selected theme before the page paints.
    tags$script(
      HTML(
        "
        (function() {
          try {
            if (localStorage.getItem('confercraft-theme') === 'dark') {
              document.documentElement.classList.add('confercraft-dark');
            }
          } catch (error) {}
        })();
        "
      )
    ),

    tags$style(
      HTML(
        "
        :root {
          --ink: #151918;
          --ink-soft: #303735;
          --paper: #fbfcfb;
          --card: rgba(255, 255, 255, 0.88);
          --card-solid: #ffffff;
          --teal: #0f7a6c;
          --teal-dark: #0a5e54;
          --teal-soft: #e9f6f3;
          --teal-soft-2: #f3faf8;
          --muted: #69716f;
          --line: #dfe9e6;
          --line-strong: #cadbd6;
          --shadow: 0 22px 70px rgba(16, 75, 65, 0.10);
          --shadow-soft: 0 10px 34px rgba(16, 75, 65, 0.065);
        }

        * { box-sizing: border-box; }

        html {
          min-height: 100%;
          scroll-behavior: smooth;
          background: var(--paper);
        }

        html, body {
          min-height: 100%;
        }

        body {
          min-height: 100vh;
          margin: 0;
          color: var(--ink);
          font-family: 'IBM Plex Sans', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          background:
            radial-gradient(circle at 84% 8%, rgba(15, 122, 108, 0.10), transparent 27%),
            radial-gradient(circle at 8% 72%, rgba(92, 181, 167, 0.08), transparent 26%),
            linear-gradient(180deg, #ffffff 0%, #fbfcfb 46%, #f4faf8 100%);
          background-attachment: fixed;
        }

        body::before {
          content: '';
          position: fixed;
          inset: 0;
          pointer-events: none;
          opacity: 0.44;
          background-image:
            linear-gradient(rgba(15, 122, 108, 0.024) 1px, transparent 1px),
            linear-gradient(90deg, rgba(15, 122, 108, 0.024) 1px, transparent 1px);
          background-size: 44px 44px;
          mask-image: linear-gradient(to bottom, black, transparent 58%);
          -webkit-mask-image: linear-gradient(to bottom, black, transparent 58%);
        }

        button, a {
          font-family: 'IBM Plex Sans', system-ui, sans-serif;
        }

        a { color: inherit; }

        .container-fluid {
          padding: 0;
        }

        .home-shell {
          width: min(1180px, calc(100% - 40px));
          margin: 0 auto;
          padding: 20px 0 30px;
          position: relative;
          z-index: 1;
        }

        .home-nav {
          position: sticky;
          top: 16px;
          z-index: 20;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 18px;
          min-height: 64px;
          padding: 10px 12px 10px 18px;
          border: 1px solid rgba(202, 219, 214, 0.92);
          border-radius: 20px;
          background: rgba(255, 255, 255, 0.82);
          box-shadow: var(--shadow-soft);
          backdrop-filter: blur(18px);
          -webkit-backdrop-filter: blur(18px);
        }

        .nav-brand {
          display: inline-flex;
          align-items: center;
          gap: 10px;
          text-decoration: none;
          min-width: 0;
        }

        .brand-mark {
          width: 34px;
          height: 34px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          flex: 0 0 auto;
          border-radius: 11px;
          background: var(--teal);
          color: white;
          box-shadow: 0 8px 20px rgba(15, 122, 108, 0.20);
        }

        .brand-mark svg {
          width: 18px;
          height: 18px;
          fill: currentColor;
        }

        .brand-word {
          font-family: 'DM Serif Display', serif;
          font-size: 23px;
          font-weight: 400;
          letter-spacing: -0.02em;
          line-height: 1;
        }

        .nav-actions {
          display: flex;
          align-items: center;
          gap: 8px;
        }

        .nav-text-link {
          min-height: 42px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          padding: 0 14px;
          border-radius: 999px;
          color: #52605d;
          font-size: 12px;
          font-weight: 700;
          text-decoration: none;
          transition: background .16s ease, color .16s ease;
        }

        .nav-text-link:hover,
        .nav-text-link:focus {
          color: var(--teal-dark);
          background: var(--teal-soft);
          text-decoration: none;
          outline: none;
        }

        .nav-icon-button {
          width: 42px;
          height: 42px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          border: 1px solid var(--line-strong);
          border-radius: 50%;
          background: rgba(255,255,255,.94);
          color: #18201e;
          cursor: pointer;
          text-decoration: none;
          box-shadow: none;
          transition: transform .16s ease, color .16s ease, border-color .16s ease, background .16s ease;
        }

        .nav-icon-button:hover,
        .nav-icon-button:focus {
          color: var(--teal);
          border-color: #9fd0c5;
          transform: translateY(-1px);
          text-decoration: none;
          outline: none;
        }

        .nav-icon-button svg {
          width: 20px;
          height: 20px;
          display: block;
          fill: currentColor;
        }

        .theme-icon-sun { display: none !important; }

        .hero {
          max-width: 900px;
          margin: 0 auto;
          padding: 118px 16px 76px;
          text-align: center;
        }

        .hero-kicker {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 7px 12px;
          border: 1px solid #c7e7df;
          border-radius: 999px;
          background: rgba(233, 246, 243, 0.88);
          color: var(--teal-dark);
          font-size: 11px;
          font-weight: 700;
          letter-spacing: .11em;
          text-transform: uppercase;
        }

        .hero-kicker-dot {
          width: 7px;
          height: 7px;
          border-radius: 50%;
          background: var(--teal);
          box-shadow: 0 0 0 5px rgba(15, 122, 108, 0.08);
        }

        .hero-title {
          max-width: 850px;
          margin: 24px auto 0;
          font-family: 'DM Serif Display', serif;
          font-size: clamp(47px, 7vw, 78px);
          line-height: .99;
          font-weight: 400;
          letter-spacing: -0.045em;
          color: #111513;
        }

        .hero-title-accent {
          color: var(--teal);
          font-style: italic;
        }

        .hero-copy {
          max-width: 690px;
          margin: 25px auto 0;
          color: var(--muted);
          font-size: clamp(15px, 1.8vw, 18px);
          line-height: 1.75;
        }

        .hero-actions {
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 10px;
          flex-wrap: wrap;
          margin-top: 31px;
        }

        .primary-cta,
        .secondary-cta {
          min-height: 46px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          gap: 8px;
          padding: 0 18px;
          border-radius: 999px;
          font-size: 13px;
          font-weight: 700;
          text-decoration: none;
          transition: transform .16s ease, box-shadow .16s ease, background .16s ease, border-color .16s ease;
        }

        .primary-cta {
          color: #ffffff;
          background: var(--teal);
          border: 1px solid var(--teal);
          box-shadow: 0 10px 26px rgba(15, 122, 108, .18);
        }

        .primary-cta:hover,
        .primary-cta:focus {
          color: #ffffff;
          background: var(--teal-dark);
          border-color: var(--teal-dark);
          transform: translateY(-1px);
          box-shadow: 0 14px 30px rgba(10, 94, 84, .22);
          text-decoration: none;
          outline: none;
        }

        .secondary-cta {
          color: #2c3633;
          background: rgba(255,255,255,.82);
          border: 1px solid var(--line-strong);
        }

        .secondary-cta:hover,
        .secondary-cta:focus {
          color: var(--teal-dark);
          border-color: #9fd0c5;
          background: #ffffff;
          transform: translateY(-1px);
          text-decoration: none;
          outline: none;
        }

        .tools-section {
          scroll-margin-top: 100px;
          padding: 8px 0 86px;
        }

        .section-heading {
          display: flex;
          align-items: flex-end;
          justify-content: space-between;
          gap: 24px;
          margin-bottom: 22px;
        }

        .section-kicker {
          margin: 0 0 7px;
          color: var(--teal);
          font-size: 11px;
          font-weight: 700;
          letter-spacing: .12em;
          text-transform: uppercase;
        }

        .section-title {
          margin: 0;
          font-family: 'DM Serif Display', serif;
          font-size: clamp(30px, 4vw, 42px);
          font-weight: 400;
          letter-spacing: -0.025em;
        }

        .section-copy {
          max-width: 470px;
          margin: 0 0 3px;
          color: var(--muted);
          font-size: 13px;
          line-height: 1.65;
          text-align: right;
        }

        .tool-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 18px;
        }

        .tool-card {
          min-height: 390px;
          display: flex;
          flex-direction: column;
          position: relative;
          overflow: hidden;
          padding: 27px;
          border: 1px solid var(--line);
          border-radius: 26px;
          background: var(--card);
          color: var(--ink);
          box-shadow: var(--shadow-soft);
          text-decoration: none;
          backdrop-filter: blur(14px);
          -webkit-backdrop-filter: blur(14px);
          transition: transform .20s ease, box-shadow .20s ease, border-color .20s ease;
        }

        .tool-card::before {
          content: '';
          position: absolute;
          left: 0;
          right: 0;
          top: 0;
          height: 4px;
          background: linear-gradient(90deg, var(--teal), #5cb5a7, #b8ded6);
        }

        .tool-card::after {
          content: '';
          position: absolute;
          width: 230px;
          height: 230px;
          right: -90px;
          top: -100px;
          border-radius: 50%;
          background: radial-gradient(circle, rgba(15,122,108,.12), rgba(15,122,108,0) 68%);
          pointer-events: none;
        }

        .tool-card:hover,
        .tool-card:focus {
          color: var(--ink);
          transform: translateY(-5px);
          border-color: #b7d9d0;
          box-shadow: 0 28px 80px rgba(16, 75, 65, 0.14);
          text-decoration: none;
          outline: none;
        }

        .tool-card-top {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 18px;
          margin-bottom: 34px;
        }

        .tool-icon {
          width: 54px;
          height: 54px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          border: 1px solid #c9e6df;
          border-radius: 17px;
          background: var(--teal-soft);
          color: var(--teal-dark);
        }

        .tool-icon svg {
          width: 27px;
          height: 27px;
          fill: currentColor;
        }

        .tool-arrow {
          width: 39px;
          height: 39px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          border: 1px solid var(--line-strong);
          border-radius: 50%;
          color: #52605d;
          background: rgba(255,255,255,.72);
          font-size: 20px;
          transition: transform .18s ease, color .18s ease, border-color .18s ease;
        }

        .tool-card:hover .tool-arrow,
        .tool-card:focus .tool-arrow {
          color: var(--teal);
          border-color: #9fd0c5;
          transform: translateX(3px);
        }

        .tool-eyebrow {
          color: var(--teal);
          font-size: 11px;
          font-weight: 700;
          letter-spacing: .105em;
          text-transform: uppercase;
          margin-bottom: 9px;
        }

        .tool-title {
          margin: 0;
          font-family: 'DM Serif Display', serif;
          font-size: clamp(29px, 3.2vw, 38px);
          line-height: 1.06;
          font-weight: 400;
          letter-spacing: -0.025em;
        }

        .tool-description {
          max-width: 500px;
          margin: 16px 0 0;
          color: var(--muted);
          font-size: 14px;
          line-height: 1.72;
        }

        .feature-list {
          display: flex;
          flex-wrap: wrap;
          gap: 7px;
          margin-top: 22px;
        }

        .feature-chip {
          display: inline-flex;
          align-items: center;
          min-height: 29px;
          padding: 0 10px;
          border-radius: 999px;
          border: 1px solid #d8e8e4;
          background: #f8fbfa;
          color: #56635f;
          font-size: 10.5px;
          font-weight: 600;
        }

        .tool-open {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 14px;
          margin-top: auto;
          padding-top: 28px;
          color: var(--teal-dark);
          font-size: 12px;
          font-weight: 700;
        }

        .tool-open-arrow {
          font-size: 18px;
          transition: transform .18s ease;
        }

        .tool-card:hover .tool-open-arrow,
        .tool-card:focus .tool-open-arrow {
          transform: translateX(4px);
        }

        .home-footer {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 20px;
          padding: 22px 4px 12px;
          border-top: 1px solid var(--line);
          color: var(--muted);
          font-size: 11px;
        }

        .footer-brand {
          color: #414b48;
          font-weight: 700;
        }

        .footer-links {
          display: flex;
          align-items: center;
          gap: 16px;
        }

        .footer-links a {
          color: var(--muted);
          text-decoration: none;
        }

        .footer-links a:hover,
        .footer-links a:focus {
          color: var(--teal);
          text-decoration: none;
          outline: none;
        }

        /* DARK MODE */
        html.confercraft-dark {
          color-scheme: dark;
          --ink: #f1f4f3;
          --ink-soft: #d4dcda;
          --paper: #0d1014;
          --card: rgba(18, 24, 28, 0.90);
          --card-solid: #14191d;
          --teal: #25b6a0;
          --teal-dark: #73d3c3;
          --teal-soft: #15312d;
          --teal-soft-2: #111c1a;
          --muted: #9ba6a3;
          --line: #29332f;
          --line-strong: #37443f;
          --shadow: 0 22px 70px rgba(0, 0, 0, 0.26);
          --shadow-soft: 0 10px 34px rgba(0, 0, 0, 0.20);
        }

        html.confercraft-dark body {
          color: var(--ink);
          background:
            radial-gradient(circle at 84% 8%, rgba(37, 182, 160, 0.10), transparent 27%),
            radial-gradient(circle at 8% 72%, rgba(37, 182, 160, 0.05), transparent 26%),
            linear-gradient(180deg, #0d1014 0%, #0f1317 48%, #10161a 100%);
        }

        html.confercraft-dark body::before {
          background-image:
            linear-gradient(rgba(115, 211, 195, 0.026) 1px, transparent 1px),
            linear-gradient(90deg, rgba(115, 211, 195, 0.026) 1px, transparent 1px);
        }

        html.confercraft-dark .home-nav {
          background: rgba(17, 22, 26, 0.84);
          border-color: #2b3733;
        }

        html.confercraft-dark .brand-word,
        html.confercraft-dark .hero-title,
        html.confercraft-dark .section-title,
        html.confercraft-dark .tool-title {
          color: #f5f7f6;
        }

        html.confercraft-dark .nav-text-link {
          color: #aeb8b5;
        }

        html.confercraft-dark .nav-icon-button,
        html.confercraft-dark .secondary-cta,
        html.confercraft-dark .tool-arrow {
          background: #1a2126;
          color: #edf2f0;
          border-color: #37443f;
        }

        html.confercraft-dark .secondary-cta:hover,
        html.confercraft-dark .secondary-cta:focus {
          color: var(--teal-dark);
          border-color: #4e766e;
          background: #1d272b;
        }

        html.confercraft-dark .hero-kicker {
          background: #15312d;
          border-color: #294e47;
          color: #8bdbcd;
        }

        html.confercraft-dark .tool-card {
          border-color: #293733;
        }

        html.confercraft-dark .tool-card:hover,
        html.confercraft-dark .tool-card:focus {
          border-color: #3e625a;
        }

        html.confercraft-dark .tool-icon {
          background: #15312d;
          border-color: #294e47;
          color: #8bdbcd;
        }

        html.confercraft-dark .feature-chip {
          border-color: #30413d;
          background: #12191c;
          color: #abb6b3;
        }

        html.confercraft-dark .footer-brand {
          color: #d7dfdd;
        }

        html.confercraft-dark .theme-icon-moon { display: none !important; }
        html.confercraft-dark .theme-icon-sun { display: block !important; }

        @media (max-width: 860px) {
          .home-shell {
            width: min(100% - 26px, 1180px);
            padding-top: 13px;
          }

          .home-nav { top: 10px; }

          .nav-text-link { display: none; }

          .hero {
            padding: 94px 8px 62px;
          }

          .hero-title {
            font-size: clamp(43px, 13vw, 67px);
          }

          .section-heading {
            align-items: flex-start;
            flex-direction: column;
            gap: 10px;
          }

          .section-copy {
            max-width: 620px;
            text-align: left;
          }

          .tool-grid {
            grid-template-columns: 1fr;
          }
        }

        @media (max-width: 560px) {
          .home-nav {
            min-height: 58px;
            padding: 8px 9px 8px 13px;
            border-radius: 17px;
          }

          .brand-mark {
            width: 31px;
            height: 31px;
            border-radius: 10px;
          }

          .brand-word { font-size: 21px; }
          .nav-icon-button { width: 39px; height: 39px; }

          .hero {
            padding: 78px 4px 54px;
          }

          .hero-title {
            font-size: clamp(39px, 12.2vw, 56px);
          }

          .hero-copy {
            font-size: 14px;
            line-height: 1.7;
          }

          .primary-cta,
          .secondary-cta {
            width: 100%;
          }

          .tools-section { padding-bottom: 58px; }

          .tool-card {
            min-height: 365px;
            padding: 22px;
            border-radius: 22px;
          }

          .tool-card-top { margin-bottom: 28px; }
          .tool-title { font-size: 31px; }

          .home-footer {
            align-items: flex-start;
            flex-direction: column;
          }
        }
        "
      )
    ),

    tags$script(
      HTML(
        "
        (function() {
          function isDark() {
            return document.documentElement.classList.contains('confercraft-dark');
          }

          function syncThemeButton() {
            var button = document.getElementById('theme_toggle');
            if (!button) return;
            var label = isDark() ? 'Switch to light mode' : 'Switch to dark mode';
            button.setAttribute('title', label);
            button.setAttribute('aria-label', label);
          }

          document.addEventListener('DOMContentLoaded', syncThemeButton);

          document.addEventListener('click', function(event) {
            var button = event.target.closest ? event.target.closest('#theme_toggle') : null;
            if (!button) return;

            event.preventDefault();
            var nextDark = !isDark();
            document.documentElement.classList.toggle('confercraft-dark', nextDark);

            try {
              localStorage.setItem('confercraft-theme', nextDark ? 'dark' : 'light');
            } catch (error) {}

            syncThemeButton();
          });
        })();
        "
      )
    )
  ),

  div(
    class = "home-shell",

    # ----------------------------------------------------------
    # NAVIGATION
    # ----------------------------------------------------------
    div(
      class = "home-nav",

      tags$a(
        class = "nav-brand",
        href = "#",
        `aria-label` = "ConferCraft home",
        span(
          class = "brand-mark",
          HTML(
            paste0(
              "<svg viewBox='0 0 24 24' aria-hidden='true'>",
              "<path d='M12 2.2c.43 2.78 1.5 4.87 3.18 6.22 1.3 1.04 3.04 1.73 5.22 2.08-2.18.35-3.92 1.04-5.22 2.08-1.68 1.35-2.75 3.44-3.18 6.22-.43-2.78-1.5-4.87-3.18-6.22C7.52 11.54 5.78 10.85 3.6 10.5c2.18-.35 3.92-1.04 5.22-2.08C10.5 7.07 11.57 4.98 12 2.2Zm7.3 13.4c.18 1.16.63 2.03 1.33 2.6.55.44 1.27.73 2.17.88-.9.14-1.62.43-2.17.87-.7.57-1.15 1.44-1.33 2.6-.18-1.16-.63-2.03-1.33-2.6-.55-.44-1.27-.73-2.17-.87.9-.15 1.62-.44 2.17-.88.7-.57 1.15-1.44 1.33-2.6Z'/>",
              "</svg>"
            )
          )
        ),
        span(class = "brand-word", "ConferCraft")
      ),

      div(
        class = "nav-actions",
        tags$a(class = "nav-text-link", href = "#tools", "Tools"),
        tags$a(
          class = "nav-icon-button",
          href = GITHUB_URL,
          target = "_blank",
          rel = "noopener noreferrer",
          title = "ConferCraft on GitHub",
          `aria-label` = "ConferCraft on GitHub",
          HTML(
            "<svg viewBox='0 0 24 24' aria-hidden='true'><path d='M12 .7C5.65.7.5 5.85.5 12.2c0 5.08 3.29 9.39 7.86 10.91.58.1.79-.25.79-.56 0-.28-.01-1.2-.02-2.18-3.2.7-3.88-1.36-3.88-1.36-.52-1.33-1.28-1.68-1.28-1.68-1.05-.72.08-.7.08-.7 1.16.08 1.77 1.19 1.77 1.19 1.03 1.77 2.7 1.26 3.36.96.1-.75.4-1.26.73-1.55-2.55-.29-5.24-1.28-5.24-5.69 0-1.26.45-2.28 1.19-3.09-.12-.29-.52-1.46.11-3.05 0 0 .97-.31 3.17 1.18A11.1 11.1 0 0 1 12 6.2c.98 0 1.96.13 2.88.39 2.2-1.49 3.17-1.18 3.17-1.18.63 1.59.23 2.76.11 3.05.74.81 1.19 1.83 1.19 3.09 0 4.42-2.69 5.39-5.25 5.68.41.36.78 1.06.78 2.14 0 1.55-.01 2.79-.01 3.17 0 .31.21.67.79.56 4.56-1.52 7.85-5.83 7.85-10.9C23.5 5.85 18.35.7 12 .7Z'/></svg>"
          )
        ),
        tags$button(
          id = "theme_toggle",
          class = "nav-icon-button",
          type = "button",
          title = "Switch to dark mode",
          `aria-label` = "Switch to dark mode",
          HTML(
            paste0(
              "<svg class='theme-icon-moon' viewBox='0 0 24 24' aria-hidden='true'><path d='M20.15 15.42A8.1 8.1 0 0 1 8.58 3.85 8.65 8.65 0 1 0 20.15 15.42Zm-8.2 5.03A6.95 6.95 0 0 1 6.44 9.27a6.9 6.9 0 0 1 .47-3.34 9.25 9.25 0 0 0 10.66 10.66 6.91 6.91 0 0 1-5.62 3.86Z'/></svg>",
              "<svg class='theme-icon-sun' viewBox='0 0 24 24' aria-hidden='true'><path d='M12 7.25A4.75 4.75 0 1 0 12 16.75 4.75 4.75 0 0 0 12 7.25Zm0 8A3.25 3.25 0 1 1 12 8.75a3.25 3.25 0 0 1 0 6.5ZM12 1.5a.75.75 0 0 1 .75.75v2a.75.75 0 0 1-1.5 0v-2A.75.75 0 0 1 12 1.5Zm0 17.5a.75.75 0 0 1 .75.75v2a.75.75 0 0 1-1.5 0v-2A.75.75 0 0 1 12 19Z'/></svg>"
            )
          )
        )
      )
    ),

    # ----------------------------------------------------------
    # HERO
    # ----------------------------------------------------------
    tags$section(
      class = "hero",
      div(
        class = "hero-kicker",
        span(class = "hero-kicker-dot"),
        "Academic document automation"
      ),
      h1(
        class = "hero-title",
        "Conference documents, ",
        span(class = "hero-title-accent", "without the repetitive work." )
      ),
      p(
        class = "hero-copy",
        paste(
          "ConferCraft turns structured participant data into polished, personalized",
          "conference documents — ready to preview, customize, and export in a few clicks."
        )
      ),
      div(
        class = "hero-actions",
        tags$a(
          class = "primary-cta",
          href = "#tools",
          span("Choose a tool"),
          span(HTML("&darr;"))
        ),
        tags$a(
          class = "secondary-cta",
          href = GITHUB_URL,
          target = "_blank",
          rel = "noopener noreferrer",
          "View on GitHub"
        )
      )
    ),

    # ----------------------------------------------------------
    # TOOLS
    # ----------------------------------------------------------
    tags$section(
      id = "tools",
      class = "tools-section",

      div(
        class = "section-heading",
        div(
          p(class = "section-kicker", "ConferCraft tools"),
          h2(class = "section-title", "Two focused tools. One workflow.")
        ),
        p(
          class = "section-copy",
          "Start with an Excel list, shape the document visually, preview the result, then export the full batch."
        )
      ),

      div(
        class = "tool-grid",

        tool_card(
          href = ACCEPTANCE_LETTER_URL,
          type = "letter",
          eyebrow = "Correspondence",
          title = "Acceptance Letter Generator",
          description = paste(
            "Create personalized acceptance letters from participant and paper data.",
            "Use rich text, open fonts, named SVG templates, signatures, and batch PDF export."
          ),
          chips = c(
            "Excel import",
            "8 templates",
            "Rich text + math",
            "Bulk PDF"
          )
        ),

        tool_card(
          href = CERTIFICATE_GENERATOR_URL,
          type = "certificate",
          eyebrow = "Recognition",
          title = "Certificate Generator",
          description = paste(
            "Create polished one-page landscape certificates from participant data.",
            "Control templates, color, fonts, logos, signatures, name size, and batch export."
          ),
          chips = c(
            "Excel import",
            "8 templates",
            "Flexible layout",
            "ZIP export"
          )
        )
      )
    ),

    # ----------------------------------------------------------
    # FOOTER
    # ----------------------------------------------------------
    tags$footer(
      class = "home-footer",
      div(
        span(class = "footer-brand", "ConferCraft"),
        "  ·  Academic event document tools"
      ),
      div(
        class = "footer-links",
        tags$a(href = ACCEPTANCE_LETTER_URL, "Acceptance Letters"),
        tags$a(href = CERTIFICATE_GENERATOR_URL, "Certificates"),
        tags$a(
          href = GITHUB_URL,
          target = "_blank",
          rel = "noopener noreferrer",
          "GitHub"
        )
      )
    )
  )
)

# The landing page is intentionally static; Shiny only serves the UI.
server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
