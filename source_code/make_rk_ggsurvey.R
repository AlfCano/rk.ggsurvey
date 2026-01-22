local({
  # =========================================================================================
  # 1. Package Definition and Metadata
  # =========================================================================================
  require(rkwarddev)
  rkwarddev.required("0.08-1")

  package_about <- rk.XML.about(
    name = "rk.ggsurvey",
    author = person(
      given = "Alfonso",
      family = "Cano",
      email = "alfonso.cano@correo.buap.mx",
      role = c("aut", "cre")
    ),
    about = list(
      desc = "A plugin package to analyze complex survey designs with custom plugins and the 'ggsurvey' package.",
      version = "0.1.9",
      url = "https://github.com/AlfCano/rk.ggsurvey",
      license = "GPL (>= 3)"
    )
  )

  # =========================================================================================
  # 2. Reusable UI Components & JS Helpers
  # =========================================================================================

  js_helpers <- '
    function getColumnName(fullName) {
        if (!fullName) return "";
        var lastBracketPos = fullName.lastIndexOf("[[");
        if (lastBracketPos > -1) {
            var lastPart = fullName.substring(lastBracketPos);
            var match = lastPart.match(/\\[\\[\\"(.*?)\\"\\]\\]/);
            if (match) {
                return match[1];
            }
        }
        if (fullName.indexOf("$") > -1) {
            return fullName.substring(fullName.lastIndexOf("$") + 1);
        } else {
            return fullName;
        }
    }
  '

  # --- Individual Label Inputs ---
  # Defined individually so we can mix-and-match for different plugins
  inp_title <- rk.XML.input(label = "Plot Title", id.name = "plot_title")
  inp_subtitle <- rk.XML.input(label = "Plot Subtitle", id.name = "plot_subtitle")
  inp_xlab <- rk.XML.input(label = "X-axis Label (blank for auto)", id.name = "plot_xlab")
  spin_xlab_wrap <- rk.XML.spinbox(label = "Wrap X-axis Label at (chars, 0 to disable)", id.name = "plot_xlab_wrap", min = 0, max = 100, initial = 0)
  inp_ylab <- rk.XML.input(label = "Y-axis Label", id.name = "plot_ylab")
  spin_ylab_wrap <- rk.XML.spinbox(label = "Wrap Y-axis Label at (chars, 0 to disable)", id.name = "plot_ylab_wrap", min = 0, max = 100, initial = 0)
  inp_leg_title <- rk.XML.input(label = "Legend Title (blank for auto)", id.name = "plot_legend_title")

  # The specific input for cleaning (Only for svyby plots)
  inp_clean_leg <- rk.XML.input(label = "Remove text from Legend Labels (e.g. 'VarName')", id.name = "clean_legend_prefix")

  spin_leg_title_wrap <- rk.XML.spinbox(label = "Wrap Legend Title at (chars, 0 to disable)", id.name = "legend_title_wrap_width", min = 0, max = 100, initial = 20)
  spin_leg_wrap <- rk.XML.spinbox(label = "Wrap Legend Labels at (chars, 0 to disable)", id.name = "legend_wrap_width", min = 0, max = 100, initial = 20)
  inp_caption <- rk.XML.input(label = "Plot Caption", id.name = "plot_caption")

  # --- Label Tabs Definitions ---

  # 1. Extended Tab (For Line/Means - Includes Cleaning)
  labels_tab_extended <- rk.XML.col(
    inp_title, inp_subtitle,
    inp_xlab, spin_xlab_wrap,
    inp_ylab, spin_ylab_wrap,
    inp_leg_title, inp_clean_leg, # <--- Included here
    spin_leg_title_wrap, spin_leg_wrap,
    inp_caption
  )

  # 2. Simple Tab (For Bar/Box/Hist - No Cleaning)
  labels_tab_simple <- rk.XML.col(
    inp_title, inp_subtitle,
    inp_xlab, spin_xlab_wrap,
    inp_ylab, spin_ylab_wrap,
    inp_leg_title, # <--- No cleaning input
    spin_leg_title_wrap, spin_leg_wrap,
    inp_caption
  )

  # --- Theme Tab ---
  theme_tab <- rk.XML.col(
      rk.XML.spinbox(label="Overall text size relative adjustment", id.name="theme_text_rel", min=0.1, max=5, initial=1, real=TRUE),
      rk.XML.spinbox(label="Plot title size relative adjustment", id.name="theme_title_rel", min=0.1, max=5, initial=1.2, real=TRUE),
      rk.XML.spinbox(label="Legend text size relative adjustment", id.name="theme_legend_rel", min=0.1, max=5, initial=0.8, real=TRUE),
      rk.XML.dropdown(label="Legend Position", id.name="theme_legend_pos", options=list(
          "Right (Default)"=list(val="right", chk=TRUE), "Left"=list(val="left"),
          "Top"=list(val="top"), "Bottom"=list(val="bottom"), "None"=list(val="none")
      )),
      rk.XML.frame(label="X-Axis Text", child=rk.XML.row(
        rk.XML.spinbox(label="Angle", id.name="theme_x_angle", min=0, max=90, initial=0),
        rk.XML.spinbox(label="H-Just", id.name="theme_x_hjust", min=0, max=1, initial=0.5, real=TRUE),
        rk.XML.spinbox(label="V-Just", id.name="theme_x_vjust", min=0, max=1, initial=0.5, real=TRUE)
      )),
      rk.XML.frame(label="X-Axis Value Labels", child=rk.XML.spinbox(label="Wrap at (chars)", id.name="theme_x_val_wrap", min=0, max=100, initial=0)),
      rk.XML.frame(label="Y-Axis Value Labels", child=rk.XML.spinbox(label="Wrap at (chars)", id.name="theme_y_val_wrap", min=0, max=100, initial=0))
  )

  # --- Device Tab ---
  device_tab <- rk.XML.col(
    rk.XML.dropdown(label = "Device type", id.name = "device_type", options = list("PNG" = list(val = "PNG", chk = TRUE), "SVG" = list(val = "SVG"), "JPG" = list(val = "JPG"))),
    rk.XML.spinbox(label = "JPG Quality (0-100)", id.name = "jpg_quality", min = 0, max = 100, initial = 75),
    rk.XML.spinbox(label = "Width (px)", id.name = "dev_width", min = 100, max = 4000, initial = 1024),
    rk.XML.spinbox(label = "Height (px)", id.name = "dev_height", min = 100, max = 4000, initial = 724),
    rk.XML.spinbox(label = "Resolution (ppi)", id.name = "dev_res", min = 50, max = 600, initial = 200),
    rk.XML.dropdown(label = "Background", id.name = "dev_bg", options = list("Transparent" = list(val = "transparent", chk = TRUE), "White" = list(val = "white")))
  )

  # --- Shared Helpers ---
  color_palette_dropdown <- rk.XML.dropdown(label = "Color Palette (ColorBrewer)", id.name = "palette_input", options = list(
    "Default (Set1)" = list(val = "Set1", chk = TRUE), "Accent" = list(val = "Accent"), "Dark2" = list(val = "Dark2"),
    "Paired" = list(val = "Paired"), "Pastel1" = list(val = "Pastel1"), "Pastel2" = list(val = "Pastel2"),
    "Set2" = list(val = "Set2"), "Set3" = list(val = "Set3"), "Blues" = list(val = "Blues"),
    "Greens" = list(val = "Greens"), "Oranges" = list(val = "Oranges"), "Reds" = list(val = "Reds"),
    "Purples" = list(val = "Purples"), "RdYlBu" = list(val = "RdYlBu"), "Spectral" = list(val = "Spectral")
  ))

  ordering_frame <- rk.XML.frame(
    label = "X-axis Ordering",
    child = rk.XML.col(
        rk.XML.cbox(label = "Order X-axis by estimate value", id.name = "order_x_est", value = "1"),
        rk.XML.cbox(label = "Invert final order", id.name = "invert_order", value = "1")
    )
  )

  js_print_graph <- '
    if(!is_preview){
      var graph_options = [];
      graph_options.push("device.type=\\"" + getValue("device_type") + "\\"");
      graph_options.push("width=" + getValue("dev_width"));
      graph_options.push("height=" + getValue("dev_height"));
      graph_options.push("pointsize=10.0");
      graph_options.push("res=" + getValue("dev_res"));
      graph_options.push("bg=\\"" + getValue("dev_bg") + "\\"");
      if(getValue("device_type") === "JPG"){
        graph_options.push("quality=" + getValue("jpg_quality"));
      }
      echo("try(rk.graph.on(" + graph_options.join(", ") + "))\\n");
    }
    echo("try(print(p))\\n");
    if(!is_preview){
      echo("try(rk.graph.off())\\n");
    }
  '

  # =========================================================================================
  # 3. MAIN PLUGIN DEFINITION: MEANS GRAPH
  # =========================================================================================

  means_svyby_selector <- rk.XML.varselector(id.name = "means_svyby_selector", label = "svyby objects")
  means_svyby_slot <- rk.XML.varslot(label = "svyby object to plot", source = "means_svyby_selector", required = TRUE, id.name = "svyby_object", classes="data.frame")
  means_xaxis_slot <- rk.XML.varslot(label = "X-axis variable (e.g., year)", source = "means_svyby_selector", required = TRUE, id.name = "xaxis_var")
  means_facet_slot <- rk.XML.varslot(label = "Faceting variable (optional)", source = "means_svyby_selector", id.name = "facet_var")
  means_est_slot <- rk.XML.varslot(label = "Estimate columns", source = "means_svyby_selector", multi=TRUE, required = TRUE, id.name = "estimate_vars")
  means_se_slot <- rk.XML.varslot(label = "Standard Error columns", source = "means_svyby_selector", multi=TRUE, required = TRUE, id.name = "se_vars")

  means_dialog <- rk.XML.dialog(
    label = "Means Graph from svyby Object",
    child = rk.XML.row(
      rk.XML.col(means_svyby_selector),
      rk.XML.col(
        rk.XML.tabbook(tabs=list(
          "Data" = rk.XML.col(means_svyby_slot, means_xaxis_slot, means_facet_slot, means_est_slot, means_se_slot),
          "Options" = rk.XML.col(
             rk.XML.cbox(label="Flip coordinates", value="1", id.name="cbox_flip"),
             rk.XML.spinbox(label="Confidence level for error bars (%)", id.name="conf_level", min=1, max=99, initial=95),
             ordering_frame,
             color_palette_dropdown,
             rk.XML.dropdown(label="Facet Layout", id.name="facet_layout", options=list(
              "Wrap (default)"=list(val="wrap",chk=TRUE), "Force to one row"=list(val="row"), "Force to one column"=list(val="col")
            ))
          ),
          "Labels" = labels_tab_extended, # USES EXTENDED TAB
          "Theme" = theme_tab,
          "Output Device" = device_tab
        )),
        rk.XML.preview(id.name="plot_preview")
      )
    )
  )

  js_calc_means <- paste(js_helpers, '
    var svyby_obj = getValue("svyby_object");
    if(!svyby_obj) return;
    var xaxis_clean = getColumnName(getValue("xaxis_var"));
    var estimate_vars_full = getValue("estimate_vars");
    var se_vars_full = getValue("se_vars");
    var facet_var_full = getValue("facet_var");
    var conf_level = getValue("conf_level") / 100;

    echo("ci_multiplier <- qnorm(1 - (1 - " + conf_level + ") / 2)\\n");

    var estimate_array = estimate_vars_full.split(/\\n/).filter(function(n){ return n != "" }).map(function(item) { return "\\"" + getColumnName(item) + "\\""; });
    var se_array = se_vars_full.split(/\\n/).filter(function(n){ return n != "" }).map(function(item) { return "\\"" + getColumnName(item) + "\\""; });

    var id_vars = new Array();
    id_vars.push("\\"" + xaxis_clean + "\\"");
    if(facet_var_full){
      id_vars.push("\\"" + getColumnName(facet_var_full) + "\\"");
    }

    echo("est <- " + svyby_obj + " %>% dplyr::mutate(rk_internal_id = dplyr::row_number())\\n");

    var selection_vars_piv1 = id_vars.join(",") + ", \\"rk_internal_id\\"";
    echo("piv1 <- est %>% dplyr::select(dplyr::all_of(c(" + selection_vars_piv1 + ", " + estimate_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + estimate_array.join(",") + ")), names_to = \\"respuesta\\", values_to = \\"recuento\\")\\n");

    var selection_vars_piv2 = "\\"rk_internal_id\\"";
    echo("piv2 <- est %>% dplyr::select(dplyr::all_of(c(" + selection_vars_piv2 + ", " + se_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + se_array.join(",") + ")), names_to = \\"variable\\", values_to = \\"se\\")\\n");

    if (estimate_array.length == 1 && se_array.length == 1) {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = " + estimate_array[0] + ")\\n");
    } else {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = stringr::str_remove(variable, \\"^se\\\\\\\\.\\"))\\n");
    }

    echo("piv3 <- dplyr::left_join(piv1, piv2, by = c(\\"rk_internal_id\\", \\"respuesta\\"))\\n");

    // NEW: Clean legend prefix
    var clean_prefix = getValue("clean_legend_prefix");
    if (clean_prefix) {
        echo("piv3[[\\"respuesta\\"]] <- stringr::str_remove(piv3[[\\"respuesta\\"]], \\"" + clean_prefix + "\\")\\n");
    }

    echo("piv3[[\\"respuesta\\"]] <- forcats::fct_rev(piv3[[\\"respuesta\\"]] )\\n");

    if (getValue("order_x_est") == "1") {
       echo("piv3[[\\"" + xaxis_clean + "\\"]] <- forcats::fct_reorder(as.factor(piv3[[\\"" + xaxis_clean + "\\"]]), piv3$recuento)\\n");
    } else {
       echo("piv3[[\\"" + xaxis_clean + "\\"]] <- as.factor(piv3[[\\"" + xaxis_clean + "\\"]])\\n");
    }

    if (getValue("invert_order") == "1") {
       echo("piv3[[\\"" + xaxis_clean + "\\"]] <- forcats::fct_rev(piv3[[\\"" + xaxis_clean + "\\"]])\\n");
    }

    echo("p <- ggplot2::ggplot(piv3, ggplot2::aes(x = " + xaxis_clean + ", y = recuento, color = " + xaxis_clean + ")) +\\n");
    echo("  ggplot2::geom_point(size=2) +\\n");
    echo("  ggplot2::geom_errorbar(ggplot2::aes(ymin = recuento - ci_multiplier*se, ymax = recuento + ci_multiplier*se), width = 0.2) +\\n");
    echo("  ggplot2::theme_bw()\\n");

    var palette = getValue("palette_input");
    var legend_wrap_width = getValue("legend_wrap_width");
    var label_wrap_call = "";
    if (legend_wrap_width && parseInt(legend_wrap_width) > 0) {
        label_wrap_call = "labels = scales::label_wrap(" + legend_wrap_width + ")";
    }

    echo("n_colors <- length(unique(piv3[[\\"" + xaxis_clean + "\\"]]))\\n");
    echo("if (n_colors > 9) {\\n");
    echo("   get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, \\"" + palette + "\\"))\\n");
    var manual_opts = "values = get_palette(n_colors)";
    if(label_wrap_call) manual_opts += ", " + label_wrap_call;
    echo("   p <- p + ggplot2::scale_color_manual(" + manual_opts + ")\\n");
    echo("} else {\\n");
    var brewer_opts = "palette = \\"" + palette + "\\"";
    if(label_wrap_call) brewer_opts += ", " + label_wrap_call;
    echo("   p <- p + ggplot2::scale_color_brewer(" + brewer_opts + ")\\n");
    echo("}\\n");

    if(getValue("cbox_flip") == "1") { echo("p <- p + ggplot2::coord_flip()\\n"); }

    if(facet_var_full){
      var facet_layout = getValue("facet_layout");
      var facet_opts = "";
      if (facet_layout == "row") { facet_opts = ", nrow = 1"; }
      else if (facet_layout == "col") { facet_opts = ", ncol = 1"; }
      echo("p <- p + ggplot2::facet_wrap(~ " + getColumnName(facet_var_full) + facet_opts + ")\\n");
    }

    var labs_list = [];
    var custom_xlab = getValue("plot_xlab");
    var xlab_wrap = getValue("plot_xlab_wrap");
    var xlab_call;
    if (custom_xlab) { xlab_call = "\\"" + custom_xlab + "\\""; }
    else { xlab_call = "rk.get.label(" + getValue("xaxis_var") + ")"; }
    if (xlab_wrap && parseInt(xlab_wrap) > 0) {
        xlab_call = "scales::label_wrap(" + xlab_wrap + ")(" + xlab_call + ")";
    }
    labs_list.push("x = " + xlab_call);

    var custom_ylab = getValue("plot_ylab");
    var ylab_wrap = getValue("plot_ylab_wrap");
    if (custom_ylab) {
        var ylab_call = "\\"" + custom_ylab + "\\"";
        if (ylab_wrap && parseInt(ylab_wrap) > 0) {
            ylab_call = "scales::label_wrap(" + ylab_wrap + ")(" + ylab_call + ")";
        }
        labs_list.push("y = " + ylab_call);
    }

    var custom_legend_title = getValue("plot_legend_title");
    var legend_title_wrap_width = getValue("legend_title_wrap_width");
    var legend_title_call;
    if (custom_legend_title) { legend_title_call = "\\"" + custom_legend_title + "\\""; }
    else { legend_title_call = "rk.get.label(" + getValue("xaxis_var") + ")"; }
    if (legend_title_wrap_width && parseInt(legend_title_wrap_width) > 0) {
        legend_title_call = "scales::label_wrap(" + legend_title_wrap_width + ")(" + legend_title_call + ")";
    }
    labs_list.push("color = " + legend_title_call);

    if (getValue("plot_title")) { labs_list.push("title = \\"" + getValue("plot_title") + "\\""); }
    if (getValue("plot_subtitle")) { labs_list.push("subtitle = \\"" + getValue("plot_subtitle") + "\\""); }
    if (getValue("plot_caption")) { labs_list.push("caption = \\"" + getValue("plot_caption") + "\\""); }

    if (labs_list.length > 0) {
      echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\\n");
    }

    var x_val_wrap = getValue("theme_x_val_wrap");
    if (x_val_wrap && parseInt(x_val_wrap) > 0) {
        echo("p <- p + ggplot2::scale_x_discrete(labels = scales::label_wrap(" + x_val_wrap + "))\\n");
    }
    var y_val_wrap = getValue("theme_y_val_wrap");
    if (y_val_wrap && parseInt(y_val_wrap) > 0) {
        echo("p <- p + ggplot2::scale_y_discrete(labels = scales::label_wrap(" + y_val_wrap + "))\\n");
    }

    var theme_list = [];
    if(getValue("theme_text_rel") != 1) { theme_list.push("text = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_text_rel") + "))"); }
    if(getValue("theme_title_rel") != 1.2) { theme_list.push("plot.title = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_title_rel") + "))"); }
    if(getValue("theme_legend_rel") != 0.8) { theme_list.push("legend.text = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_legend_rel") + "))"); }
    if(getValue("theme_legend_pos") != "right") { theme_list.push("legend.position = \\"" + getValue("theme_legend_pos") + "\\""); }
    var x_angle = getValue("theme_x_angle");
    var x_hjust = getValue("theme_x_hjust");
    var x_vjust = getValue("theme_x_vjust");
    if(x_angle != 0 || x_hjust != 0.5 || x_vjust != 0.5) {
        theme_list.push("axis.text.x = ggplot2::element_text(angle=" + x_angle + ", hjust=" + x_hjust + ", vjust=" + x_vjust + ")");
    }
    if(theme_list.length > 0) {
      echo("p <- p + ggplot2::theme(" + theme_list.join(", ") + ")\\n");
    }
  ')

  # =========================================================================================
  # 4. COMPONENTS
  # =========================================================================================

  # --- LINE GRAPH COMPONENT ---
  line_svyby_selector <- rk.XML.varselector(id.name = "line_svyby_selector", label = "svyby objects")
  line_svyby_slot <- rk.XML.varslot(label = "svyby object to plot", source = "line_svyby_selector", required = TRUE, id.name = "svyby_object", classes="data.frame")
  line_xaxis_slot <- rk.XML.varslot(label = "X-axis variable (e.g., year)", source = "line_svyby_selector", required = TRUE, id.name = "xaxis_var")
  line_group_slot <- rk.XML.varslot(label = "Grouping variable (optional, for multiple lines)", source = "line_svyby_selector", id.name = "group_var")

  # MODIFIED: Reordered options as requested
  line_group_aes <- rk.XML.dropdown(label="Map Grouping to:", id.name="group_aes_mode", options=list(
        "Color only"=list(val="color", chk=TRUE),
        "Shape only"=list(val="shape"),
        "Color & Shape"=list(val="both")
  ))

  line_facet_slot <- rk.XML.varslot(label = "Faceting variable (optional)", source = "line_svyby_selector", id.name = "facet_var")
  line_est_slot <- rk.XML.varslot(label = "Estimate columns", source = "line_svyby_selector", multi=TRUE, required = TRUE, id.name = "estimate_vars")
  line_se_slot <- rk.XML.varslot(label = "Standard Error columns", source = "line_svyby_selector", multi=TRUE, required = TRUE, id.name = "se_vars")

  line_graph_dialog <- rk.XML.dialog(
    label = "Line Graph from svyby Object",
    child = rk.XML.row(
      rk.XML.col(line_svyby_selector),
      rk.XML.col(
        rk.XML.tabbook(tabs=list(
          "Data" = rk.XML.col(line_svyby_slot, line_xaxis_slot, line_group_slot, line_group_aes, line_facet_slot, line_est_slot, line_se_slot),
          "Options" = rk.XML.col(
            rk.XML.spinbox(label="Confidence level for error bars (%)", id.name="conf_level", min=1, max=99, initial=95),
            ordering_frame,
            color_palette_dropdown,
            rk.XML.dropdown(label="Facet Layout", id.name="facet_layout", options=list(
              "Wrap (default)"=list(val="wrap",chk=TRUE), "Force to one row"=list(val="row"), "Force to one column"=list(val="col")
            ))
          ),
          "Labels" = labels_tab_extended, # USES EXTENDED TAB
          "Theme" = theme_tab,
          "Output Device" = device_tab
        )),
        rk.XML.preview(id.name="plot_preview")
      )
    )
  )

  js_calc_line <- paste(js_helpers, '
    var svyby_obj = getValue("svyby_object");
    if(!svyby_obj) return;
    var xaxis_clean = getColumnName(getValue("xaxis_var"));
    var group_var_full = getValue("group_var");
    var group_aes_mode = getValue("group_aes_mode");
    var facet_var_full = getValue("facet_var");
    var estimate_vars_full = getValue("estimate_vars");
    var se_vars_full = getValue("se_vars");
    var conf_level = getValue("conf_level") / 100;

    echo("ci_multiplier <- qnorm(1 - (1 - " + conf_level + ") / 2)\\n");
    var estimate_array = estimate_vars_full.split(/\\n/).filter(function(n){ return n != "" }).map(function(item) { return "\\"" + getColumnName(item) + "\\""; });
    var se_array = se_vars_full.split(/\\n/).filter(function(n){ return n != "" }).map(function(item) { return "\\"" + getColumnName(item) + "\\""; });

    var id_vars = new Array();
    id_vars.push("\\"" + xaxis_clean + "\\"");
    if(group_var_full){ id_vars.push("\\"" + getColumnName(group_var_full) + "\\""); }
    if(facet_var_full){ id_vars.push("\\"" + getColumnName(facet_var_full) + "\\""); }

    echo("est <- " + svyby_obj + " %>% dplyr::mutate(rk_internal_id = dplyr::row_number())\\n");

    var selection_vars_piv1 = id_vars.join(",") + ", \\"rk_internal_id\\"";
    echo("piv1 <- est %>% dplyr::select(dplyr::all_of(c(" + selection_vars_piv1 + ", " + estimate_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + estimate_array.join(",") + ")), names_to = \\"respuesta\\", values_to = \\"recuento\\")\\n");

    var selection_vars_piv2 = "\\"rk_internal_id\\"";
    echo("piv2 <- est %>% dplyr::select(dplyr::all_of(c(" + selection_vars_piv2 + ", " + se_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + se_array.join(",") + ")), names_to = \\"variable\\", values_to = \\"se\\")\\n");

    if (estimate_array.length == 1 && se_array.length == 1) {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = " + estimate_array[0] + ")\\n");
    } else {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = stringr::str_remove(variable, \\"^se\\\\\\\\.\\"))\\n");
    }
    echo("piv3 <- dplyr::left_join(piv1, piv2, by = c(\\"rk_internal_id\\", \\"respuesta\\"))\\n");

    // NEW: Clean legend prefix
    var clean_prefix = getValue("clean_legend_prefix");
    if (clean_prefix) {
        echo("piv3[[\\"respuesta\\"]] <- stringr::str_remove(piv3[[\\"respuesta\\"]], \\"" + clean_prefix + "\\")\\n");
    }

    echo("piv3[[\\"respuesta\\"]] <- forcats::fct_rev(piv3[[\\"respuesta\\"]] )\\n");

    if (getValue("order_x_est") == "1") {
       echo("piv3[[\\"" + xaxis_clean + "\\"]] <- forcats::fct_reorder(as.factor(piv3[[\\"" + xaxis_clean + "\\"]]), piv3$recuento)\\n");
    } else {
       echo("piv3[[\\"" + xaxis_clean + "\\"]] <- as.factor(piv3[[\\"" + xaxis_clean + "\\"]])\\n");
    }
    if (getValue("invert_order") == "1") {
       echo("piv3[[\\"" + xaxis_clean + "\\"]] <- forcats::fct_rev(piv3[[\\"" + xaxis_clean + "\\"]])\\n");
    }

    // --- PLOT CONSTRUCTION ---
    var aes_call = "x = " + xaxis_clean + ", y = recuento";
    var color_var = "respuesta";

    if (group_var_full) {
        var group_clean = getColumnName(group_var_full);

        if (group_aes_mode == "both") {
            color_var = group_clean;
            aes_call += ", color = " + group_clean + ", shape = " + group_clean;
            if (estimate_array.length > 1) {
                aes_call += ", linetype = respuesta, group = interaction(" + group_clean + ", respuesta)";
            } else {
                aes_call += ", group = " + group_clean;
            }
        }
        else if (group_aes_mode == "color") {
            color_var = group_clean;
            aes_call += ", color = " + group_clean;
            if (estimate_array.length > 1) {
                aes_call += ", linetype = respuesta, group = interaction(" + group_clean + ", respuesta)";
            } else {
                aes_call += ", group = " + group_clean;
            }
        }
        else if (group_aes_mode == "shape") {
            color_var = "respuesta";
            aes_call += ", shape = " + group_clean + ", color = respuesta";
            if (estimate_array.length > 1) {
                aes_call += ", group = interaction(" + group_clean + ", respuesta)";
            } else {
                aes_call += ", group = " + group_clean;
            }
        }
    } else {
        aes_call += ", color = respuesta, group = respuesta";
    }

    echo("p <- ggplot2::ggplot(piv3, ggplot2::aes(" + aes_call + ")) +\\n");
    echo("  ggplot2::geom_line(linewidth=1) +\\n");
    echo("  ggplot2::geom_point(size=2) +\\n");
    echo("  ggplot2::geom_errorbar(ggplot2::aes(ymin = recuento - ci_multiplier*se, ymax = recuento + ci_multiplier*se), width = 0.2) +\\n");
    echo("  ggplot2::theme_bw()\\n");

    // PALETTE
    var palette = getValue("palette_input");
    var legend_wrap_width = getValue("legend_wrap_width");
    var label_wrap_call = "";
    if (legend_wrap_width && parseInt(legend_wrap_width) > 0) {
        label_wrap_call = "labels = scales::label_wrap(" + legend_wrap_width + ")";
    }

    echo("n_colors <- length(unique(piv3[[\\"" + color_var + "\\"]]))\\n");
    echo("if (n_colors > 9) {\\n");
    echo("   get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, \\"" + palette + "\\"))\\n");
    var manual_opts = "values = get_palette(n_colors)";
    if(label_wrap_call) manual_opts += ", " + label_wrap_call;
    echo("   p <- p + ggplot2::scale_color_manual(" + manual_opts + ")\\n");
    echo("} else {\\n");
    var brewer_opts = "palette = \\"" + palette + "\\"";
    if(label_wrap_call) brewer_opts += ", " + label_wrap_call;
    echo("   p <- p + ggplot2::scale_color_brewer(" + brewer_opts + ")\\n");
    echo("}\\n");

    if(facet_var_full){
      var facet_layout = getValue("facet_layout");
      var facet_opts = "";
      if (facet_layout == "row") { facet_opts = ", nrow = 1"; }
      else if (facet_layout == "col") { facet_opts = ", ncol = 1"; }
      echo("p <- p + ggplot2::facet_wrap(~ " + getColumnName(facet_var_full) + facet_opts + ")\\n");
    }

    // LABELS
    var labs_list = [];
    var custom_xlab = getValue("plot_xlab");
    if (custom_xlab) { labs_list.push("x = \\"" + custom_xlab + "\\""); }
    else { labs_list.push("x = rk.get.label(" + getValue("xaxis_var") + ")"); }

    var custom_ylab = getValue("plot_ylab");
    if (custom_ylab) { labs_list.push("y = \\"" + custom_ylab + "\\""); }

    var custom_legend_title = getValue("plot_legend_title");
    var legend_title = custom_legend_title ? custom_legend_title : "Respuesta";

    if (group_var_full) {
        var g_title = custom_legend_title ? custom_legend_title : "Group";
        var r_title = "Respuesta";

        if (group_aes_mode == "both") {
            labs_list.push("color = \\"" + g_title + "\\"");
            labs_list.push("shape = \\"" + g_title + "\\"");
            labs_list.push("linetype = \\"" + r_title + "\\"");
        }
        else if (group_aes_mode == "color") {
            labs_list.push("color = \\"" + g_title + "\\"");
            labs_list.push("linetype = \\"" + r_title + "\\"");
        }
        else if (group_aes_mode == "shape") {
            labs_list.push("shape = \\"" + g_title + "\\"");
            if(custom_legend_title) labs_list.push("color = \\"" + custom_legend_title + "\\"");
            else labs_list.push("color = \\"" + r_title + "\\"");
        }
    } else {
        labs_list.push("color = \\"" + legend_title + "\\"");
    }

    if (getValue("plot_title")) { labs_list.push("title = \\"" + getValue("plot_title") + "\\""); }
    if (getValue("plot_subtitle")) { labs_list.push("subtitle = \\"" + getValue("plot_subtitle") + "\\""); }
    if (getValue("plot_caption")) { labs_list.push("caption = \\"" + getValue("plot_caption") + "\\""); }

    if (labs_list.length > 0) {
      echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\\n");
    }

    // THEME
    var x_val_wrap = getValue("theme_x_val_wrap");
    if (x_val_wrap && parseInt(x_val_wrap) > 0) {
        echo("p <- p + ggplot2::scale_x_discrete(labels = scales::label_wrap(" + x_val_wrap + "))\\n");
    }
    var y_val_wrap = getValue("theme_y_val_wrap");
    if (y_val_wrap && parseInt(y_val_wrap) > 0) {
        echo("p <- p + ggplot2::scale_y_continuous(labels = scales::label_wrap(" + y_val_wrap + "))\\n");
    }

    var theme_list = [];
    if(getValue("theme_text_rel") != 1) { theme_list.push("text = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_text_rel") + "))"); }
    if(getValue("theme_title_rel") != 1.2) { theme_list.push("plot.title = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_title_rel") + "))"); }
    if(getValue("theme_legend_rel") != 0.8) { theme_list.push("legend.text = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_legend_rel") + "))"); }
    if(getValue("theme_legend_pos") != "right") { theme_list.push("legend.position = \\"" + getValue("theme_legend_pos") + "\\""); }
    var x_angle = getValue("theme_x_angle");
    var x_hjust = getValue("theme_x_hjust");
    var x_vjust = getValue("theme_x_vjust");
    if(x_angle != 0 || x_hjust != 0.5 || x_vjust != 0.5) {
        theme_list.push("axis.text.x = ggplot2::element_text(angle=" + x_angle + ", hjust=" + x_hjust + ", vjust=" + x_vjust + ")");
    }
    if(theme_list.length > 0) {
      echo("p <- p + ggplot2::theme(" + theme_list.join(", ") + ")\\n");
    }
  ')
  line_component <- rk.plugin.component("Line Graph", xml=list(dialog=line_graph_dialog), js=list(require=c("ggplot2","dplyr","tidyr","RColorBrewer","scales","stringr"), calculate=js_calc_line, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # --- C. OTHER GRAPHS ---
  bar_svy_selector <- rk.XML.varselector(id.name="bar_svy_selector", label="Survey Design Objects"); attr(bar_svy_selector, "classes") <- "svydesign"
  bar_svy_slot <- rk.XML.varslot(label="Survey Design Object", source="bar_svy_selector", required=TRUE, id.name="svy_object")
  bar_x_slot <- rk.XML.varslot(label="X Variable", source="bar_svy_selector", required=TRUE, id.name="x_var"); attr(bar_x_slot, "source_property") <- "variables"

  bar_dialog <- rk.XML.dialog(label = "Survey Bar Plot", child = rk.XML.row(bar_svy_selector, rk.XML.col(
    rk.XML.tabbook(tabs=list(
      "Data" = rk.XML.col(bar_svy_slot, bar_x_slot),
      "Labels" = labels_tab_simple, # USES SIMPLE TAB
      "Style & Layout" = rk.XML.col(color_palette_dropdown),
      "Output Device" = device_tab
    )),
    rk.XML.preview(id.name="plot_preview")
  )))
  js_calc_bar <- paste(js_helpers, '
    var svy_obj = getValue("svy_object");
    var x_var = getColumnName(getValue("x_var"));
    if(!svy_obj) return;
    echo("p <- ggsurvey::ggbarweight_svy(" + svy_obj + ", " + x_var + ")\\n");
  ')

  bar_component <- rk.plugin.component("Bar Diagram", xml=list(dialog=bar_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_bar, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))
  box_component <- rk.plugin.component("Box Plot", xml=list(dialog=bar_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_bar, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))
  hex_component <- rk.plugin.component("Hexbin Plot", xml=list(dialog=bar_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_bar, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))
  hist_component <- rk.plugin.component("Histogram", xml=list(dialog=bar_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_bar, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # =========================================================================================
  # 5. Final Skeleton Generation
  # =========================================================================================
  all_components <- list(
    line_component,
    bar_component,
    box_component,
    hex_component,
    hist_component
  )

  rk.plugin.skeleton(
    about = package_about,
    path = ".",
    xml = list(dialog = means_dialog),
    js = list(
      require = c("survey", "dplyr", "ggplot2", "tidyr", "forcats", "stringr", "RColorBrewer", "scales"),
      calculate = js_calc_means,
      printout = js_print_graph
    ),
    rkh = list(help = rk.rkh.doc(title=rk.rkh.title("Means Graph"))),
    components = all_components,
    pluginmap = list(name = "Means Graph", hierarchy = list("Survey","Graphs","ggGraphs")),
    create = c("pmap", "xml", "js", "desc", "rkh"),
    load = TRUE,
    overwrite = TRUE,
    show = FALSE
  )

  cat("\nFully optimized plugin package 'rk.ggsurvey' (v0.1.9) generated.\n")
  cat("  rk.updatePluginMessages(plugin.dir=\"rk.ggsurvey\")\n")
  cat("  devtools::install(\"rk.ggsurvey\")\n")
})
