local({
  # =========================================================================================
  # Package Definition and Metadata
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
      desc = "A plugin package analyze complex survey designs with custom plugins and the 'ggsurvey' package.",
      version = "0.1.5", # Updated Version
      url = "https://github.com/AlfCano/rk.ggsurvey",
      license = "GPL (>= 3)"
    )
  )

  # =========================================================================================
  # --- Reusable UI and JS Helpers ---
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

  labels_tab <- rk.XML.col(
    rk.XML.input(label="Plot Title", id.name="title_input"),
    rk.XML.input(label="Variable prefix to remove from legend", id.name="prefix_clean_input"),
    rk.XML.input(label="Subtitle", id.name="subtitle_input"),
    rk.XML.input(label="X-axis label", id.name="xlab_input"),
    rk.XML.input(label="Y-axis label", id.name="ylab_input"),
    rk.XML.input(label="Legend title", id.name="legend_title_input"),
    rk.XML.input(label="Caption", id.name="caption_input")
  )

  device_tab <- rk.XML.col(
    rk.XML.dropdown(label="Device type", id.name="device_type", options=list("PNG"=list(val="PNG", chk=TRUE), "SVG"=list(val="SVG"), "JPG"=list(val="JPG"))),
    rk.XML.spinbox(label="JPG Quality (0-100)", id.name="jpg_quality", min=0, max=100, initial=75),
    rk.XML.spinbox(label="Width (px)", id.name="dev_width", min=100, max=4000, initial=720),
    rk.XML.spinbox(label="Height (px)", id.name="dev_height", min=100, max=4000, initial=720),
    rk.XML.spinbox(label="Resolution (ppi)", id.name="dev_res", min=50, max=600, initial=125),
    rk.XML.dropdown(label="Background", id.name="dev_bg", options=list("Transparent"=list(val="transparent", chk=TRUE), "White"=list(val="white")))
  )

  js_print_graph <- '
    if(!is_preview){
      var graph_options = new Array();
      graph_options.push("device.type=\\"" + getValue("device_type") + "\\"");
      graph_options.push("width=" + getValue("dev_width"));
      graph_options.push("height=" + getValue("dev_height"));
      graph_options.push("pointsize=10.0");
      graph_options.push("res=" + getValue("dev_res"));
      graph_options.push("bg=\\"" + getValue("dev_bg") + "\\"");
      if(getValue("device_type") == "JPG"){
        graph_options.push("quality=" + getValue("jpg_quality"));
      }
      echo("rk.graph.on(" + graph_options.join(", ") + ")\\n");
    }
    echo("try(print(p))\\n");
    if(!is_preview){
      echo("rk.graph.off()\\n");
    }
  '

  color_palette_dropdown <- rk.XML.dropdown(label="Color Palette (ColorBrewer)", id.name="palette_input", options=list(
    "Default (Set1)"=list(val="Set1",chk=TRUE),
    "Qualitative: Accent"=list(val="Accent"),
    "Qualitative: Dark2"=list(val="Dark2"),
    "Qualitative: Paired"=list(val="Paired"),
    "Qualitative: Pastel1"=list(val="Pastel1"),
    "Qualitative: Pastel2"=list(val="Pastel2"),
    "Qualitative: Set2"=list(val="Set2"),
    "Qualitative: Set3"=list(val="Set3"),
    "Sequential: Blues"=list(val="Blues"),
    "Sequential: Greens"=list(val="Greens"),
    "Sequential: Oranges"=list(val="Oranges"),
    "Sequential: Purples"=list(val="Purples"),
    "Sequential: Reds"=list(val="Reds"),
    "Diverging: RdYlBu"=list(val="RdYlBu"),
    "Diverging: Spectral"=list(val="Spectral"),
    "Diverging: BrBG"=list(val="BrBG")
  ))

  # =========================================================================================
  # Component 1: Line Graph
  # =========================================================================================
  svyby_selector <- rk.XML.varselector(id.name = "svyby_selector", label = "svyby objects")
  svyby_object_slot <- rk.XML.varslot(label = "svyby object to plot", source = "svyby_selector", required = TRUE, id.name = "svyby_object", classes="data.frame")
  xaxis_var_slot <- rk.XML.varslot(label = "X-axis variable (e.g., year)", source = "svyby_selector", required = TRUE, id.name = "xaxis_var")
  facet_var_slot <- rk.XML.varslot(label = "Faceting variable (optional)", source = "svyby_selector", id.name = "facet_var")
  estimate_vars_slot <- rk.XML.varslot(label = "Estimate columns", source = "svyby_selector", multi=TRUE, required = TRUE, id.name = "estimate_vars")
  se_vars_slot <- rk.XML.varslot(label = "Standard Error columns", source = "svyby_selector", multi=TRUE, required = TRUE, id.name = "se_vars")

  line_graph_dialog <- rk.XML.dialog(
    label = "Line Graph from svyby Object",
    child = rk.XML.row(
      rk.XML.col(svyby_selector),
      rk.XML.col(
        rk.XML.tabbook(tabs=list(
          "Data" = rk.XML.col(svyby_object_slot, xaxis_var_slot, facet_var_slot, estimate_vars_slot, se_vars_slot),
          "Labels" = labels_tab,
          "Style & Layout" = rk.XML.col(
            rk.XML.spinbox(label="Confidence level for error bars (%)", id.name="conf_level", min=1, max=99, initial=95),
            color_palette_dropdown,
            rk.XML.dropdown(label="Facet Layout", id.name="facet_layout", options=list(
              "Wrap (default)"=list(val="wrap",chk=TRUE), "Force to one row"=list(val="row"), "Force to one column"=list(val="col")
            ))
          ),
          "Output Device" = device_tab
        )),
        rk.XML.preview(id.name="plot_preview")
      )
    )
  )

  js_calc_line_graph <- paste(js_helpers, '
    var svyby_obj = getValue("svyby_object");
    if(!svyby_obj) return;
    var xaxis_clean = getColumnName(getValue("xaxis_var"));
    var estimate_vars_full = getValue("estimate_vars");
    var se_vars_full = getValue("se_vars");
    var prefix_clean = getValue("prefix_clean_input");
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

    // Force selection to avoid collision
    echo("est <- " + svyby_obj + "\\n");
    echo("piv1 <- est %>% dplyr::select(dplyr::all_of(c(" + id_vars.join(",") + ", " + estimate_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + estimate_array.join(",") + ")), names_to = \\"respuesta\\", values_to = \\"recuento\\")\\n");
    echo("piv2 <- est %>% dplyr::select(dplyr::all_of(c(" + id_vars.join(",") + ", " + se_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + se_array.join(",") + ")), names_to = \\"variable\\", values_to = \\"se\\")\\n");

    // Robust join matching
    if (estimate_array.length == 1 && se_array.length == 1) {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = " + estimate_array[0] + ")\\n");
    } else {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = stringr::str_remove(variable, \\"^se\\\\\\\\.\\"))\\n");
    }

    echo("piv3 <- dplyr::left_join(piv1, piv2, by = c(" + id_vars.join(", ") + ", \\"respuesta\\"))\\n");

    if(prefix_clean){
      echo("piv3[[\\"respuesta\\"]] <- gsub(\\"" + prefix_clean + "\\", \\"\\", piv3[[\\"respuesta\\"]] )\\n");
    }
    echo("piv3[[\\"respuesta\\"]] <- forcats::fct_rev(piv3[[\\"respuesta\\"]] )\\n");

    echo("p <- ggplot2::ggplot(piv3, ggplot2::aes(x = " + xaxis_clean + ", y = recuento, color = respuesta, group = respuesta)) +\\n  ggplot2::geom_line() +\\n");
    echo("  ggplot2::geom_errorbar(ggplot2::aes(ymin = recuento - ci_multiplier*se, ymax = recuento + ci_multiplier*se), width = 0.2) +\\n");
    echo("  ggplot2::scale_color_brewer(palette = \\"" + getValue("palette_input") + "\\", labels = function(x) stringr::str_wrap(x, width = 20)) +\\n");

    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \\"" + getValue("title_input") + "\\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \\"" + getValue("subtitle_input") + "\\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \\"" + getValue("xlab_input") + "\\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \\"" + getValue("ylab_input") + "\\""); }
    if(getValue("legend_title_input")) { labs_list.push("color = \\"" + getValue("legend_title_input") + "\\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \\"" + getValue("caption_input") + "\\""); }
    if(labs_list.length > 0) { echo("  ggplot2::labs(" + labs_list.join(", ") + ") +\\n"); }

    echo("  ggplot2::theme_bw()\\n");

    if(facet_var_full){
      var facet_layout = getValue("facet_layout");
      var facet_opts = "";
      if (facet_layout == "row") { facet_opts = ", nrow = 1"; }
      else if (facet_layout == "col") { facet_opts = ", ncol = 1"; }
      echo("p <- p + ggplot2::facet_wrap(~ " + getColumnName(facet_var_full) + facet_opts + ")\\n");
    }
  ')

  # =========================================================================================
  # Component 2: Bar Diagram
  # =========================================================================================
  bar_svy_selector <- rk.XML.varselector(id.name="bar_svy_selector", label="Survey Design Objects"); attr(bar_svy_selector, "classes") <- "svydesign"
  bar_svy_slot <- rk.XML.varslot(label="Survey Design Object", source="bar_svy_selector", required=TRUE, id.name="svy_object")
  bar_x_slot <- rk.XML.varslot(label="X Variable", source="bar_svy_selector", required=TRUE, id.name="x_var"); attr(bar_x_slot, "source_property") <- "variables"
  bar_y_slot <- rk.XML.varslot(label="Y Variable (for crosstabs)", source="bar_svy_selector", id.name="y_var"); attr(bar_y_slot, "source_property") <- "variables"
  bar_z_slot <- rk.XML.varslot(label="Faceting Variable (for 3D)", source="bar_svy_selector", id.name="z_var"); attr(bar_z_slot, "source_property") <- "variables"

  bar_dialog <- rk.XML.dialog(label = "Survey Bar Plot", child = rk.XML.row(bar_svy_selector, rk.XML.col(
    rk.XML.tabbook(tabs=list(
      "Data" = rk.XML.col(bar_svy_slot, bar_x_slot, bar_y_slot, bar_z_slot),
      "Labels" = labels_tab,
      "Style & Layout" = rk.XML.col(
        rk.XML.cbox(label="Fill bars with relative frequency (fill=TRUE)", id.name="cbox_fill", value="1", chk=TRUE),
        rk.XML.cbox(label="Flip coordinates", value="1", id.name="cbox_flip"),
        rk.XML.cbox(label="Show legend", value="1", id.name="cbox_legend", chk=TRUE),
        rk.XML.spinbox(label="X-axis text angle", id.name="spin_angle", min=0, max=90, initial=0),
        rk.XML.spinbox(label="X-axis text vjust", id.name="spin_vjust", min=0, max=1, initial=0.5, real=TRUE),
        color_palette_dropdown
      ),
      "Output Device" = device_tab
    )),
    rk.XML.preview(id.name="plot_preview")
  )))

  js_calc_bar <- paste(js_helpers, '
    var svy_obj = getValue("svy_object");
    if(!svy_obj) return;
    var x_var = getColumnName(getValue("x_var"));
    var y_var = getColumnName(getValue("y_var"));
    var z_var = getColumnName(getValue("z_var"));

    var func_name = y_var ? (z_var ? "ggbarcrosstabs3d_svy" : "ggbarcrosstabs_svy") : "ggbarweight_svy";
    var plot_call = "ggsurvey::" + func_name + "(" + svy_obj + ", " + x_var;
    if(y_var) { plot_call += ", " + y_var; }
    if(z_var) { plot_call += ", " + z_var; }
    if(getValue("cbox_fill") == "1") { plot_call += ", fill=TRUE"; }
    plot_call += ")";
    echo("p <- " + plot_call + "\\n");

    if(getValue("cbox_flip") == "1") { echo("p <- p + ggplot2::coord_flip()\\n"); }
    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \\"" + getValue("title_input") + "\\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \\"" + getValue("subtitle_input") + "\\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \\"" + getValue("xlab_input") + "\\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \\"" + getValue("ylab_input") + "\\""); }
    if(getValue("legend_title_input")) { labs_list.push("fill = \\"" + getValue("legend_title_input") + "\\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \\"" + getValue("caption_input") + "\\""); }
    if(labs_list.length > 0) { echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\\n"); }
    if(getValue("cbox_legend") && getValue("cbox_legend") != "1") { echo("p <- p + ggplot2::theme(legend.position=\\"none\\")\\n"); }
    if(getValue("spin_angle") && (getValue("spin_angle") != "0" || getValue("spin_vjust") != "0.5")) {
        echo("p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=" + getValue("spin_angle") + ", vjust=" + getValue("spin_vjust") + "))\\n");
    }
    if(getValue("palette_input")) { echo("p <- p + ggplot2::scale_fill_brewer(palette = \\"" + getValue("palette_input") + "\\")\\n"); }
  ')
  bar_component <- rk.plugin.component("Bar Diagram", xml=list(dialog=bar_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_bar, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # =========================================================================================
  # Component 3: Box Plot
  # =========================================================================================
  box_svy_selector <- rk.XML.varselector(id.name="box_svy_selector", label="Survey Design Objects"); attr(box_svy_selector, "classes") <- "svydesign"
  box_svy_slot <- rk.XML.varslot(label="Survey Design Object", source="box_svy_selector", required=TRUE, id.name="svy_object")
  box_x_slot <- rk.XML.varslot(label="X Variable", source="box_svy_selector", required=TRUE, id.name="x_var"); attr(box_x_slot, "source_property") <- "variables"
  box_y_slot <- rk.XML.varslot(label="Y Variable (for 2D)", source="box_svy_selector", id.name="y_var"); attr(box_y_slot, "source_property") <- "variables"
  box_z_slot <- rk.XML.varslot(label="Faceting Variable (for 3D)", source="box_svy_selector", id.name="z_var"); attr(box_z_slot, "source_property") <- "variables"

  box_dialog <- rk.XML.dialog(label = "Survey Box Plot", child = rk.XML.row(box_svy_selector, rk.XML.col(
    rk.XML.tabbook(tabs=list(
      "Data" = rk.XML.col(box_svy_slot, box_x_slot, box_y_slot, box_z_slot),
      "Labels" = labels_tab,
      "Style & Layout" = rk.XML.col(
        rk.XML.cbox(label="Flip coordinates", value="1", id.name="cbox_flip"),
        rk.XML.spinbox(label="X-axis text angle", id.name="spin_angle", min=0, max=90, initial=0),
        rk.XML.spinbox(label="X-axis text vjust", id.name="spin_vjust", min=0, max=1, initial=0.5, real=TRUE)
      ),
      "Output Device" = device_tab
    )),
    rk.XML.preview(id.name="plot_preview")
  )))

  js_calc_box <- paste(js_helpers, '
    var svy_obj = getValue("svy_object");
    if(!svy_obj) return;
    var x_var = getColumnName(getValue("x_var"));
    var y_var = getColumnName(getValue("y_var"));
    var z_var = getColumnName(getValue("z_var"));

    var func_name = y_var ? (z_var ? "ggboxweight3d_svy" : "ggboxweight2d_svy") : "ggboxweight_svy";
    var plot_call = "ggsurvey::" + func_name + "(" + svy_obj + ", " + x_var;
    if(y_var) { plot_call += ", " + y_var; }
    if(z_var) { plot_call += ", " + z_var; }
    plot_call += ")";
    echo("p <- " + plot_call + "\\n");

    if(getValue("cbox_flip") == "1") { echo("p <- p + ggplot2::coord_flip()\\n"); }
    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \\"" + getValue("title_input") + "\\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \\"" + getValue("subtitle_input") + "\\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \\"" + getValue("xlab_input") + "\\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \\"" + getValue("ylab_input") + "\\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \\"" + getValue("caption_input") + "\\""); }
    if(labs_list.length > 0) { echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\\n"); }
    if(getValue("spin_angle") && (getValue("spin_angle") != "0" || getValue("spin_vjust") != "0.5")) {
        echo("p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=" + getValue("spin_angle") + ", vjust=" + getValue("spin_vjust") + "))\\n");
    }
  ')
  box_component <- rk.plugin.component("Box Plot", xml=list(dialog=box_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_box, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # =========================================================================================
  # Component 4: Hexbin Plot
  # =========================================================================================
  hex_svy_selector <- rk.XML.varselector(id.name="hex_svy_selector", label="Survey Design Objects"); attr(hex_svy_selector, "classes") <- "svydesign"
  hex_svy_slot <- rk.XML.varslot(label="Survey Design Object", source="hex_svy_selector", required=TRUE, id.name="svy_object")
  hex_x_slot <- rk.XML.varslot(label="X Variable", source="hex_svy_selector", required=TRUE, id.name="x_var"); attr(hex_x_slot, "source_property") <- "variables"
  hex_y_slot <- rk.XML.varslot(label="Y Variable", source="hex_svy_selector", required=TRUE, id.name="y_var"); attr(hex_y_slot, "source_property") <- "variables"
  hex_a_slot <- rk.XML.varslot(label="Horizontal Facet (a)", source="hex_svy_selector", id.name="a_var"); attr(hex_a_slot, "source_property") <- "variables"
  hex_b_slot <- rk.XML.varslot(label="Vertical Facet (b)", source="hex_svy_selector", id.name="b_var"); attr(hex_b_slot, "source_property") <- "variables"

  hex_dialog <- rk.XML.dialog(label = "Survey Hexbin Plot", child = rk.XML.row(hex_svy_selector, rk.XML.col(
    rk.XML.tabbook(tabs=list(
      "Data" = rk.XML.col(hex_svy_slot, hex_x_slot, hex_y_slot, hex_a_slot, hex_b_slot),
      "Labels" = labels_tab,
      "Style & Layout" = rk.XML.col(
        rk.XML.dropdown(label="Color Palette (Viridis)", id.name="palette_input", options=list(
          "Default (viridis)"=list(val="viridis",chk=TRUE), "Magma"=list(val="magma"), "Inferno"=list(val="inferno"), "Plasma"=list(val="plasma")
        )),
        rk.XML.spinbox(label="X-axis text angle", id.name="spin_angle", min=0, max=90, initial=0),
        rk.XML.spinbox(label="X-axis text vjust", id.name="spin_vjust", min=0, max=1, initial=0.5, real=TRUE)
      ),
      "Output Device" = device_tab
    )),
    rk.XML.preview(id.name="plot_preview")
  )))

  js_calc_hex <- paste(js_helpers, '
    var svy_obj = getValue("svy_object");
    if(!svy_obj) return;
    var x_var = getColumnName(getValue("x_var"));
    var y_var = getColumnName(getValue("y_var"));
    var a_var = getColumnName(getValue("a_var"));
    var b_var = getColumnName(getValue("b_var"));

    var plot_call = "";
    if(b_var){
      plot_call = "ggsurvey::gghexweight3d_svy(" + svy_obj + ", " + x_var + ", " + y_var + ", " + a_var + ", " + b_var + ")";
    } else if (a_var) {
      plot_call = "ggsurvey::gghexweight2d_svy(" + svy_obj + ", " + x_var + ", " + y_var + ", " + a_var + ")";
    } else {
      plot_call = "ggsurvey::gghexweight_svy(" + svy_obj + ", " + x_var + ", " + y_var + ")";
    }
    echo("p <- " + plot_call + "\\n");

    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \\"" + getValue("title_input") + "\\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \\"" + getValue("subtitle_input") + "\\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \\"" + getValue("xlab_input") + "\\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \\"" + getValue("ylab_input") + "\\""); }
    if(getValue("legend_title_input")) { labs_list.push("fill = \\"" + getValue("legend_title_input") + "\\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \\"" + getValue("caption_input") + "\\""); }
    if(labs_list.length > 0) { echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\\n"); }

    if(getValue("palette_input")) {
       echo("p <- p + ggplot2::scale_fill_viridis_c(option=\\"" + getValue("palette_input") + "\\")\\n");
    }
    if(getValue("spin_angle") && (getValue("spin_angle") != "0" || getValue("spin_vjust") != "0.5")) {
        echo("p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=" + getValue("spin_angle") + ", vjust=" + getValue("spin_vjust") + "))\\n");
    }
  ')
  hex_component <- rk.plugin.component("Hexbin Plot", xml=list(dialog=hex_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_hex, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # =========================================================================================
  # Component 5: Histogram
  # =========================================================================================
  hist_svy_selector <- rk.XML.varselector(id.name="hist_svy_selector", label="Survey Design Objects"); attr(hist_svy_selector, "classes") <- "svydesign"
  hist_svy_slot <- rk.XML.varslot(label="Survey Design Object", source="hist_svy_selector", required=TRUE, id.name="svy_object")
  hist_x_slot <- rk.XML.varslot(label="X Variable", source="hist_svy_selector", required=TRUE, id.name="x_var"); attr(hist_x_slot, "source_property") <- "variables"
  hist_facet_slot <- rk.XML.varslot(label="Faceting Variable (optional)", source="hist_svy_selector", id.name="z_var"); attr(hist_facet_slot, "source_property") <- "variables"

  hist_dialog <- rk.XML.dialog(label = "Survey Histogram", child = rk.XML.row(hist_svy_selector, rk.XML.col(
    rk.XML.tabbook(tabs=list(
      "Data" = rk.XML.col(hist_svy_slot, hist_x_slot, hist_facet_slot),
      "Labels" = labels_tab,
      "Style & Layout" = rk.XML.col(
        rk.XML.spinbox(label="X-axis text angle", id.name="spin_angle", min=0, max=90, initial=0),
        rk.XML.spinbox(label="X-axis text vjust", id.name="spin_vjust", min=0, max=1, initial=0.5, real=TRUE)
      ),
      "Output Device" = device_tab
    )),
    rk.XML.preview(id.name="plot_preview")
  )))

  js_calc_hist <- paste(js_helpers, '
    var svy_obj = getValue("svy_object");
    if(!svy_obj) return;
    var x_var = getColumnName(getValue("x_var"));
    var z_var = getColumnName(getValue("z_var"));

    echo("p <- ggsurvey::gghistweight_svy(" + svy_obj + ", " + x_var + ")\\n");
    if(z_var){
        echo("p <- p + ggplot2::facet_wrap( ~ " + z_var + ")\\n");
    }

    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \\"" + getValue("title_input") + "\\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \\"" + getValue("subtitle_input") + "\\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \\"" + getValue("xlab_input") + "\\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \\"" + getValue("ylab_input") + "\\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \\"" + getValue("caption_input") + "\\""); }
    if(labs_list.length > 0) { echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\\n"); }
    if(getValue("spin_angle") && (getValue("spin_angle") != "0" || getValue("spin_vjust") != "0.5")) {
        echo("p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=" + getValue("spin_angle") + ", vjust=" + getValue("spin_vjust") + "))\\n");
    }
  ')

  hist_component <- rk.plugin.component("Histogram", xml=list(dialog=hist_dialog), js=list(require=c("ggsurvey","ggplot2"), calculate=js_calc_hist, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # =========================================================================================
  # Component 6: Means Graph (Updated with Robust Join)
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
          "Labels" = labels_tab,
          "Style & Layout" = rk.XML.col(
             rk.XML.cbox(label="Flip coordinates", value="1", id.name="cbox_flip"),
             rk.XML.spinbox(label="Confidence level for error bars (%)", id.name="conf_level", min=1, max=99, initial=95),
             color_palette_dropdown,
             rk.XML.dropdown(label="Facet Layout", id.name="facet_layout", options=list(
              "Wrap (default)"=list(val="wrap",chk=TRUE), "Force to one row"=list(val="row"), "Force to one column"=list(val="col")
            ))
          ),
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
    var prefix_clean = getValue("prefix_clean_input");
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

    echo("est <- " + svyby_obj + "\\n");

    // Robust Pivot Strategy: Explicitly select ID + Target columns to avoid name collisions in join
    echo("piv1 <- est %>% dplyr::select(dplyr::all_of(c(" + id_vars.join(",") + ", " + estimate_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + estimate_array.join(",") + ")), names_to = \\"respuesta\\", values_to = \\"recuento\\")\\n");
    echo("piv2 <- est %>% dplyr::select(dplyr::all_of(c(" + id_vars.join(",") + ", " + se_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + se_array.join(",") + ")), names_to = \\"variable\\", values_to = \\"se\\")\\n");

    // Robust Join Key Matching
    if (estimate_array.length == 1 && se_array.length == 1) {
       // Single variable: Force the name match
       echo("piv2 <- dplyr::mutate(piv2, respuesta = " + estimate_array[0] + ")\\n");
    } else {
       // Multi variable: Rely on naming convention
       echo("piv2 <- dplyr::mutate(piv2, respuesta = stringr::str_remove(variable, \\"^se\\\\\\\\.\\"))\\n");
    }

    echo("piv3 <- dplyr::left_join(piv1, piv2, by = c(" + id_vars.join(", ") + ", \\"respuesta\\"))\\n");

    if(prefix_clean){
      echo("piv3[[\\"respuesta\\"]] <- gsub(\\"" + prefix_clean + "\\", \\"\\", piv3[[\\"respuesta\\"]] )\\n");
    }
    echo("piv3[[\\"respuesta\\"]] <- forcats::fct_rev(piv3[[\\"respuesta\\"]] )\\n");

    echo("p <- ggplot2::ggplot(piv3, ggplot2::aes(x = " + xaxis_clean + ", y = recuento, color = respuesta, group = respuesta)) +\\n");
    echo("  ggplot2::geom_point(size=2) +\\n");
    echo("  ggplot2::geom_errorbar(ggplot2::aes(ymin = recuento - ci_multiplier*se, ymax = recuento + ci_multiplier*se), width = 0.2) +\\n");
    echo("  ggplot2::scale_color_brewer(palette = \\"" + getValue("palette_input") + "\\", labels = function(x) stringr::str_wrap(x, width = 20)) +\\n");

    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \\"" + getValue("title_input") + "\\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \\"" + getValue("subtitle_input") + "\\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \\"" + getValue("xlab_input") + "\\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \\"" + getValue("ylab_input") + "\\""); }
    if(getValue("legend_title_input")) { labs_list.push("color = \\"" + getValue("legend_title_input") + "\\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \\"" + getValue("caption_input") + "\\""); }
    if(labs_list.length > 0) { echo("  ggplot2::labs(" + labs_list.join(", ") + ") +\\n"); }

    echo("  ggplot2::theme_bw()\\n");

    if(getValue("cbox_flip") == "1") { echo("p <- p + ggplot2::coord_flip()\\n"); }

    if(facet_var_full){
      var facet_layout = getValue("facet_layout");
      var facet_opts = "";
      if (facet_layout == "row") { facet_opts = ", nrow = 1"; }
      else if (facet_layout == "col") { facet_opts = ", ncol = 1"; }
      echo("p <- p + ggplot2::facet_wrap(~ " + getColumnName(facet_var_full) + facet_opts + ")\\n");
    }
  ')
  means_component <- rk.plugin.component("Means Graph", xml=list(dialog=means_dialog), js=list(require=c("ggplot2", "tidyr", "dplyr", "forcats", "stringr", "RColorBrewer"), calculate=js_calc_means, printout=js_print_graph), hierarchy = list("Survey","Graphs","ggGraphs"))

  # =========================================================================================
  # Final Plugin Skeleton Call
  # =========================================================================================
  all_components <- list(
    bar_component,
    box_component,
    hex_component,
    hist_component,
    means_component
  )

  rk.plugin.skeleton(
    about = package_about,
    path = ".",
    xml = list(dialog = line_graph_dialog),
    js = list(
      require = c("survey", "dplyr", "ggplot2", "tidyr", "forcats", "stringr", "RColorBrewer"),
      calculate = js_calc_line_graph,
      printout = js_print_graph
    ),
    rkh = list(help = rk.rkh.doc(title=rk.rkh.title("Line Graph from svyby Object"))),
    components = all_components,
    pluginmap = list(name = "Line Graph", hierarchy = list("Survey","Graphs","ggGraphs")),
    create = c("pmap", "xml", "js", "desc", "rkh"),
    load = TRUE,
    overwrite = TRUE,
    show = FALSE
  )

  cat("\nFully optimized plugin package 'rk.ggsurvey' with 6 plugins generated.\n\nTo complete installation:\n\n")
  cat("  rk.updatePluginMessages(plugin.dir=\"rk.ggsurvey\")\n\n")
  cat("  devtools::install(\"rk.ggsurvey\")\n")
})
