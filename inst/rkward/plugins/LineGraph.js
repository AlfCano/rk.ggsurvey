// this code was generated using the rkwarddev package.
// perhaps don't make changes here, but in the rkwarddev script instead!

function preview(){
	preprocess(true);
	calculate(true);
	printout(true);
}

function preprocess(is_preview){
	// add requirements etc. here
	if(is_preview) {
		echo("if(!base::require(ggplot2)){stop(" + i18n("Preview not available, because package ggplot2 is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(ggplot2)\n");
	}	if(is_preview) {
		echo("if(!base::require(dplyr)){stop(" + i18n("Preview not available, because package dplyr is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(dplyr)\n");
	}	if(is_preview) {
		echo("if(!base::require(tidyr)){stop(" + i18n("Preview not available, because package tidyr is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(tidyr)\n");
	}	if(is_preview) {
		echo("if(!base::require(RColorBrewer)){stop(" + i18n("Preview not available, because package RColorBrewer is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(RColorBrewer)\n");
	}	if(is_preview) {
		echo("if(!base::require(scales)){stop(" + i18n("Preview not available, because package scales is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(scales)\n");
	}	if(is_preview) {
		echo("if(!base::require(stringr)){stop(" + i18n("Preview not available, because package stringr is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(stringr)\n");
	}
}

function calculate(is_preview){
	// read in variables from dialog


	// the R code to be evaluated

    function getColumnName(fullName) {
        if (!fullName) return "";
        var lastBracketPos = fullName.lastIndexOf("[[");
        if (lastBracketPos > -1) {
            var lastPart = fullName.substring(lastBracketPos);
            var match = lastPart.match(/\[\[\"(.*?)\"\]\]/);
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
   
    var svyby_obj = getValue("svyby_object");
    if(!svyby_obj) return;
    var xaxis_clean = getColumnName(getValue("xaxis_var"));
    var group_var_full = getValue("group_var");
    var group_aes_mode = getValue("group_aes_mode");
    var facet_var_full = getValue("facet_var");
    var estimate_vars_full = getValue("estimate_vars");
    var se_vars_full = getValue("se_vars");
    var conf_level = getValue("conf_level") / 100;

    echo("ci_multiplier <- qnorm(1 - (1 - " + conf_level + ") / 2)\n");
    var estimate_array = estimate_vars_full.split(/\n/).filter(function(n){ return n != "" }).map(function(item) { return "\"" + getColumnName(item) + "\""; });
    var se_array = se_vars_full.split(/\n/).filter(function(n){ return n != "" }).map(function(item) { return "\"" + getColumnName(item) + "\""; });

    var id_vars = new Array();
    id_vars.push("\"" + xaxis_clean + "\"");
    if(group_var_full){ id_vars.push("\"" + getColumnName(group_var_full) + "\""); }
    if(facet_var_full){ id_vars.push("\"" + getColumnName(facet_var_full) + "\""); }

    echo("est <- " + svyby_obj + " %>% dplyr::mutate(rk_internal_id = dplyr::row_number())\n");

    var selection_vars_piv1 = id_vars.join(",") + ", \"rk_internal_id\"";
    echo("piv1 <- est %>% dplyr::select(dplyr::all_of(c(" + selection_vars_piv1 + ", " + estimate_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + estimate_array.join(",") + ")), names_to = \"respuesta\", values_to = \"recuento\")\n");

    var selection_vars_piv2 = "\"rk_internal_id\"";
    echo("piv2 <- est %>% dplyr::select(dplyr::all_of(c(" + selection_vars_piv2 + ", " + se_array.join(",") + "))) %>% tidyr::pivot_longer(cols=dplyr::all_of(c(" + se_array.join(",") + ")), names_to = \"variable\", values_to = \"se\")\n");

    if (estimate_array.length == 1 && se_array.length == 1) {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = " + estimate_array[0] + ")\n");
    } else {
       echo("piv2 <- dplyr::mutate(piv2, respuesta = stringr::str_remove(variable, \"^se\\\\.\"))\n");
    }
    echo("piv3 <- dplyr::left_join(piv1, piv2, by = c(\"rk_internal_id\", \"respuesta\"))\n");

    // NEW: Clean legend prefix
    var clean_prefix = getValue("clean_legend_prefix");
    if (clean_prefix) {
        echo("piv3[[\"respuesta\"]] <- stringr::str_remove(piv3[[\"respuesta\"]], \"" + clean_prefix + "\")\n");
    }

    echo("piv3[[\"respuesta\"]] <- forcats::fct_rev(piv3[[\"respuesta\"]] )\n");

    if (getValue("order_x_est") == "1") {
       echo("piv3[[\"" + xaxis_clean + "\"]] <- forcats::fct_reorder(as.factor(piv3[[\"" + xaxis_clean + "\"]]), piv3$recuento)\n");
    } else {
       echo("piv3[[\"" + xaxis_clean + "\"]] <- as.factor(piv3[[\"" + xaxis_clean + "\"]])\n");
    }
    if (getValue("invert_order") == "1") {
       echo("piv3[[\"" + xaxis_clean + "\"]] <- forcats::fct_rev(piv3[[\"" + xaxis_clean + "\"]])\n");
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

    echo("p <- ggplot2::ggplot(piv3, ggplot2::aes(" + aes_call + ")) +\n");
    echo("  ggplot2::geom_line(linewidth=1) +\n");
    echo("  ggplot2::geom_point(size=2) +\n");
    echo("  ggplot2::geom_errorbar(ggplot2::aes(ymin = recuento - ci_multiplier*se, ymax = recuento + ci_multiplier*se), width = 0.2) +\n");
    echo("  ggplot2::theme_bw()\n");

    // PALETTE
    var palette = getValue("palette_input");
    var legend_wrap_width = getValue("legend_wrap_width");
    var label_wrap_call = "";
    if (legend_wrap_width && parseInt(legend_wrap_width) > 0) {
        label_wrap_call = "labels = scales::label_wrap(" + legend_wrap_width + ")";
    }

    echo("n_colors <- length(unique(piv3[[\"" + color_var + "\"]]))\n");
    echo("if (n_colors > 9) {\n");
    echo("   get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, \"" + palette + "\"))\n");
    var manual_opts = "values = get_palette(n_colors)";
    if(label_wrap_call) manual_opts += ", " + label_wrap_call;
    echo("   p <- p + ggplot2::scale_color_manual(" + manual_opts + ")\n");
    echo("} else {\n");
    var brewer_opts = "palette = \"" + palette + "\"";
    if(label_wrap_call) brewer_opts += ", " + label_wrap_call;
    echo("   p <- p + ggplot2::scale_color_brewer(" + brewer_opts + ")\n");
    echo("}\n");

    if(facet_var_full){
      var facet_layout = getValue("facet_layout");
      var facet_opts = "";
      if (facet_layout == "row") { facet_opts = ", nrow = 1"; }
      else if (facet_layout == "col") { facet_opts = ", ncol = 1"; }
      echo("p <- p + ggplot2::facet_wrap(~ " + getColumnName(facet_var_full) + facet_opts + ")\n");
    }

    // LABELS
    var labs_list = [];
    var custom_xlab = getValue("plot_xlab");
    if (custom_xlab) { labs_list.push("x = \"" + custom_xlab + "\""); }
    else { labs_list.push("x = rk.get.label(" + getValue("xaxis_var") + ")"); }

    var custom_ylab = getValue("plot_ylab");
    if (custom_ylab) { labs_list.push("y = \"" + custom_ylab + "\""); }

    var custom_legend_title = getValue("plot_legend_title");
    var legend_title = custom_legend_title ? custom_legend_title : "Respuesta";

    if (group_var_full) {
        var g_title = custom_legend_title ? custom_legend_title : "Group";
        var r_title = "Respuesta";

        if (group_aes_mode == "both") {
            labs_list.push("color = \"" + g_title + "\"");
            labs_list.push("shape = \"" + g_title + "\"");
            labs_list.push("linetype = \"" + r_title + "\"");
        }
        else if (group_aes_mode == "color") {
            labs_list.push("color = \"" + g_title + "\"");
            labs_list.push("linetype = \"" + r_title + "\"");
        }
        else if (group_aes_mode == "shape") {
            labs_list.push("shape = \"" + g_title + "\"");
            if(custom_legend_title) labs_list.push("color = \"" + custom_legend_title + "\"");
            else labs_list.push("color = \"" + r_title + "\"");
        }
    } else {
        labs_list.push("color = \"" + legend_title + "\"");
    }

    if (getValue("plot_title")) { labs_list.push("title = \"" + getValue("plot_title") + "\""); }
    if (getValue("plot_subtitle")) { labs_list.push("subtitle = \"" + getValue("plot_subtitle") + "\""); }
    if (getValue("plot_caption")) { labs_list.push("caption = \"" + getValue("plot_caption") + "\""); }

    if (labs_list.length > 0) {
      echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\n");
    }

    // THEME
    var x_val_wrap = getValue("theme_x_val_wrap");
    if (x_val_wrap && parseInt(x_val_wrap) > 0) {
        echo("p <- p + ggplot2::scale_x_discrete(labels = scales::label_wrap(" + x_val_wrap + "))\n");
    }
    var y_val_wrap = getValue("theme_y_val_wrap");
    if (y_val_wrap && parseInt(y_val_wrap) > 0) {
        echo("p <- p + ggplot2::scale_y_continuous(labels = scales::label_wrap(" + y_val_wrap + "))\n");
    }

    var theme_list = [];
    if(getValue("theme_text_rel") != 1) { theme_list.push("text = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_text_rel") + "))"); }
    if(getValue("theme_title_rel") != 1.2) { theme_list.push("plot.title = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_title_rel") + "))"); }
    if(getValue("theme_legend_rel") != 0.8) { theme_list.push("legend.text = ggplot2::element_text(size = ggplot2::rel(" + getValue("theme_legend_rel") + "))"); }
    if(getValue("theme_legend_pos") != "right") { theme_list.push("legend.position = \"" + getValue("theme_legend_pos") + "\""); }
    var x_angle = getValue("theme_x_angle");
    var x_hjust = getValue("theme_x_hjust");
    var x_vjust = getValue("theme_x_vjust");
    if(x_angle != 0 || x_hjust != 0.5 || x_vjust != 0.5) {
        theme_list.push("axis.text.x = ggplot2::element_text(angle=" + x_angle + ", hjust=" + x_hjust + ", vjust=" + x_vjust + ")");
    }
    if(theme_list.length > 0) {
      echo("p <- p + ggplot2::theme(" + theme_list.join(", ") + ")\n");
    }
  
}

function printout(is_preview){
	// read in variables from dialog


	// printout the results
	if(!is_preview) {
		new Header(i18n("Line Graph results")).print();	
	}
    if(!is_preview){
      var graph_options = [];
      graph_options.push("device.type=\"" + getValue("device_type") + "\"");
      graph_options.push("width=" + getValue("dev_width"));
      graph_options.push("height=" + getValue("dev_height"));
      graph_options.push("pointsize=10.0");
      graph_options.push("res=" + getValue("dev_res"));
      graph_options.push("bg=\"" + getValue("dev_bg") + "\"");
      if(getValue("device_type") === "JPG"){
        graph_options.push("quality=" + getValue("jpg_quality"));
      }
      echo("try(rk.graph.on(" + graph_options.join(", ") + "))\n");
    }
    echo("try(print(p))\n");
    if(!is_preview){
      echo("try(rk.graph.off())\n");
    }
  

}

