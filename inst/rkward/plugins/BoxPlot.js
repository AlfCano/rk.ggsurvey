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
		echo("if(!base::require(ggsurvey)){stop(" + i18n("Preview not available, because package ggsurvey is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(ggsurvey)\n");
	}	if(is_preview) {
		echo("if(!base::require(ggplot2)){stop(" + i18n("Preview not available, because package ggplot2 is not installed or cannot be loaded.") + ")}\n");
	} else {
		echo("require(ggplot2)\n");
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
    echo("p <- " + plot_call + "\n");

    if(getValue("cbox_flip") == "1") { echo("p <- p + ggplot2::coord_flip()\n"); }
    var labs_list = new Array();
    if(getValue("title_input")) { labs_list.push("title = \"" + getValue("title_input") + "\""); }
    if(getValue("subtitle_input")) { labs_list.push("subtitle = \"" + getValue("subtitle_input") + "\""); }
    if(getValue("xlab_input")) { labs_list.push("x = \"" + getValue("xlab_input") + "\""); }
    if(getValue("ylab_input")) { labs_list.push("y = \"" + getValue("ylab_input") + "\""); }
    if(getValue("caption_input")) { labs_list.push("caption = \"" + getValue("caption_input") + "\""); }
    if(labs_list.length > 0) { echo("p <- p + ggplot2::labs(" + labs_list.join(", ") + ")\n"); }
    if(getValue("spin_angle") && (getValue("spin_angle") != "0" || getValue("spin_vjust") != "0.5")) {
        echo("p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=" + getValue("spin_angle") + ", vjust=" + getValue("spin_vjust") + "))\n");
    }
  
}

function printout(is_preview){
	// read in variables from dialog


	// printout the results
	if(!is_preview) {
		new Header(i18n("Box Plot results")).print();	
	}
    if(!is_preview){
      var graph_options = new Array();
      graph_options.push("device.type=\"" + getValue("device_type") + "\"");
      graph_options.push("width=" + getValue("dev_width"));
      graph_options.push("height=" + getValue("dev_height"));
      graph_options.push("pointsize=10.0");
      graph_options.push("res=" + getValue("dev_res"));
      graph_options.push("bg=\"" + getValue("dev_bg") + "\"");
      if(getValue("device_type") == "JPG"){
        graph_options.push("quality=" + getValue("jpg_quality"));
      }
      echo("rk.graph.on(" + graph_options.join(", ") + ")\n");
    }
    echo("try(print(p))\n");
    if(!is_preview){
      echo("rk.graph.off()\n");
    }
  

}

