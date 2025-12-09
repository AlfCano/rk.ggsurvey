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
    var x_var = getColumnName(getValue("x_var"));
    if(!svy_obj) return;
    echo("p <- ggsurvey::ggbarweight_svy(" + svy_obj + ", " + x_var + ")\n");
  
}

function printout(is_preview){
	// read in variables from dialog


	// printout the results
	if(!is_preview) {
		new Header(i18n("Hexbin Plot results")).print();	
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

