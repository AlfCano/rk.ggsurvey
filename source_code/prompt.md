# The Golden Rules of RKWard Plugin Development (Revised & Expanded)

*Based on `rkwarddev` version `0.10-3` and lessons learned from rigorous debugging.*

### 1. The R Script is the Single Source of Truth
*   Your sole output will be a single R script that defines all plugin components as R objects and uses `rk.plugin.skeleton()` to write the final files. This script **must** be wrapped in `local({})` to avoid polluting the user's global environment.
*   The script must begin with `require(rkwarddev)` and a `rkwarddev.required()` check.

### 2. The Mandate of Explicit IDs (For Widgets Only)
*   **Every interactive UI element** (`varslot`, `input`, `cbox`, `dropdown`, `saveobj`, `preview`, etc.) **must** be assigned a unique, hard-coded `id.name`. This is the primary defense against "Can't find an ID!" and "subscript out of bounds" errors.
*   **CRITICAL:** **Do not** assign an `id.name` to layout containers (`rk.XML.col`, `rk.XML.row`, `rk.XML.frame`). The `rkwarddev` framework manages these automatically, and manual IDs will conflict with its internal system, causing errors.

### 3. The Inflexible One-`varselector`-to-Many-`varslot`s Pattern
*   The `source` argument of every `varslot` that depends on a selection **must** be the same `id.name` from the parent `varselector`.
*   To select variables from a data frame *inside* another object (like a `svydesign` object), you **must** set the property *after* creating the object: `attr(my_varslot, "source_property") <- "variables"`. Passing it as a direct argument will fail.

### 4. The `<logic>` Section is Forbidden
*   The `<logic>` section, including `rk.XML.connect()`, is fragile, highly sensitive to the `rkwarddev` version, and the most common source of obscure, hard-to-debug errors.
*   **All conditional behavior must be handled inside the `calculate` JavaScript string.** It is always better to have a slightly less "slick" UI (e.g., an input field that is always enabled) than a plugin that fails to load due to an incompatible `<logic>` tag.

### 5. The Immutable Raw JavaScript String Paradigm
You **must avoid programmatic JavaScript generation** and write self-contained, multi-line R character strings.

*   **Master `getValue()`:** Begin every `calculate` block by declaring a JavaScript variable for every UI component's `id.name`.
*   **The Robust `getColumnName` Helper is Mandatory:** For selecting variables from *any* object, you **must** include this specific, robust helper function to prevent crashes from `null` matches:
    ```javascript
    function getColumnName(fullName) {
        if (!fullName) return "";
        var lastBracketPos = fullName.lastIndexOf("[[");
        if (lastBracketPos > -1) {
            var lastPart = fullName.substring(lastBracketPos);
            var match = lastPart.match(/\\[\\[\\"(.*?)\\"\\]\\]/);
            if (match) { // Check if match is not null
                return match;
            }
        }
        if (fullName.indexOf("$") > -1) {
            return fullName.substring(fullName.lastIndexOf("$") + 1);
        } else {
            return fullName;
        }
    }
    ```
*   **Handle `varslot` Multi-Selections Correctly:** A multi-select `varslot` returns a single string with values separated by newlines. You **must** split this string using `split(/\\n/)`, not `split(/\\s+/)`, to correctly handle variable names that contain spaces.
*   **Master Escaping for Regex:** To generate a regular expression in R code from JavaScript, you must escape the backslashes at multiple levels. To produce `\\.` in the final R code, the JavaScript string must contain `\\\\.`.

### 6. The Sacred `is_preview` Pattern for Plots
*   The `printout` script is run for both the final submission and the preview pane. It has access to a built-in boolean JavaScript variable, `is_preview`.
*   Plotting device commands **must** be wrapped in a check to ensure they only run on final submission. This is the only correct way to implement a plot preview.
    ```javascript
    if(!is_preview){
      echo("rk.graph.on(...)");
    }
    echo("try(print(p))");
    if(!is_preview){
      echo("rk.graph.off()");
    }
    ```

### 7. The `calculate`/`printout` Separation of Concerns
*   **The `calculate` Block:** Generates the R code for the **entire computation sequence**, including data wrangling and plot object creation (e.g., `p <- ggplot(...)`). It **must** assign the final result object to a hard-coded name.
*   **The `printout` Block:** Its **only** purpose is to display the final result. For plots, it should contain the `is_preview` logic and the `print(p)` call. For text output, it should be a simple `rk.header()` and `print(result_object)`. **Avoid complex `if` statements here.**

### 8. Precision in `rkwarddev` Function Calls
*   The target `rkwarddev` version has specific function signatures that must be followed.
*   **`rk.XML.cbox`:** You **must** use `rk.XML.cbox(..., value="1")`.
*   **Placeholders:** The correct function for a simple text label is `rk.XML.text()`, not `rk.XML.label()`.
*   **Trust But Verify Function Arguments:** Do not assume a wrapper function (like in `ggsurvey`) accepts the same arguments as its underlying base function (like in `ggplot2`). Always check the documentation for the specific function you are calling. The `position` and `bins` arguments were prime examples of this error.

### 9. Correct Component Architecture
*   For a package with multiple plugins, the main plugin's definition is passed directly to `rk.plugin.skeleton()`.
*   All other plugins **must** be defined with `rk.plugin.component()` and passed as a `list` to the `components` argument of the main call.
*   The `hierarchy` list must use the correct, **case-sensitive** names for RKWard's menus (e.g., `"analysis"`, not `"Analysis"`).

### 10. The Sanctity of XML Quoting
*   When defining R objects that generate XML, the R string literal should use single quotes (`'...'`). The XML attributes within that string must use double quotes (`"`). If the R code *inside* an attribute (like `initial`) needs its own string quotes, it **must** use single quotes (`'...'`).
*   **Valid:** `rk.XML.input(initial = 'list(val = "A")')`
*   **Invalid:** `rk.XML.input(initial = "list(val = "A")")` will fail to parse.
