const eleventyNavigationPlugin = require("@11ty/eleventy-navigation");
const CleanCSS = require("clean-css");
const UglifyJS = require("uglify-js");
const path = require("path")

module.exports = function(eleventyConfig) {

    eleventyConfig.addPlugin(eleventyNavigationPlugin);

    eleventyConfig.setUseGitIgnore(false);

    eleventyConfig.addPassthroughCopy("assets")
    eleventyConfig.addPassthroughCopy({ "bundle/joegame-lib.min.js": "joegame-lib.min.js" })

    eleventyConfig.addPassthroughCopy({ "node_modules/gravisPats/gravis": "gravis" })
    eleventyConfig.addPassthroughCopy({ "node_modules/gravisPats/gravis-Standard": "gravis-Standard" })
    eleventyConfig.addPassthroughCopy({ "node_modules/gravisPats/gravis.cfg": "gravis.cfg" })

    eleventyConfig.addPassthroughCopy({ "node_modules/timidity-wasm/dist/worklet-bundle.js": "worklet-bundle.js" })

    eleventyConfig.addPassthroughCopy({ "pwa": "/" })
    eleventyConfig.addPassthroughCopy("favicon.ico")

    eleventyConfig.addFilter("cssmin", function(code) {
        return new CleanCSS({}).minify(code).styles;
    });

    eleventyConfig.addFilter("jsmin", function(code) {
        return new UglifyJS.minify(code).code;
    });

    return {
        dir: {
            input: "site",
            output: "public"
        }
    }
};
