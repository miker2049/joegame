const CleanCSS = require("clean-css");
const UglifyJS = require("uglify-js");
const path = require("path")

module.exports = function(eleventyConfig) {

    eleventyConfig.setUseGitIgnore(false);

    eleventyConfig.addPassthroughCopy("assets")
    eleventyConfig.addPassthroughCopy({ "bundle/joegame-lib.min.js": "joegame-lib.min.js" })

    eleventyConfig.addPassthroughCopy({ "node_modules/gravisPats/gravis": "gravis" })
    eleventyConfig.addPassthroughCopy({ "node_modules/gravisPats/gravis-Standard": "gravis-Standard" })
    eleventyConfig.addPassthroughCopy({ "node_modules/gravisPats/gravis.cfg": "gravis.cfg" })


    eleventyConfig.addPassthroughCopy({ "node_modules/timidity-wasm/dist/worklet-bundle.js": "worklet-bundle.js" })
    eleventyConfig.addPassthroughCopy({ "node_modules/joegame-twitter-dialogue-scraper/convos": "assets/convos" })


    eleventyConfig.addPassthroughCopy({ "pwa": "/" })
    eleventyConfig.addPassthroughCopy("favicon.ico")
    // eleventyConfig.addPassthroughCopy("pwa/*")

    eleventyConfig.addFilter("cssmin", function(code) {
        return new CleanCSS({}).minify(code).styles;
    });

    eleventyConfig.addFilter("jsmin", function(code) {
        return new UglifyJS.minify(code).code;
    });

    // eleventyConfig.addWatchTarget("./public/joegame-lib.min.js");



    return {
        dir: {
            input: "site",
            output: "public"
        }
    }
};
