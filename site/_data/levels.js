const recursive = require("recursive-readdir");


module.exports = async function() {

    let files = await recursive("site/_includes/levels/");
    return files.map(val => {
        let out = val.replace(/site\/\_includes\/levels\//g, '')
        out = out.replace(/\.js$/, "")
        return out
    })
}
