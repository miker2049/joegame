import marked from "marked";
/**
 * takes a string and returns an html element
 * {string}:
 */
export default function mdParse(input) {
    let html = marked(input);
    // let element: HTMLElement = htmlToElement(html);
    return html;
}
function htmlToElement(html) {
    var template = document.createElement('template');
    html = html.trim(); // Never return a text node of whitespace as the result
    template.innerHTML = html;
    return template.content.firstChild;
}
//# sourceMappingURL=mdParse.js.map