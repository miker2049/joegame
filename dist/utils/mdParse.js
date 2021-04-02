"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = mdParse;

var _marked = _interopRequireDefault(require("marked"));

/**
 * takes a string and returns an html element
 * {string}:
 */
function mdParse(input) {
  var html = (0, _marked.default)(input); // let element: HTMLElement = htmlToElement(html);

  return html;
}

function htmlToElement(html) {
  var template = document.createElement('template');
  html = html.trim(); // Never return a text node of whitespace as the result

  template.innerHTML = html;
  return template.content.firstChild;
}
//# sourceMappingURL=mdParse.js.map