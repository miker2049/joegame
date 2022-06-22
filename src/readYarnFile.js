
/**
 * Source code taken from  https://github.com/YarnSpinnerTool/YarnEditor/ under their MIT license
 * @arg {string} a yarn file string
 * @returns {any[]} array of nodes, just like the json
 */
module.exports = function(str) {
  let objects = []
  var lines = str.split(/\r?\n/);
  var obj = null;
  var index = 0;
  var readingBody = false;
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].trim() === '===') {
      readingBody = false;
      if (obj != null) {
        obj.body = obj.body.substr(0, obj.body.length - 1);
        objects.push(obj);
        obj = null;
      }
    } else if (readingBody) {
      obj.body += lines[i] + '\n';
    } else {
      if (lines[i].indexOf('title:') > -1) {
        if (obj == null) obj = {};
        obj.title = lines[i].substr(7, lines[i].length - 7);
      } else if (lines[i].indexOf('position:') > -1) {
        if (obj == null) obj = {};
        var xy = lines[i].substr(9, lines[i].length - 9).split(',');
        obj.position = { x: Number(xy[0].trim()), y: Number(xy[1].trim()) };
      } else if (lines[i].indexOf('colorID:') > -1) {
        if (obj == null) obj = {};
        obj.colorID = Number(
          lines[i].substr(9, lines[i].length - 9).trim()
        );
      } else if (lines[i].indexOf('tags:') > -1) {
        if (obj == null) obj = {};
        obj.tags = lines[i].substr(6, lines[i].length - 6);
      } else if (lines[i].trim() == '---') {
        readingBody = true;
        obj.body = '';
      }
    }
  }
  if (obj != null) {
    objects.push(obj);
  }
  return objects
}
