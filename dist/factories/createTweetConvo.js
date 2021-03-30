"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _loadAfterLoad = _interopRequireDefault(require("../utils/loadAfterLoad"));

var _createCharacter = _interopRequireDefault(require("./createCharacter"));

var _wikiData = _interopRequireDefault(require("../utils/wikiData"));

var _joegameTypes = require("../joegameTypes");

var _TweetConvo = _interopRequireDefault(require("../components/TweetConvo"));

var _shuffleArr = _interopRequireDefault(require("../utils/shuffleArr"));

/*
 * Until further notice this takes tile coords
 */
function _default(_x, _x2, _x3, _x4, _x5) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(level, tx, ty, charGroup, convoID) {
    var mani, convoIDD, convoJsonPath, convo, users, charAmount, listOfChars, chars, i, char, _char, _char2, _char3, tconvo;

    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            mani = level.scene.cache.json.get('convo-manifest').files;
            convoIDD = mani[randomIndexx(mani)].match(/(\d+)(_single)?\.json$/)[1];
            convoJsonPath = mani.find(function (entry) {
              return entry.match(convoIDD);
            }); // console.log('loading ' + convoIDD)

            _context.next = 5;
            return (0, _loadAfterLoad.default)(level.scene, convoIDD, 'assets/convos/' + convoJsonPath);

          case 5:
            // console.log('loaded ' + convoIDD)
            convo = level.scene.cache.json.get(convoIDD);
            users = Array.from(new Set(convo.map(function (tweet) {
              return tweet.username;
            })));
            charAmount = Math.min(users.length, 4);
            listOfChars = Array.from((0, _wikiData.default)(level.scene.game).character);

            if (charGroup && charGroup !== 'all') {
              listOfChars = listOfChars.filter(function (char) {
                // console.log(char)
                if (char[1].charGroups) {
                  return char[1].charGroups.includes(charGroup);
                }
              });
            }

            listOfChars = (0, _shuffleArr.default)(listOfChars).slice(0, charAmount);
            chars = [];

            for (i = 0; i < listOfChars.length; i++) {
              if (i === 0) {
                // to the west
                char = (0, _createCharacter.default)(listOfChars[i][0], tx - level.map.tileWidth, ty, level);
                char.face(_joegameTypes.Dir.east);
                chars.push(char);
              } else if (i === 1) {
                // to the north
                _char = (0, _createCharacter.default)(listOfChars[i][0], tx, ty - level.map.tileHeight, level);

                _char.face(_joegameTypes.Dir.south);

                chars.push(_char);
              } else if (i === 2) {
                // to the east
                _char2 = (0, _createCharacter.default)(listOfChars[i][0], tx + level.map.tileWidth, ty, level);

                _char2.face(_joegameTypes.Dir.west);

                chars.push(_char2);
              } else if (i === 3) {
                // to the south
                _char3 = (0, _createCharacter.default)(listOfChars[i][0], tx, ty + level.map.tileHeight, level);

                _char3.face(_joegameTypes.Dir.north);

                chars.push(_char3);
              }
            }

            chars.forEach(function (c) {
              return level.scene.add.existing(c);
            }); // const characters = charAmount.map(n=>{
            //     if(n===1){
            //         createCharacter
            //     }
            // })
            // console.log(convo)

            tconvo = new _TweetConvo.default(chars, convo, users, level);
            return _context.abrupt("return", tconvo);

          case 16:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _ref.apply(this, arguments);
}

function randomIndexx(arr) {
  return Math.floor(Math.random() * arr.length);
}