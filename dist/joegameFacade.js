"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var _gameconfig = _interopRequireDefault(require("./gameconfig"));

var _IjoegameFacade2 = _interopRequireDefault(require("./IjoegameFacade"));

var _loadMapJSON2 = _interopRequireDefault(require("./utils/loadMapJSON"));

var _loadMapAssets = _interopRequireDefault(require("./utils/loadMapAssets"));

var _createAnims = _interopRequireDefault(require("./utils/createAnims"));

var _createBaseLevel = _interopRequireDefault(require("./factories/createBaseLevel"));

var _addAllNPCsFromLayer = _interopRequireDefault(require("./actions/addAllNPCsFromLayer"));

var _addAllTweetConvosFromLayer = _interopRequireDefault(require("./actions/addAllTweetConvosFromLayer"));

var _addAllObjectsFromLayer = _interopRequireDefault(require("./actions/addAllObjectsFromLayer"));

var _addAllPlatformsFromLayer = _interopRequireDefault(require("./actions/addAllPlatformsFromLayer"));

var _addPlayerToLevel = _interopRequireDefault(require("./actions/addPlayerToLevel"));

var _createLevelPhysics = _interopRequireDefault(require("./factories/createLevelPhysics"));

var _createDepthMap = _interopRequireDefault(require("./utils/createDepthMap"));

var _runCinematicNode = _interopRequireDefault(require("./actions/runCinematicNode"));

var _createTweetConvo = _interopRequireDefault(require("./factories/createTweetConvo"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var joegameFacade = /*#__PURE__*/function (_IjoegameFacade) {
  (0, _inherits2.default)(joegameFacade, _IjoegameFacade);

  var _super = _createSuper(joegameFacade);

  function joegameFacade() {
    var _this;

    (0, _classCallCheck2.default)(this, joegameFacade);

    for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
      args[_key] = arguments[_key];
    }

    _this = _super.call.apply(_super, [this].concat(args));
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "createAnims", _createAnims.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "runLevelScene", _createBaseLevel.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "addAllNPCsFromLayer", _addAllNPCsFromLayer.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "addAllTweetConvosFromLayer", _addAllTweetConvosFromLayer.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "addAllObjectsFromLayer", _addAllObjectsFromLayer.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "addAllPlatformsFromLayer", _addAllPlatformsFromLayer.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "addPlayerToLevel", _addPlayerToLevel.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "createLevelPhysics", _createLevelPhysics.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "createDepthMap", _createDepthMap.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "runCinematicNode", _runCinematicNode.default);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "createTweetConvo", _createTweetConvo.default);
    return _this;
  }

  (0, _createClass2.default)(joegameFacade, [{
    key: "initGame",
    value: function initGame(gdata, convo) {
      return new Promise(function (resolve, reject) {
        new Phaser.Game((0, _gameconfig.default)(gdata, convo, resolve));
      });
    }
  }, {
    key: "loadMapJSON",
    value: function loadMapJSON(game, mapjsonpath) {
      return (0, _loadMapJSON2.default)(game, mapjsonpath);
    }
  }, {
    key: "loadAssets",
    value: function loadAssets(game, mapjsonpath) {
      return (0, _loadMapAssets.default)(game, mapjsonpath);
    }
  }]);
  return joegameFacade;
}(_IjoegameFacade2.default);

exports.default = joegameFacade;