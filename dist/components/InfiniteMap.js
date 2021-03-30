"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.InfiniteMap = exports.MapObjectWatcher = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

require("phaser");

var _joegameTypes = require("../joegameTypes");

var _getTileFromPoint = _interopRequireDefault(require("../utils/getTileFromPoint"));

var MapObjectWatcher = /*#__PURE__*/function () {
  function MapObjectWatcher(obj, limit, tileSize, mapTWidth, mapTHeight) {
    (0, _classCallCheck2.default)(this, MapObjectWatcher);
    this.obj = obj;
    this.tileSize = tileSize;
    this.mapTWidth = mapTWidth;
    this.mapTHeight = mapTHeight; //in tile units

    this.limit = limit; //these are in tile units
    // this.watchNorth = Math.round((this.obj.y - (this.mapTHeight*this.tileSize))/this.tileSize)+limit
    // this.watchSouth = Math.round(((this.obj.y+this.obj.height) + (this.mapTHeight*this.tileSize))/this.tileSize)-limit
    // this.watchEast = Math.round((this.obj.x + (this.mapTWidth*this.tileSize))/this.tileSize)-limit
    // this.watchWest = Math.round(((this.obj.x+this.obj.width) - (this.mapTWidth*this.tileSize))/this.tileSize)+limit
  }

  (0, _createClass2.default)(MapObjectWatcher, [{
    key: "shift",
    value: function shift(dir) {
      switch (dir) {
        case _joegameTypes.Dir.north:
          {
            this.obj.y -= this.mapTHeight * this.tileSize; // this.watchNorth = (this.obj.y + this.limit)/this.tileSize

            break;
          }

        case _joegameTypes.Dir.south:
          {
            this.obj.y += this.mapTHeight * this.tileSize; // this.watchNorth = (this.obj.y + this.limit)/this.tileSize

            break;
          }

        case _joegameTypes.Dir.east:
          {
            this.obj.x += this.mapTWidth * this.tileSize; // this.watchNorth = (this.obj.y + this.limit)/this.tileSize

            break;
          }

        case _joegameTypes.Dir.west:
          {
            this.obj.x -= this.mapTWidth * this.tileSize; // this.watchNorth = (this.obj.y + this.limit)/this.tileSize

            break;
          }
      }
    }
  }, {
    key: "watchNorth",
    get: function get() {
      return Math.round((this.obj.y - this.mapTHeight * this.tileSize) / this.tileSize) + this.limit;
    }
  }, {
    key: "watchSouth",
    get: function get() {
      return Math.round((this.obj.y + this.obj.height + this.mapTHeight * this.tileSize) / this.tileSize) - this.limit;
    }
  }, {
    key: "watchEast",
    get: function get() {
      return Math.round((this.obj.x + this.mapTWidth * this.tileSize) / this.tileSize) - this.limit;
    }
  }, {
    key: "watchWest",
    get: function get() {
      return Math.round((this.obj.x + this.obj.width - this.mapTWidth * this.tileSize) / this.tileSize) + this.limit;
    }
  }, {
    key: "check",
    value: function check(x, y) {
      console.log('y', y, 'this.watchNorth', this.watchNorth);

      if (x <= this.watchWest) {
        this.shift(_joegameTypes.Dir.west);
      } else if (x >= this.watchEast) {
        this.shift(_joegameTypes.Dir.east);
      } else if (y <= this.watchNorth) {
        this.shift(_joegameTypes.Dir.north);
      } else if (y >= this.watchSouth) {
        this.shift(_joegameTypes.Dir.south);
      }
    }
  }]);
  return MapObjectWatcher;
}();

exports.MapObjectWatcher = MapObjectWatcher;

var InfiniteMap = /*#__PURE__*/function () {
  function InfiniteMap(params) {
    var _this = this;

    (0, _classCallCheck2.default)(this, InfiniteMap);
    this.tilemap = params.tilemap;
    var camera = params.scene.cameras.main;
    this.tileOffsetX = 0;
    this.tileOffsetY = 0; // let raw=this.params.scene.cache.json.get(getMapKeyNameRaw(this.params.tilemapPath)) as TiledRawJson

    this.mapTileWidth = this.tilemap.width;
    this.mapTileHeight = this.tilemap.height;
    this.tileSize = this.tilemap.tileWidth;
    this.objWatchers = params.mapObjects.map(function (mo) {
      return new MapObjectWatcher(mo, 4, _this.tileSize, _this.tilemap.width, _this.tilemap.height);
    });
    this.cameraCenter = (0, _getTileFromPoint.default)(camera.midPoint, this.tilemap.tileWidth);
    camera.on('followupdate', function (cam, gob) {
      var newmid = (0, _getTileFromPoint.default)(camera.midPoint, _this.tilemap.tileWidth);

      if (typeof _this.cameraCenter.x === 'number' && typeof _this.cameraCenter.y === 'number' && typeof newmid.x === 'number' && typeof newmid.y === 'number') {
        if (newmid.y < _this.cameraCenter.y) {
          _this.cameraCenter.y = newmid.y;

          _this.shiftNorth();

          _this.objWatchers.forEach(function (w) {
            return w.check(newmid.x, newmid.y);
          });
        } else if (newmid.y > _this.cameraCenter.y) {
          _this.cameraCenter.y = newmid.y;

          _this.shiftSouth();

          _this.objWatchers.forEach(function (w) {
            return w.check(newmid.x, newmid.y);
          });
        } else if (newmid.x > _this.cameraCenter.x) {
          _this.cameraCenter.x = newmid.y;

          _this.shiftEast();

          _this.objWatchers.forEach(function (w) {
            return w.check(newmid.x, newmid.y);
          });
        } else if (newmid.x < _this.cameraCenter.x) {
          _this.cameraCenter.x = newmid.x;

          _this.shiftWest();

          _this.objWatchers.forEach(function (w) {
            return w.check(newmid.x, newmid.y);
          });
        }
      }
    });
  }

  (0, _createClass2.default)(InfiniteMap, [{
    key: "tileWidth",
    get: function get() {
      return this.tilemap.tileWidth;
    }
  }, {
    key: "tileHeight",
    get: function get() {
      return this.tilemap.tileHeight;
    }
  }, {
    key: "height",
    get: function get() {
      return this.tilemap.height;
    }
  }, {
    key: "width",
    get: function get() {
      return this.tilemap.width;
    }
  }, {
    key: "tilesets",
    get: function get() {
      return this.tilemap.tilesets;
    }
  }, {
    key: "getObjectLayer",
    value: function getObjectLayer(layer) {
      return this.tilemap.getObjectLayer(layer);
    }
  }, {
    key: "getLayer",
    value: function getLayer(layer) {
      return this.tilemap.getLayer(layer);
    }
  }, {
    key: "getTileAt",
    value: function getTileAt(x, y, nonNull, layer) {
      return this.tilemap.getTileAt(x, y, nonNull, layer);
    }
  }, {
    key: "shiftNorth",
    value: function shiftNorth() {
      var _this2 = this;

      this.tilemap.layers.forEach(function (l) {
        console.log(_this2.mapTileHeight - 1);
        l.tilemapLayer.copy(0, _this2.mapTileHeight - 1, _this2.mapTileWidth, 1, 0, 0);
      });
    }
  }, {
    key: "shiftSouth",
    value: function shiftSouth() {}
  }, {
    key: "shiftEast",
    value: function shiftEast() {}
  }, {
    key: "shiftWest",
    value: function shiftWest() {}
  }]);
  return InfiniteMap;
}();

exports.InfiniteMap = InfiniteMap;