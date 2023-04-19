CREATE TABLE IF NOT EXISTS "anim_keys" (
	"id"	INTEGER NOT NULL,
	"anim_id"	INTEGER NOT NULL UNIQUE, -- the in-game identifier for one anim
	"anim_key"	TEXT NOT NULL UNIQUE, -- the in-game identifier for one anim
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "anim_frames" (
	"id"	INTEGER NOT NULL,
	"anim_id"	INTEGER NOT NULL, -- the identifier for one anim
	"spritesheet"	INTEGER NOT NULL,
	"spritesheet_frame" INTEGER NOT NULL, -- the frame of the spritesheet
	"frame_number"	INTEGER NOT NULL, --the location within the anim sequence
	FOREIGN KEY("spritesheet") REFERENCES "spritesheets" ("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "bodies" (
	"id"	INTEGER NOT NULL,
	"anim_north"	INTEGER NOT NULL,
	"anim_south"	INTEGER NOT NULL,
	"anim_east"	INTEGER NOT NULL,
	"anim_west"	INTEGER NOT NULL,
	"speed"	INTEGER NOT NULL,
	"dash_distance"	INTEGER NOT NULL,
	"scale"	INTEGER NOT NULL,
	"body_offset_x"	INTEGER NOT NULL,
	"body_offset_y"	INTEGER NOT NULL,
	"width"	INTEGER NOT NULL,
	"height"	INTEGER NOT NULL,
	FOREIGN KEY("anim_north") REFERENCES "anim_frames" ("anim_id"),
	FOREIGN KEY("anim_south") REFERENCES "anim_frames" ("anim_id"),
	FOREIGN KEY("anim_east") REFERENCES "anim_frames" ("anim_id"),
	FOREIGN KEY("anim_west") REFERENCES "anim_frames" ("anim_id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "characters" (
       "id" INTEGER NOT NULL,
       "character_name" TEXT NOT NULL,
       "body" INTEGER NOT NULL,
	FOREIGN KEY("body") REFERENCES "bodies" ("id"),
	PRIMARY KEY("id")
);



CREATE TABLE IF NOT EXISTS "tweet_threads" (
"id" INTEGER PRIMARY KEY,
"position" INTEGER NOT NULL,
"convo_id" INTEGER NOT NULL, --the selector for a whole thread
"tweet_id" TEXT NOT NULL,
FOREIGN KEY("tweet_id") REFERENCES "tweets" ("tweet_id")
);
-- Example output of corrupted table tweet_threads
-- 387|0|50|1468865894432575489
-- 388|1|50|1469012986731896839
-- 389|2|50|1469173696053096455
-- 390|3|50|1469299573076250625
-- 2991|2|50|1526284742173241344
-- 2992|3|50|1526286427176288256
-- 2993|4|50|1526288308015443970
-- 2994|5|50|1526289237393522688
-- 2995|6|50|1526289408965550080
-- 2996|7|50|1526293299283230722
-- 2997|8|50|1526295915824353283
-- 2998|1|50|1526190430924156928
-- 2999|0|50|1526034149546639361
-- 4181|2|50|1526284742173241344
-- 4182|3|50|1526286427176288256
-- 4183|4|50|1526288308015443970
-- 4184|5|50|1526289237393522688
-- 4185|6|50|1526289408965550080
-- 4186|7|50|1526293299283230722
-- 4187|8|50|1526295915824353283
-- 4188|1|50|1526190430924156928
-- 4189|0|50|1526034149546639361
-- 5430|2|50|1526284742173241344
-- 5431|3|50|1526286427176288256
-- 5432|4|50|1526288308015443970
-- 5433|5|50|1526289237393522688
-- 5434|6|50|1526289408965550080
-- 5435|7|50|1526293299283230722
-- 5436|8|50|1526295915824353283
-- 5437|1|50|1526190430924156928
-- 5438|0|50|1526034149546639361


CREATE TABLE IF NOT EXISTS "tweets" (
"id" INTEGER PRIMARY KEY,
"created_at" TEXT NOT NULL,
"author_id" TEXT NOT NULL,
"tweet_text" TEXT NOT NULL,
"media_poll" TEXT,
"tweet_id" TEXT NOT NULL UNIQUE
);
CREATE TABLE IF NOT EXISTS "tweeters" (
"id" INTEGER PRIMARY KEY,
"author_id" TEXT NOT NULL UNIQUE,
"username" TEXT NOT NULL,
FOREIGN KEY("author_id") REFERENCES "tweets" ("author_id")
);
CREATE TABLE IF NOT EXISTS "assets" (
	"id"	INTEGER NOT NULL,
	"filename"	TEXT NOT NULL,
	"hash"	TEXT,
	"creator"	TEXT,
	"asset_source"	TEXT,
	"blob_data"	NUMERIC,
	"asset_type"	INTEGER NOT NULL DEFAULT 0,
	FOREIGN KEY("asset_type") REFERENCES "asset_types",
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "asset_types" (
	"type"	TEXT NOT NULL,
	"id"	INTEGER,
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "mapobjects" (
	"id"	INTEGER,
	"name"	TEXT,
	"width"	INTEGER,
	"height"	INTEGER,
	"texture"	INTEGER,
	"frame"	INTEGER,
	"flip_x"	INTEGER,
	"flip_y"	INTEGER,
	"depth"	INTEGER,
	"rotation"	INTEGER,
	"origin_x"	INTEGER,
	"origin_y"	INTEGER,
	"scrollFactor"	INTEGER,
	"visible"	INTEGER,
	"body_x"	INTEGER,
	"body_y"	INTEGER,
	"body_width"	INTEGER,
	"body_height"	INTEGER,
	"moveable"	INTEGER,
	"tint"	INTEGER,
	"tile_layer"	TEXT,
	"tile_alterations"	JSON,
	"popupText"	TEXT,
	FOREIGN KEY("texture") REFERENCES "images"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "images" (
	"id"	INTEGER NOT NULL,
	"data"	BLOB NOT NULL,
	"name"	TEXT NOT NULL UNIQUE,
	"frame_width"	INTEGER NOT NULL,
	"frame_height"	INTEGER NOT NULL,
	"margin"	INTEGER NOT NULL,
	"spacing"	INTEGER NOT NULL,
	"creator"	TEXT NOT NULL,
	"source"	TEXT NOT NULL,
	"hash"	TEXT,
	PRIMARY KEY("id")
);
CREATE INDEX "hashidimg" ON "images" (
	substr(hash, 0, 8)
);
