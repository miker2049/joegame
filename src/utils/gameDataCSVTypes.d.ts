declare enum rowTypes {
    spritesheet = "spritesheet",
    image = "image",
    character = "character",
    platform = "platform",
    mapobject = "mapobject",
    convoManifest = "convoManifest",
}
type header = null | -1
type typee = rowTypes
type key = string
type url = string
type animLength = number
type frameWidth = number
type frameHeight = number
type margin = number
type spacing = number

type name = string
type texture = string
type anim = string
type speed = number
type dashDistance = number
type scale = number
type bodyOffsety = number
type bodyOffsetx = number
type width = number
type height = number
type charGroup = string
type groundTiles = string
type edgeTiles = string
type reqSpritesheet = string
type reqImage = string

export type spritesheetCSVRow = [header, typee, key, url, animLength, frameWidth, frameHeight, margin, spacing]
export type imageCSVRow = [header, typee, key, url]
export type characterCSVRow = [header, typee, name, texture, anim, anim, anim, anim, speed, dashDistance, scale, bodyOffsety, bodyOffsetx, width, height, charGroup, charGroup]

export type platformCSVRow = [header, typee, name, texture, groundTiles, edgeTiles]
export type mapobjectCSVRow = [header, typee, name, reqSpritesheet, reqSpritesheet, reqSpritesheet, reqImage, reqImage, reqImage]
export type convoManifestCSVRow = [header, typee, url]
export type gamedataCSVRow = spritesheetCSVRow | imageCSVRow | characterCSVRow | platformCSVRow | mapobjectCSVRow | convoManifestCSVRow
