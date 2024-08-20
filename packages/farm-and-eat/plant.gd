class_name Plant
extends Node2D
@onready var age = $Age as TimedValue
@onready var spr = $Sprite2D as Sprite2D


@export var plant_name = "carrots"
@export var unplanted_texture: Texture2D
@export var growing_texture_1: Texture2D
@export var growing_texture_2: Texture2D
@export var ripe_texture: Texture2D
@export var grow_speed = 20.0

@export var tilesize = 32

var planted = true
var grown = false

func _ready():
	spr.material.set_shader_parameter("hcolor", Color.TRANSPARENT)
	age.timeout.connect(_tick)
	

func harvest():
	planted = false 
	grown = false
	age.value = 0
	spr.texture = unplanted_texture
	spr.material.set_shader_parameter("hcolor", Color.TRANSPARENT)
	age.stop()
	

func plant():
	planted = true
	age.start()
	

func _tick():
	if grown:
		pass
	elif not planted:
		spr.texture = unplanted_texture
	elif age.value < grow_speed:
		spr.texture = growing_texture_1
	elif age.value < grow_speed*2:
		spr.texture = growing_texture_2
	elif age.value > grow_speed * 3:
		spr.texture = ripe_texture
		grown = true
		Jobq.push_job(self)
		spr.material.set_shader_parameter("hcolor", Color.CORNSILK)
