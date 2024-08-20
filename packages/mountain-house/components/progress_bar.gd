class_name MtnProgressBar
extends Control

@export var bar_texture: Texture2D
@export var bg_texture: Texture2D
@export var bar_size: float = 50.0

var value = 1.0
var main_bar: NinePatchRect
var bg_bar: NinePatchRect
func _ready():
	main_bar = NinePatchRect.new()
	bg_bar = NinePatchRect.new()
	main_bar.texture = bar_texture
	bg_bar.texture = bg_texture
	_init_bar(main_bar)
	_init_bar(bg_bar)
	add_child(main_bar)
	add_child(bg_bar)
	set_value(0.5)
	rotation = PI

func set_value(n:float):
	value = clampf(n,0.0,1.0)
	main_bar.size.y = remap(value,0.0,1.0,0.0,bar_size)
	main_bar.position.y = -bar_size/2

func _init_bar(b: NinePatchRect):
	b.patch_margin_bottom = 2.5
	b.patch_margin_top = 2.5
	b.patch_margin_left = bar_texture.get_size().x / 2
	b.patch_margin_right = bar_texture.get_size().x / 2
	b.size.y = bar_size
	b.position.y = -bar_size/2
