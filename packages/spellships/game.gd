extends Node


var main_menu = preload("res://main_menu.tscn")
var main_game = preload("res://world.tscn")

var curr_scene: Node


func _start_pscene(pscene: PackedScene)->Node:
	var s = pscene.instantiate()
	get_tree().root.add_child.call_deferred(s)
	curr_scene = s
	return s
	
func _start_game():
	#_start_pscene(main_game)
	curr_scene.queue_free()
	get_tree().change_scene_to_packed(main_game)
	
	
func _ready():
	var ss = _start_pscene(main_menu)
	ss.pressed_start.connect(_start_game)
