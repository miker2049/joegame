class_name ShipWorld
extends Node2D

var n = 10


func get_random_ship_world_point()->Vector2:
	var s = get_viewport().get_visible_rect().size
	s.y -= 275
	return s * Vector2(randf(),randf())


func _ready():
	_init_ships()

func _init_ships():
	for d in n:
		var s = preload("res://ship.tscn")
		var bship = s.instantiate()
		bship.conf = preload("res://small_ship.tres")
		bship.team = Ship.Team.RED_TEAM
		bship.position =get_random_ship_world_point()
		add_child(bship)

func add_ship(word:String):
	var s = preload("res://ship.tscn")
	var ship = s.instantiate()
	ship.conf = preload("res://small_ship_blue.tres")
	ship.team = Ship.Team.BLUE_TEAM
	ship.position = get_random_ship_world_point()
	ship.word = word
	add_child(ship)

