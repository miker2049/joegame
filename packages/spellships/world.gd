extends Node2D

var n = 70


func _ready():
	
	_init_ships()

func _init_ships():
	for d in n:
		var s = preload("res://ship.tscn")
		var ship = s.instantiate()
		ship.conf = preload("res://small_ship.tres")
		ship.team = Ship.Team.RED_TEAM
		ship.position = Lib.get_random_viewport_point()
		add_child(ship)
		var bship = s.instantiate()
		bship.conf = preload("res://small_ship_blue.tres")
		bship.team = Ship.Team.BLUE_TEAM
		bship.position = Lib.get_random_viewport_point()
		add_child(bship)

func _process(delta):
	if Input.is_mouse_button_pressed(1):
		var s = preload("res://ship.tscn")
		var ship = s.instantiate()
		ship.conf = preload("res://small_ship.tres")
		ship.team = Ship.Team.RED_TEAM
		ship.position = Lib.get_random_viewport_point()
		add_child(ship)
