class_name ShipWanderState
extends State
@export var actor: Ship
@export var purview: Area2D

var curr_destination: Vector2

signal see_enemy(enemy:Ship)
@onready var vehicle_mover = $"../../VehicleMover"

func _ready():
	set_physics_process(false)
	
# Called when the node enters the scene tree for the first time.
func _enter_state():
	set_physics_process(true)
	_set_rand_dest()
	purview.area_entered.connect(
		func(a:Area2D):
			var thisship = a.get_parent()
			if "team" in thisship:
				if thisship.team != actor.team:
					see_enemy.emit(thisship))

func _set_rand_dest():
	var s = get_viewport().get_visible_rect().size
	curr_destination = s * Vector2(randf(),randf())
	vehicle_mover.move_toward(curr_destination)

func _exit_state():
	set_physics_process(false)

func _physics_process(delta):
	if actor.position.distance_to(curr_destination) < 32:
		_set_rand_dest()
	
