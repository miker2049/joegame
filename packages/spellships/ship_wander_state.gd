class_name ShipWanderState
extends State
@export var actor: Ship
@export var purview: Area2D
@export var vehicle: Vehicle

var curr_destination: Vector2

signal see_enemy(Ship)



func _ready() -> void:
	set_physics_process(false)

# Called when the node enters the scene tree for the first time.
func _enter_state():
	set_physics_process(true)
	print("wander")
	vehicle.wander()

func _sort_by_distance(a: PhysicsBody2D, b: PhysicsBody2D)->bool:
	var aD = actor.global_position.distance_squared_to(a.global_position)
	var bD = actor.global_position.distance_squared_to(b.global_position)
	return true if aD < bD else false

func _is_different_team(s: PhysicsBody2D)->bool:
	return true if "team" in s and s.team != actor.team else false
		
		 
func _check_purview():
	var ob = purview.get_overlapping_bodies().filter(_is_different_team)
	if ob.size() > 0:
		ob.sort_custom(_sort_by_distance)
		see_enemy.emit(ob[0])

func _physics_process(delta):
	_check_purview()

func _exit_state():
	set_physics_process(false)
