class_name VehicleMover
extends Node

@export var actor: CharacterBody2D

@export var max_speed: float = 30
@export var accel: float = 0.051
@export var turn_speed: float = 0.01

@export var running: bool = true
@export var debug_pointer: RayCast2D

var cdest: Vector2
var curr_speed: float = 0

func move_toward(pos: Vector2)->void:
	cdest = pos


func _physics_process(delta):
	debug_pointer.target_position=actor.to_local(cdest)
	get_tree().debug_collisions_hint
	var new_rot: float = lerp_angle(actor.rotation,actor.position.angle_to_point(cdest),turn_speed)
	var rot_change = abs(new_rot - actor.rotation)
	var dist_target = actor.position.distance_to(cdest)
	
	actor.rotation = new_rot
	curr_speed = max(max_speed, curr_speed+accel)
	curr_speed -= rot_change * 10
	curr_speed = Lib.ease_in_speed(actor.position,cdest,curr_speed,64)
	actor.velocity = Vector2.from_angle(actor.rotation).normalized()*curr_speed
	actor.move_and_slide()
