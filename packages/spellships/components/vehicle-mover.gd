class_name VehicleMover
extends Node

@export var actor: CharacterBody2D
@export var personal_space: Area2D

@export var max_speed: float = 30
@export var accel: float = 0.21
@export var deccel: float = accel
@export var turn_speed: float = 0.01

@export var running: bool = true
@export var debug_pointer: RayCast2D
@export var ease_radius: float = 16.0
@export var avoid: bool = true

var cdest: Vector2
var curr_speed: float = 0

func move_toward(pos: Vector2)->void:
	cdest = pos

func _physics_process(delta):
	debug_pointer.target_position=actor.to_local(cdest)
	var new_rot: float = lerp_angle(actor.rotation,actor.position.angle_to_point(cdest),turn_speed)
	actor.rotation = new_rot 
	var rot_change = abs(new_rot - actor.rotation)
	var dist_target = actor.position.distance_to(cdest)
	var vel_delta = accel
	if(curr_speed>max_speed):
		curr_speed = max(0.0,curr_speed-deccel)
	else:
		curr_speed = min(max_speed,curr_speed+accel)
	curr_speed -= rot_change * 10
	curr_speed = Lib.ease_in_speed(actor.position,cdest,curr_speed,ease_radius)
	actor.velocity = Vector2.from_angle(actor.rotation).normalized()*curr_speed
	if avoid:
		var sum = Vector2.ZERO
		for obs in personal_space.get_overlapping_areas():
			var dist = actor.position.direction_to(obs.position)
			var diff = actor.position - obs.position
			sum += diff
		sum=sum.normalized()*max_speed
		actor.velocity += sum - actor.velocity
		#print(sum)
	#print(actor.veloci)

		
	actor.move_and_slide()
