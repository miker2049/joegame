class_name Vehicle
extends Node

# ty coding train https://natureofcode.com/autonomous-agents

@export var actor: CharacterBody2D
@export var personal_space: Area2D


@export var max_speed: float = 3000.0
@export var turn_speed: float = 0.1
@export var arrive_radius: float = 100.0

@export var wander_radius: float = 25.0
@export var wander_dist: float = 80.0
@export var wander_change: float = 0.3;

@export var mass: float  = 10.0

# It's important
@export var desired_space: float = 128

# When do you start worrying about going over the edge
@export var boundaries_offset: float = 50.0
@export var consider_boundaries: bool = true
@export var do_seperate: bool = true



var curr_accel: Vector2 = Vector2.ZERO
var wander_theta: float = 0.0

enum VehicleMode {Wander, Seek, Arrive, Still}
var curr_target: Vector2

var curr_mode: VehicleMode = VehicleMode.Still

func apply_force(f: Vector2):
	#var ff = f.duplicate()
	curr_accel +=f/mass

func _seek(target: Vector2) -> void:
	var desired = target - actor.global_position
	desired = desired.normalized() * max_speed
	var steer = desired - actor.velocity
	apply_force(steer.limit_length(turn_speed))
	
func seek(target: Vector2):
	curr_target = target
	curr_mode = VehicleMode.Seek

func _arrive(target: Vector2) -> void:
	var desired = target - actor.global_position
	var distance = desired.length() - 10
	if distance < arrive_radius:
		var m = remap(distance,0,arrive_radius,0,max_speed)
		desired  = desired.normalized() * m
	else:
		desired  = desired.normalized() * max_speed
	var steer = desired - actor.velocity
	apply_force(steer.limit_length(turn_speed))

func arrive(target: Vector2):
	curr_target = target
	curr_mode = VehicleMode.Arrive

func _wander():
	# offset the theta one way or the other
	wander_theta += randf_range(-wander_change,wander_change)
	var circle_pos = Vector2(actor.velocity)
	# get circle out ahead. changing magnitude does this..
	circle_pos = circle_pos.normalized()
	circle_pos *= wander_dist
	# offset it necessarily to the actor
	circle_pos += actor.global_position

	var curr_angle = actor.velocity.angle()
	# gets a point along the circle.
	var coff = Vector2(wander_radius*cos(wander_theta+curr_angle),wander_radius * sin(wander_theta + curr_angle))
	var target = circle_pos + coff
	_seek(target)

func wander():
	curr_mode = VehicleMode.Wander


func boundary_check():
	var desired = null
	var s = get_viewport().get_visible_rect().size
	# check x boundary
	if actor.global_position.x < boundaries_offset:
		desired = Vector2(max_speed,actor.velocity.y)
	elif actor.global_position.x > s.x - boundaries_offset:
		desired = Vector2(-max_speed,actor.velocity.y)

	# check y
	if actor.global_position.y < boundaries_offset:
		desired = Vector2(actor.velocity.x,max_speed)
	elif actor.global_position.y > s.y - boundaries_offset:
		desired = Vector2(actor.velocity.x,-max_speed)

	if desired:
		desired = desired.normalized() * max_speed
		var steer = desired - actor.velocity
		apply_force(steer.limit_length(turn_speed))

func seperate():
	var sum = Vector2.ZERO
	var count = 0
	var overlaps = personal_space.get_overlapping_areas()
	for n in overlaps:
		
		var thispos = n.global_position
	
		var dist = actor.global_position.distance_to(n.global_position)
		#print(dist,",",desired_space)
		if dist < desired_space:
			#print("hdsajlk")
			# pointing away is subtracting like this!
			var diff = actor.global_position - n.global_position
			diff = diff.normalized() * (1/dist)
			count+=1

			sum += diff
	if count>0:
		#print(sum)
		sum = sum.normalized() * max_speed
		#sum = sum / count
		#print(sum)
		var steer = sum - actor.velocity
		
		apply_force( steer.limit_length(turn_speed))




func _physics_process(delta):
	match curr_mode:
		VehicleMode.Still:
			pass
		VehicleMode.Wander:
			_wander()
		VehicleMode.Seek:
			_seek(curr_target)
		VehicleMode.Arrive:
			_arrive(curr_target)
	if consider_boundaries:
		boundary_check()
	if do_seperate:
		seperate()
	
	# arrive(actor.get_global_mouse_position(),delta)

	actor.velocity += curr_accel
	actor.velocity = actor.velocity.limit_length(max_speed)
	actor.rotation = atan2(actor.velocity.y,actor.velocity.x)
	actor.move_and_slide()

	curr_accel *=0
