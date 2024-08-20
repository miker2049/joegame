class_name TileMover
extends Vehicle


@export var tilesize: int

@export var speed: int = 150

var curr_target_queue: PackedVector2Array
var curr_pos = Vector2.ZERO
var at_position: bool = false
var paused: bool = false

var follow_radius: float

signal reached_target

func _ready():
	follow_radius = 4.0
	do_rotate = false
	curr_mode = VehicleMode.None

func _move_towards_point(point:Vector2,delta):
	curr_pos = actor.global_position.move_toward(point,delta)
	actor.velocity = (curr_pos-actor.global_position)/(delta)
	actor.velocity = actor.velocity.normalized() * speed

func stop()->void:
	at_position = true 
	curr_target_queue.clear()

func pause():
	paused = true

func unpause():
	paused = false

func get_tile_pos()->Vector2i:
	var x = floor(actor.global_position.x/CC.TILE_SIZE)
	var y = floor(actor.global_position.y/CC.TILE_SIZE)
	return Vector2i(x,y)


func _check_arrival():
	var curr_target = curr_target_queue[curr_target_queue.size()-1]
	if actor.global_position.x > curr_target.x * tilesize:
		if actor.global_position.x <= (curr_target.x+1)*tilesize:
			if actor.global_position.y > curr_target.y * tilesize:
				if actor.global_position.y <= (curr_target.y+1)*tilesize:
					at_position = true
					curr_mode = VehicleMode.Still
					reached_target.emit()


func _get_closest_path_point():
	var record: float = 99999999
	var record_idx = -1
	if curr_target_queue:
		for point_idx in curr_target_queue.size():
			var point = curr_target_queue[point_idx]
			var real_point = point * tilesize
			var dist = real_point.distance_squared_to(actor.global_position) 
			if dist < record:
				record = dist
				record_idx = point_idx
		return record_idx
	else:
		return -1

func _follow_tile_path(a_idx = _get_closest_path_point()):
	var half_tile = Vector2(tilesize/2,tilesize/2)
	if a_idx < 0:
		return
	if a_idx == curr_target_queue.size() -1:
		_seek(curr_target_queue[a_idx] * tilesize + Vector2(tilesize,tilesize))
	else:
		var a = curr_target_queue[a_idx] * tilesize
		var b = curr_target_queue[a_idx+1] * tilesize
		var actor_future = (actor.velocity.normalized() * tilesize/2.0) + actor.global_position
		var normal_point = Lib.get_normal_point(actor_future,a,b)
		var distance  = actor_future.distance_to(normal_point)
		if distance > follow_radius:
			var path_line = b-a
			path_line = path_line.normalized() * tilesize
			var target = normal_point + path_line
			_seek(target)
		else:
			_follow_tile_path(a_idx+1)
		
	

func set_path(p: PackedVector2Array):
	curr_target_queue = p
	at_position = false
	curr_mode = VehicleMode.None

func _physics_process(delta):
	if not at_position and curr_target_queue.size()>0 and not paused:
		_follow_tile_path()
		_check_arrival()
	_vehicle_physics_process(delta)

