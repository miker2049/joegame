class_name IdlePersonState
extends PersonState

var currd = Vector2.ZERO
signal reached_target

func _enter_state(p: Person):
	_init_state.call_deferred(p)

func _init_state(p: Person):
	p.mover.curr_mode = Vehicle.VehicleMode.Still
	await get_tree().create_timer(randf()*3).timeout
	p.mover.curr_mode = Vehicle.VehicleMode.None
	
	p.wprogress.visible =false
	print("idle")
	var idle_shape: CollisionShape2D = p.idle_area.get_child(0)
	var idle_rect = idle_shape.shape.get_rect()
	var ts = p.mover.tilesize
	var idle_x = randi_range(idle_shape.global_position.x + idle_rect.position.x, idle_rect.size.x + idle_shape.global_position.x + idle_rect.position.x)/ts
	var idle_y = randi_range(idle_shape.global_position.y + idle_rect.position.y, idle_rect.size.y + idle_shape.global_position.y + idle_rect.position.y)/ts
	var curr_target = Vector2i(floor(idle_x),floor(idle_y))
	var cpos = p.mover.get_tile_pos()
	var path = p.mappath.get_pathfind(cpos,curr_target)
	p.mover.set_path(path)
	p.mover.reached_target.connect(_emit_reached)
	

	
func _emit_reached():
	print("reach emit")
	reached_target.emit()

func _exit_state(p: Person):
	if p.mover and p.mover.reached_target.is_connected(_emit_reached):
		p.mover.reached_target.disconnect(_emit_reached)


