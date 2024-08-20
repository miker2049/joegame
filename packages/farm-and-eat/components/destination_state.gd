class_name DestinationState
extends PersonState

var curr_target: Vector2

func __enter_state(p: Person):
	var cpos = p.mover.get_tile_pos()
	var path = p.mappath.get_pathfind(cpos,curr_target)
	p.mover.set_path(path)
	p.mover.reached_target.connect(_on_enter_destination)

func _on_enter_destination():
	pass

func __exit_state(p: Person):
	if p.mover and p.mover.reached_target.is_connected(_on_enter_destination):
		p.mover.reached_target.disconnect(_on_enter_destination)

