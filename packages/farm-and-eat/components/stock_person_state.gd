class_name StockPersonState
extends PersonState

var currd = Vector2.ZERO
signal reached_target



func _enter_state(p: Person):
	assert(p.inventory.size()>0, "Can't stock foor you don't have.")
	print("stock")
	var cpos = p.mover.get_tile_pos()
	var path = p.mappath.get_pathfind(cpos,p.pantry.get_child(0).global_position/p.mover.tilesize)
	p.mover.set_path(path)
	p.mover.reached_target.connect(_emit_reached)

func _emit_reached():
	print("stock emit reached")
	reached_target.emit()

func _exit_state(p: Person):
	for it in p.inventory:
		Inventory.stock.push_back(it)
	p.inventory = []
	if p.mover and p.mover.reached_target.is_connected(_emit_reached):
		p.mover.reached_target.disconnect(_emit_reached)


