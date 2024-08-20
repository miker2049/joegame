extends Camera2D

var curr_center: Vector2

func _process(delta):
	if Input.is_action_just_released("click"):
		curr_center = get_global_mouse_position()
	var pos = get_screen_center_position()
	if pos.distance_to(curr_center)>20:
		global_position = pos.move_toward(curr_center,delta*300)
