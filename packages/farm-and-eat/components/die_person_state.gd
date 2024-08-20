class_name DiePersonState
extends PersonState

func _enter_state(p: Person):
	p.mover_sprite.enabled = false
	p.mover.curr_mode = Vehicle.VehicleMode.Still
	p.spr.play.call_deferred("die")

	
func _exit_state(p: Person):
	pass


