class_name ShipDeadState
extends State
@export var actor: Ship

# Called when the node enters the scene tree for the first time.
func _enter_state():
	actor.queue_free()
