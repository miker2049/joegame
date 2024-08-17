class_name ShipBumpedState
extends State
@export var actor: Ship
@export var cannon: Cannon
@export var timer: Timer
signal done


@onready var vehicle_mover = $"../../VehicleMover"

var collision

# Called when the node enters the scene tree for the first time.
func _enter_state():
	timer.timeout.connect(cannon.fire)
	timer.start()
	set_physics_process(true)
	collision = actor.get_last_slide_collision()
	vehicle_mover.max_speed = 0
	var this_collider=collision.get_collider()
	if "team" in this_collider:
		if this_collider.team != actor.team:
			actor.curr_target = this_collider
	await get_tree().create_timer(2).timeout
	#print("done bumped")
	done.emit()

func _physics_process(delta):
	if collision:
		actor.velocity = 0.1 * actor.velocity.bounce(collision.get_normal()) * 2
	actor.move_and_slide()

func _exit_state():
	if timer.timeout.is_connected(cannon.fire):
		timer.timeout.disconnect(cannon.fire)
	timer.stop()
	set_physics_process(false)
	pass

