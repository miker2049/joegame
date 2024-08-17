class_name ShipAttackingState
extends State

@export var actor: Ship
@export var cannon: Cannon
@export var shoot_timer: Timer
@export var attack_zone: Area2D
@export var vehicle: Vehicle


signal lost_target

func  _ready():
	print("attack")
	set_physics_process(false)

# Called when the node enters the scene tree for the first time.
func _enter_state():
	set_physics_process(true)
	shoot_timer.start()
	vehicle.curr_mode = Vehicle.VehicleMode.Arrive

func _exit_state():
	set_physics_process(false)
	shoot_timer.stop()


func _physics_process(delta):
	if actor.curr_target and is_instance_valid(actor.curr_target):
		vehicle.curr_target = actor.curr_target.global_position



