class_name ShipAttackingState
extends State

@export var actor: Ship
@export var attack_zone: Area2D

signal reached_target(enemy:Ship)
@onready var vehicle_mover = $"../../VehicleMover" as VehicleMover

func _ready():
	set_physics_process(false)
	
# Called when the node enters the scene tree for the first time.
func _enter_state():
	print("heyy attack")
	vehicle_mover.max_speed = 5

	
func _exit_state():
	set_physics_process(false)

func _physics_process(delta):
	if actor.curr_target:
		vehicle_mover.move_toward(actor.curr_target.position)

	

