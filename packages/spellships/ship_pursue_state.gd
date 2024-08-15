class_name ShipPursueState
extends State

@export var actor: Ship
@export var attack_zone: Area2D

signal reached_target
@onready var vehicle_mover = $"../../VehicleMover"

func _ready():
	set_physics_process(false)
	
# Called when the node enters the scene tree for the first time.
func _enter_state():
	set_physics_process(true)
	attack_zone.area_entered.connect(_on_target_reach)

func _on_target_reach(a:Area2D):
	if a.get_parent().name == actor.curr_target.name:
		reached_target.emit()
	
func _exit_state():
	set_physics_process(false)

func _physics_process(delta):
	if actor.curr_target:
		vehicle_mover.move_toward(actor.curr_target.position)

	
