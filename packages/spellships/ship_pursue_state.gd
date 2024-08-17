class_name ShipPursueState
extends State

@export var actor: Ship
@export var attack_zone: Area2D
@export var vehicle: Vehicle

signal reached_target
signal lost_target

func _ready():
	set_physics_process(false)

# Called when the node enters the scene tree for the first time.
func _enter_state():
	print("pursue")
	set_physics_process(true)
	vehicle.seek(actor.curr_target.global_position)


func _physics_process(delta):
	vehicle.curr_target = actor.curr_target.global_position
	for bod  in attack_zone.get_overlapping_bodies():
		if bod.name == actor.curr_target.name:
			reached_target.emit()



func _on_target_reach(a:CharacterBody2D):
	if "team" in a:
		if is_instance_valid(a):
			reached_target.emit()

func _exit_state():
	set_physics_process(false)
