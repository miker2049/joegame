class_name ShipConfig
extends Resource

func _init():
	resource_local_to_scene = true

@export var health: float = 100.0
@export var attack: float = 10.0
@export var ship_width: float = 32.0
@export var ship_height: float = 64.0
@export var texture: Texture = preload("res://assets/ship.png")

@export var vehicle_max_speed: float = 30
@export var vehicle_accel: float = 0.21
@export var vehicle_deccel: float = 0.21
@export var vehicle_turn_speed: float = 0.01

@export var purview_radius: float = 256.0
@export var attack_radius: float = 128.0

@export var shoot_speed: float = 1.0
