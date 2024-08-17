class_name DamageTarget
extends Node

@export var actor_area: Area2D
@export var team: Ship.Team = Ship.Team.RED_TEAM

signal hit_enemy(Ship)

func _ready() -> void:
	actor_area.area_entered.connect(_area_entered)

func _area_entered(a: CharacterBody2D) -> void:
	var thisship = a
	if "team" in thisship:
		if thisship.team != team:
			hit_enemy.emit(thisship)
