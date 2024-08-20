class_name TileMoverSprite
extends Node
@export var spr: AnimatedSprite2D
@export var actor: CharacterBody2D

@export var enabled: bool = true

func _update_animation():
	if abs(actor.velocity.x)>abs(actor.velocity.y):
		if actor.velocity.x < 0:
			spr.play("left")
		else:
			spr.play("right")
	else:
		if actor.velocity.y < 0:
			spr.play("up")
		else:
			spr.play("down")
	if actor.velocity.is_zero_approx():
		spr.stop()


func _physics_process(delta):
	if enabled:
		_update_animation()
