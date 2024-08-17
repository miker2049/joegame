class_name HealthBar
extends Node2D

@export var actor: Ship
@export var offset: Vector2 = Vector2(0,10)

func _ready():
	top_level = false
	

func _draw():
	draw_rect(Rect2(offset.x,offset.y,25,5),Color.RED)
	draw_rect(Rect2(offset.x,offset.y, 25.0*(float(actor.health)/float(actor.full_health)), 5),Color.GREEN)
	global_rotation = 0

func _process(delta):
	queue_redraw()
