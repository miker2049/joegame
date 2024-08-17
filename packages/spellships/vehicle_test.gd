extends Node2D

@onready var vehicle = $VehicleTest/Vehicle

var destination: Vector2 = Vector2(200,200)

func _ready():
	vehicle.wander()

func _draw():
	var v = get_viewport_rect()
	draw_circle(destination,50,Color.BLUE_VIOLET)
	draw_rect(Rect2(100,100,v.size.x-200,v.size.y-200),Color.BLACK,false,1.0)
