extends Area2D
@onready var collider = $CollisionShape2D
@onready var spr = $Sprite2D
@onready var notifier = $ScreenNotify

var speed = 100
var accel = 0.001
var thickness = 40
# Called when the node enters the scene tree for the first time.
func _ready():
	collider.shape = RectangleShape2D.new()
	collider.shape.size = Vector2(thickness,randi_range(20,280))
	notifier.rect.size.x = 40
	_sync_shader_scale()

func _sync_shader_scale():
	spr.material.set_shader_parameter("pos", collider.position-collider.shape.size/2)
	spr.material.set_shader_parameter("rectSize", collider.shape.size)
	spr.material.set_shader_parameter("tilesize", 64)

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	position.x -= speed*delta
	speed += accel
	pass


func _on_visible_on_screen_notifier_2d_screen_exited():
	position.x = get_viewport().get_visible_rect().size.x + thickness*2
	collider.shape.size = Vector2(thickness,randi_range(20,280))
	_sync_shader_scale()
	



func _on_area_entered(area):
	pass # Replace with function body.


func _on_body_entered(body):
	if "isTurd" in body:
		get_tree().get_root().get_node("World/ui/deadlabel").visible = true
		await get_tree().create_timer(0.8).timeout
		get_tree().get_root().get_node("World/ui/deadlabel").visible = false

