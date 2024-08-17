class_name MainMenuScene
extends Control
@onready var button = $Button

signal pressed_start

func _emit_start():
	pressed_start.emit()

func _ready():
	button.pressed.connect(_emit_start)
