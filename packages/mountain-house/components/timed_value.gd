class_name TimedValue
extends Timer

@export var value: float = 100.0
@export var delta: float = -1.0

func _process_timer():
	value += delta

func _ready():
	timeout.connect(_process_timer)
	
