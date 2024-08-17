extends Node2D

@onready var ship_world = $ShipWorld as ShipWorld
@onready var word_ui = $WordUI as WordUI

func _ready():
	word_ui.entered_word.connect(func (w): ship_world.add_ship(w))
