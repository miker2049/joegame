class_name PersonMachine
extends Node


@export var state: PersonState
@export var person: Person



# Called when the node enters the scene tree for the first time.
func _ready():
	change_state.call_deferred(state)
	


func change_state(new_state: PersonState):
	if state is PersonState:
		state._exit_state(person)
	new_state._enter_state(person)
	state = new_state



# states


		
		

