class_name Ship
extends CharacterBody2D
@onready var fsm = $FSM as FSM
@onready var ship_wander_state = $FSM/ShipWanderState as ShipWanderState
@onready var ship_pursue_state = $FSM/ShipPursueState as ShipPursueState
@onready var ship_attacking_state = $FSM/ShipAttackingState as ShipAttackingState

enum Team {RED_TEAM, BLUE_TEAM}

@export var team: Team = Team.RED_TEAM

var curr_target: Ship

func _ready() -> void:
	ship_wander_state.see_enemy.connect(_set_new_target)
	ship_pursue_state.reached_target.connect(fsm.change_state.bind(ship_attacking_state))
	#ship_pursue_state.reached_target.connect(_goo)
	
func _set_new_target(a: Ship):
	curr_target = a
	fsm.change_state(ship_pursue_state)
	
