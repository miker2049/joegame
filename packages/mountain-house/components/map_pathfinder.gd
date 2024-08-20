class_name MapPath
extends Node

@export var tmap: TileMap

var map_size: Vector2i
var astar: AStar2D


func _ready():
	astar = AStar2D.new()
	map_size = tmap.get_used_rect().size
	_init_astar()


func _id_to_pos(id: int)-> Vector2i:
	var y = id / map_size.x
	var x = id % map_size.x
	return Vector2i(x,y)
	
func _pos_to_id(pos: Vector2i)-> int:
	return (pos.y*map_size.x)+pos.x


			
func _neighbor_arr(pos: Vector2i)->Array[Vector2i]:
	var i = pos.x
	var j = pos.y
	return [Vector2i(i,j-1),Vector2i(i,j+1),Vector2i(i-1,j),Vector2i(i+1,j)]
# iterate on a tile through layers
func _get_tile_weight(pos:Vector2i) -> float:
	var weight = 0.0
	for l in tmap.get_layers_count():
		var atlas_coords = tmap.get_cell_atlas_coords(l,pos)
		if atlas_coords:
			var d = tmap.get_cell_tile_data(l,pos)
			if d:
				var tw = d.get_custom_data("travel_cost")
				if tw:
					weight += tw
	return weight

func _set_weights():
	for y in map_size.y:
		for x in map_size.x:
			var pos = Vector2i(x,y)
			var weight = _get_tile_weight(pos)
			var id = _pos_to_id(pos)
			astar.add_point(id,pos,weight)


func _connect_points():
	for y in map_size.y:
		for x in map_size.x:
			var pos = Vector2i(x,y)
			var id = _pos_to_id(pos)
			for neighbor in _neighbor_arr(pos):
				if Rect2i(0,0,map_size.x,map_size.y).has_point(neighbor):
					var nid = _pos_to_id(neighbor)
					astar.connect_points(id,nid,true)

func _init_astar():
	astar.reserve_space(map_size.x*map_size.y)
	_set_weights()
	_connect_points()

func get_pathfind(s: Vector2i, d: Vector2i)->PackedVector2Array:
	var p = astar.get_point_path(_pos_to_id(s),_pos_to_id(d))
	return p

