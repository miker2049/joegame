extends TileMap

@onready var person = $Person as Person
@onready var map_path = $MapPath as MapPath
@onready var rest_area = $RestArea
@onready var pantry = $Pantry

const SOIL_SOURCE = 3
const SOIL_COORD = Vector2i(19,9)

var plant_scene = preload("res://plant.tscn")
var person_scene = preload("res://person.tscn")
func _ready():
	var size = get_used_rect().size
	for pp in 10:
		var ps = person_scene.instantiate()
		ps.global_position = Vector2(randf()*200,randf()*200)
		ps.pantry = pantry
		ps.idle_area = rest_area
		ps.mappath = map_path
		add_child(ps)
	for li in get_layers_count():
		for y in size.y:
			for x in size.x:
				var pos = Vector2(x,y)
				var cell_source = get_cell_source_id(li,pos)
				var cell_atlas_coor = get_cell_atlas_coords(li,pos)
				if cell_source == SOIL_SOURCE and cell_atlas_coor == SOIL_COORD:
					var ps: Plant = plant_scene.instantiate()
					ps.global_position = pos * tile_set.tile_size.x
					add_child(ps)
					
				
				
		
	
	#var ps = preload("res://person.tscn")
	#for pp in 20:
		#var sc: Person = ps.instantiate()
		#sc.global_position = Vector2(randf()*800,randf()*800)
		#add_child(sc)
		#person.mover.wander()
		#
	#var cpos = person.mover.get_tile_pos()
	#var p = map_path.get_pathfind(cpos,Vector2i(10,10))
	#person.mover.set_path(p)
