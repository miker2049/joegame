extends Node

const PLANT_JOB = 0
const HARVEST_JOB = 1

signal new_job

var jobs: Array[Plant] = []

func push_job(job: Plant):
	jobs.push_back(job)
	new_job.emit()
