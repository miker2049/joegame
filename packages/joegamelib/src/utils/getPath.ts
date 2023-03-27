import { IPathfinder } from '../ILevel'
import createPathTransitions from './createPathTransitions'
export interface getPathParams {
  x: number
  y: number
  dx: number
  dy: number
  tempObsX?: number
  tempObsY?: number
  finder: IPathfinder
}
export default function (
  params: getPathParams
): Promise<Phaser.Types.Math.Vector2Like[]> {
  return new Promise((res, rej) => {
    if (params.tempObsX != undefined && params.tempObsY != undefined) {
      params.finder.avoidAdditionalPoint(params.tempObsX, params.tempObsY)
    }
    // console.log(params.x,params.y,params.dx,params.dy)

    params.finder.findPath(params.x, params.y, params.dx, params.dy, (path) => {
      if (path) {
        res(createPathTransitions(path))
      } else {
        rej()
      }
    })
    params.finder.calculate()
    if (params.tempObsX != undefined && params.tempObsY != undefined) {
      params.finder.stopAvoidingAdditionalPoint(
        params.tempObsX,
        params.tempObsY
      )
    }
  })
}
