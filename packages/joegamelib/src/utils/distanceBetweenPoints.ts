import 'phaser'
import {AssuredVec2} from '../joegameTypes'
/**
 * For now we will just have an alias here, but put in place for future
 * framework agnosticism.
 */
const distanceBetweenPoints = (a: AssuredVec2, b: AssuredVec2) =>
    //our own implementation would go here
    Phaser.Math.Distance.BetweenPoints(a,b)

export default distanceBetweenPoints
