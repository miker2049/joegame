import { ILevelComponents } from '../ILevel'
import { createNPCMachine } from './NPCMachine'
import { ICharacter } from '../ICharacter'
import randomInterestSet from '../utils/randomInterestSet'
import { interpret } from 'xstate'
import defaults from '../defaults'
interface tweet {
    text: string
    username: string
}
const urlRegex = /((([A-Za-z]{3,9}:(?:\/\/)?)(?:[\-;:&=\+\$,\w]+@)?[A-Za-z0-9\.\-]+|(?:www\.|[\-;:&=\+\$,\w]+@)[A-Za-z0-9\.\-]+)((?:\/[\+~%\/\.\w\-_]*)?\??(?:[\-\+=&;%@\.\w_]*)#?(?:[\.\!\/\\\w]*))?)/g
export default class TweetConvo {
    chars: ICharacter[]
    convo: tweet[]
    users: string[]
    running: boolean
    // box:

    constructor(chars: ICharacter[], convo: tweet[], users: string[], level: ILevelComponents) {
        this.chars = chars
        this.convo = convo
        this.convo.reverse()
        this.users = users
        this.running = false
    }

    async runConvo(): Promise<void> {
        if (!this.running) {
            this.running = true
            for (let conv of this.convo) {
                const charIndex = this.users.findIndex((val) => {
                    return val === conv.username
                }) % this.chars.length
                let text = conv.text.replace(/@[^\s]*/g, '')
                    .replace(urlRegex, '')
                    .replace(/\s\s+/g, '')
                    .replace(/[\.\?\!\;\:]/g, '$&  ')
                this.chars[charIndex].jumpUp()
                this.chars.forEach(char => char.setDepth(defaults.charDepth))
                this.chars[charIndex].setDepth(10)
                await this.chars[charIndex].speak(text, defaults.talkingSpeed)
                this.chars[charIndex].voxbox.close()
            }
            this.running = false
            setTimeout(() => this.runConvo(), 5000)

        }
    }
}
