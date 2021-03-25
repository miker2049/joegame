import { ILevelComponents } from '../ILevel';
import { ICharacter } from '../ICharacter';
interface tweet {
    text: string;
    username: string;
}
export default class TweetConvo {
    chars: ICharacter[];
    convo: tweet[];
    users: string[];
    constructor(chars: ICharacter[], convo: tweet[], users: string[], level: ILevelComponents);
    runConvo(): Promise<void>;
}
export {};
