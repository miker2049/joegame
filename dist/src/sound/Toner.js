import * as Tone from 'tone';
import Gong from "./synths/Gong";
import SynthBeep from "./synths/SynthBeep";
import { Talking } from "./synths/Talking";
import Walk from "./synths/Walk";
export default class Toner {
    constructor(context) {
        this.context = context;
        Tone.setContext(this.context);
        this.instruments = new Map();
        this.instruments.set('arp', new SynthBeep());
        this.instruments.set('gong', new Gong());
        this.instruments.set('walk', new Walk());
        Tone.setContext(this.context);
        this.instruments.set('talking', new Talking());
        Tone.setContext(this.context);
        Tone.start();
        Tone.Transport.start();
    }
    play(config) {
        const found = this.instruments.get(config.inst);
        if (found && this.context.state === 'running')
            found.play(config);
    }
}
//# sourceMappingURL=Toner.js.map