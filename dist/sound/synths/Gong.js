import { Player, loaded } from 'tone';
export default class {
    constructor() {
        this.id = 'Gong';
        this.volume = 0.75;
        this.synth = new Player("https://tonejs.github.io/audio/berklee/gong_1.mp3").toDestination();
    }
    play() {
        loaded().then(() => {
            console.log(this.synth);
            this.synth.start(0);
        });
    }
    setVolume() { }
}
//# sourceMappingURL=Gong.js.map