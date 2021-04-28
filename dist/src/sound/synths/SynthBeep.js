export default class {
    // seq: Tone.Sequence
    constructor() {
        this.id = 'Arp';
        // synth: Tone.PolySynth
        this.volume = 0.75;
        // this.synth = new Tone.PolySynth().toDestination();
        // this.seq = new Tone.Sequence((time, note) => {
        //     this.synth.triggerAttackRelease(note, 0.1, time);
        //     // subdivisions are given as subarrays
        // }, ["C4", ["E4", "D4", "E4"], "G4", ["A4", "G4"]])
        // Tone.Transport.start()
    }
    play() {
        // console.log(this.synth)
        // console.log(Tone.Transport.state)
        // this.seq.start(0.05)
        // let now = Tone.now()
        // this.synth.triggerAttackRelease("C4", "16n")
        // this.synth.triggerAttackRelease("E4", "16n", now + 0.5)
        // this.synth.triggerAttackRelease("G4", "16n", now + 1)
        // this.synth.triggerAttackRelease("B4", "16n", now + 1.5)
    }
    setVolume() { }
}
//# sourceMappingURL=SynthBeep.js.map