MIDIClient.init;
MIDIClient.disposeClient;

~fluid = MIDIOut(0)
16.do{arg i; ~fluid.allNotesOff(i)}

Pdef.removeAll;

(
var tempomod= 1;
var defaultPlayerBind = Pbind(\type,\midi,\midiout,~fluid);
var    patternChoices = (
    three: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.55.rrand(0.8),
            \degree, 3,
            \dur, Prand([0.125],inf) *tempomod
        )
    },
    one: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.5.rrand(0.7),
            \degree, 0,
            \dur, Prand([0.25],inf) * tempomod
        )
    },
    onethreelong: {
        Pbindf(
            defaultPlayerBind,
            \amp, Pseq([0.6,0.5],inf),
            \note, Pseq([7,0],inf),
            \dur ,   Pseq([0.5,4.1],inf),
            \sustain, Pseq([4.5,4],inf),
        )
    },
    seven: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.5.rrand(0.7),
            \degree, 5,
            \dur, Prand([0.5],inf) * tempomod
        )
    },
    trill: {
        Pbindf(
            defaultPlayerBind,
            \amp, Pseries(0.8,-0.03333,inf),
            \degree, Pseq([2,1],inf),
            \dur, Prand([1/16],inf) * tempomod
        )
    },
    chord: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.4.rrand(0.7),
            \degree, Prand([[0,3,5],[3,5,8]],inf),
            \dur, 4
        )
    },
    walk: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.4.rrand(0.7),
            \degree, Pwalk([7,5,4,14],
                Prand([-1,1], inf),
                Pseq([1, -1], inf)),
            \dur, Prand([0.25],inf) *tempomod
        )
    }
);

Tempo.bpm = 92;
~fluid.control(0,0,0);
~fluid.control(0,32,0);

~fluid.program(0,53);
~fluid.program(1,52);
~fluid.program(2,85);
~fluid.program(3,22);
~fluid.program(4,17);
~fluid.program(5,80);
~fluid.program(6,26);

Pdef(\mst,Pspawn(
    Pbind(
        \root, Pseq([
            0,
            0,
            3,
            0,
            0,
            3,
            0,
            0,
            3,
            5,
            -2,
            5,
            7,
            7,
        ], inf),
        \scale, Pseq([
            Scale.minor,
            Scale.minor,
            Scale.majorPentatonic,
            Scale.minor,
            Scale.minor,
            Scale.majorPentatonic,
            Scale.minor,
            Scale.minor,
            Scale.majorPentatonic,
            Scale.minor,
            Scale.minor,
            Scale.minor, //
            Scale.major,
            Scale.major,
        ], inf).trace,
        \numplayers, Pseq([1,1,1,1,1,1,1,1,1,Pstutter(3,Pseries(2,2,16))], inf).trace,
        \pattern, Pfunc{|bev|
            Pfindur(8,Ppar(bev.numplayers.collect{
                Pspawn(
                    Pbind(
                        \patternKey, Pwrand(
                            [ \chord, \trill, \onethreelong, \three,\one,\walk,\seven],
                            [    0.025,    0.2,        0.025,   0.2,  0.25, 0.2, 0.1],
                            inf),
                        \chan, Pwhite(0,6,inf),
                        \pan, Prand([127,103,63,30,0],inf),
                        \octave, Pseq([4,5,6],inf),
                        \noteamount, Prand([2,4,6,8],inf),
                        \pattern, Pfunc { |ev|
                            ~fluid.control(ev.chan,10,ev.pan);
                            Pfin(ev.noteamount,Pbindf(ev.dict[ev.patternKey].value,
                                \scale, bev.scale,
                                \octave, ev.octave,
                                \chan, ev.chan,
                                \root, bev.root,
                                \noteamount, ev.noteamount,
                            ))
                        },
                        \delta, 0,
                        \method, \seq
                    ), (dict: patternChoices));
            } ++[
                //meter, to remove
                Pbind(
                    \type, \midi,
                    \midiout, ~fluid,
                    \chan, 9,
                    \midinote, 50,
                    \dur, 1/2,
                    \amp, Pseq([0.7,0.5,0.5,0.5], inf)
                ),

            ]
            ))},
        \delta,0,
        \method,\seq
    ))).play;
)

p.stop;
