MIDIClient.init;
MIDIClient.disposeClient;

~fluid = MIDIOut(0)
16.do{arg i; ~fluid.allNotesOff(i)}
~fluid.noteOn(0,60,72)
Pdef.removeAll;

(
Pbind(
    \degree, Pseries(0,1,8).trace,
    \dur, 1
).play
)


(
var tempomod= 1;
var defaultPlayerBind = Pbind(\type,\midi,\midiout,~fluid);
var    patternChoices = (
    firstpos: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.55.rrand(0.8),
            \dur, 8,
            \degree, Pseq( [[0,2,4,6, 9]], inf ),
        )
    },
    second: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.5.rrand(0.7),
            \dur, 6,
            \degree, Pseq([ [2,4,6] ], 2),
        )
    },
    down: {
        Pbindf(
            defaultPlayerBind,
            \amp, Pseq([0.6,0.5],inf),
            \note, Pseq([6,4,2,0],inf),
            \dur ,   Pseq([0.5,0.5,0.5,4],inf),
            \sustain, Pseq([5.5,5.0,4.5,4],inf),
        )
    },
    down2: {
        Pbindf(
            defaultPlayerBind,
            \amp, Pseq([0.6,0.5],inf),
            \note, Pseq([2,1,0,-1,0],inf),
            \dur ,   Pseq([0.5,0.5,0.5,0.75,4],inf),
            // \sustain, Pseq([5.5,5.0,4.5,4],inf),
        )
    },
    seven: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.5.rrand(0.7),
            \degree, 5,
            \dur, 1
        )
    },
    silent: {
        Pbindf(
            defaultPlayerBind,
            \degree, 0,
            \amp, 0,
            \dur, 4
        )
    }
);

Tempo.bpm = 72;
~fluid.control(0,0,0);
~fluid.control(0,32,0);

~fluid.program(0,53);
~fluid.program(1,52);
~fluid.program(2,85);
~fluid.program(3,17);
~fluid.program(4,78);

Pdef(\mst,Pspawn(
    Pbind(
        \numplayers, Pseq([Pstutter(10,Pseries(4,2,16))], inf).trace,
        \pattern, Pfunc{|bev|
            Pfindur(24,Ppar(bev.numplayers.collect{
                Pspawn(
                    Pbind(
                        \patternKey, Pwrand(
                            [ \firstpos, \second, \down, \silent, \down2],
                            [      0.25,    0.25,  0.25,    0.15,   0.1],
                            inf),
                        \scale, Scale.major,
                        \chan, Prand([ 0,1,2,3 ],inf),
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
                    // \amp, Pseq([0.7,0.5,0.5,0.5], inf),
                    \amp, 1
                ),
                Pbind(
                    \type, \midi,
                    \midiout, ~fluid,
                    \chan, 4,
                    \degree, Pseq([
                        Pn(Pseq([6,5,8,9]),8),
                        Pn(Pseq([6,5,8,11]),8),
                    ],inf),
                    \dur, 1/2,
                    // \amp, Pseq([0.7,0.5,0.5,0.5], inf),
                    \amp, 1
                ),

            ]
            ))},
        \delta,1,
        \method,\seq
    ))).play;
)

p.stop;
