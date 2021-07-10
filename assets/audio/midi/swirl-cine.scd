MIDIClient.init;
MIDIClient.disposeClient;

~fluid = MIDIOut(0)

(
~fluid.control(0,0,0);
~fluid.control(0,32,0);
~fluid.program(1,33);
~fluid.program(2,86);
~fluid.program(3,21);
~fluid.program(4,17);
~fluid.program(5,80);
~fluid.program(6,76);
~fluid.control(0,0,1);
~fluid.control(0,32,0);
~fluid.program(0,0);
)

(
Pbindef(
    \drum,
    \type, \midi,
    \midinote, 49,
    \chan, 9,
    \dur, 1/2,
    \midiout, ~fluid,
    \amp, 0.8
).pause
)
(
Pbindef(
    \test,
    \type, \midi,
    \midinote, 60,
    \chan, 0,
    \dur, 1,
    \midiout, ~fluid,
    \amp, 0.8
).play
)

(
Pbindef(
    \test2,
    \type, \midi,
    \midinote, Pwalk([70,67,75,79],
        Prand([-1,1], inf),
        Pseq([1, -1], inf)),
    \chan, 3,
    \dur, 1/4,
    \midiout, ~fluid,
    \amp, Pseq([0.5],inf)
).play
)

(
Pbindef(
    \test3,
    \type, \midi,
    \midinote, 63,
    \chan, 2,
    \dur, 1/3,
    \midiout, ~fluid,
    \amp, 0.3
).play
)

Pdef.removeAll;
Tempo.bpm = 108
Pbind.stopAll



~patternChoices = (
    three: {
        Pbind(
            \amp, 0.8,
            \degree, 3,
        )
    })
~pp=~patternChoices['three'].value.patternpairs
~pa=~pp.find(['degree'])


(
var tempomod= 1;
var defaultPlayerBind = Pbind(\type,\midi,\midiout,~fluid);
var    patternChoices = (
    three: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.8,
            \degree, 3,
            \dur, Prand([0.125],inf) *tempomod
        )
    },
    one: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.8,
            \degree, 0,
            \dur, Prand([0.25],inf) * tempomod
        )
    },
    seven: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.6,
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
            \amp, Pwhite(0.3,0.7,inf),
            // \degree, [0,5],
            // \degree, Pfunc({
            //     |chordev|
            //     var out;
            //     // if(chordev.scale == Scale.major, {out=[ [1,3,5], [3,5,8] ].choose});
            //     // if(chordev.scale == Scale.minor, {out= [ [1,6], [6,12] ].choose });
            //     if(
            //         (chordev.scale == Scale.major).or(chordev.scale == Scale.minor),
            //         [ [1,3,5], [3,5,8] ].choose,
            //         [[1,5], [5,8]].choose
            //     );
            //     // out;
            // }).trace,
            \dur, 4
        )
    },
    walk: {
        Pbindf(
            defaultPlayerBind,
            \amp, 0.8,
            \degree, Pwalk([7,5,4,14],
                Prand([-1,1], inf),
                Pseq([1, -1], inf)),
            \dur, Prand([0.125],inf) *tempomod
        )
    }
);

~fluid.control(0,0,0);
~fluid.control(0,32,0);
~fluid.program(1,34);
~fluid.program(2,85);
~fluid.program(3,22);
~fluid.program(4,19);
~fluid.program(5,80);
~fluid.program(6,76);


~fluid.control(0,0,1);
~fluid.control(0,32,0);
~fluid.program(0,0);
Pdef(\mst,Pspawn(
    Pbind(
        \root, Pseq([
            0,
            0,
            3,
            5,
            -2,
            1,
            7,
            7,
        ], inf),
        \scale, Pseq([
            Scale.minor,
            Scale.minor,
            Scale.majorPentatonic,
            Scale.minor,
            Scale.minor,
            Scale.major,
            Scale.major,
            Scale.major,
        ], inf).trace,
        \numplayers, Pn(Pstutter(3,Pseries(2,2,16))),
        \pan, Prand([127,103,63,30,0],inf),
        \pattern, Pfunc{|bev|
            Pfindur(8,Ppar(bev.numplayers.collect{
                Pspawn(
                    Pbind(
                        \patternKey, Prand([ \chord, \trill, \three,\one,\walk,\seven], inf),
                        
                        // \patternKey, Prand([\chord], inf),
                        \chan, Pwhite(0,3,inf),
                        \octave, Pseq([4,5,6],inf),
                        \noteamount, Prand([2,4,6,8],inf),
                        \pattern, Pfunc { |ev|
                            ~fluid.control(ev.chan,10,bev.pan);
                            Pfin(ev.noteamount,Pbindf(ev.dict[ev.patternKey].value,
                                \scale, bev.scale,
                                \octave, ev.octave,
                                \chan, ev.chan,
                                \root, bev.root,
                                \noteamount, ev.noteamount,
                                // catch chords here?
                                // \degree, Pfunc({
                                //     arg chordev;
                                //     if(chordev.patternKey==\chord,
                                //         {
                                //             switch( chordev.scale,
                                //                 Scale.major, { [[1,3,5],[3,5,8]].choose },
                                //                 Scale.minor, { [[1,3,5],[3,5,8]].choose },
                                //                 Scale.majorPentatonic, { [[1,6],[6,12]].choose },
                                //                 Scale.minorPentatonic, { [[1,6],[6,12]].choose },
                                //             );
                                //         },
                                //         {
                                //             var pat=ev.dict[chordev.patternKey].value.patternpairs;
                                //             var deg = pat.find(['degree'])+1;
                                //             deg.postln;
                                //         }
                                //     )
                                // })
                            ))
                        },
                        \delta, 0,
                        \method, \seq
                    ), (dict: patternChoices));
            }))},
        \delta,0,
        \method,\seq
    ))).play;
)

p.stop;
