(
s.waitForBoot(
    {
        ~createVowel = {
            arg vowel = \i;
            {
                |dur = 1, freq=300, amp=0.5, pitchDev=10, sus=1|
                var sig = Formants.ar(Line.ar(freq+pitchDev,freq,dur/4), Vowel(vowel, \tenor)) * amp;
                var env = EnvGen.ar(Env.linen(0.01,sus,0.05, amp),1, doneAction: 2);
                var noise = PinkNoise.ar() * EnvGen.ar(Env.perc(0.01,0.5));
                sig = BMoog.ar(sig, Line.ar(440,20000,dur));
                sig = sig + (noise *0.1);
                Out.ar(0, Pan2.ar(sig*env));
            }
        };

        ~runConvert = {
            arg input;
            var p;
            p=Pipe.new("ffmpeg -i %.wav -codec:a libmp3lame -b:a 128k %.mp3".format(input,input),
                "r");
            l = p.getLine;                    // get the first line
            while({l.notNil}, {l.postln; l = p.getLine; });    // post until l = nil
            p.close;
            Pipe.new("rm %.wav".format(input),"r").close();
            p;
        };
        SynthDef(\vowel_i,~createVowel.(\i)).add;
        SynthDef(\vowel_e,~createVowel.(\e)).add;
        SynthDef(\vowel_o,~createVowel.(\o)).add;
        SynthDef(\vowel_u,~createVowel.(\u)).add;

        SynthDef(\itempickup1,{
            arg freq = 440;
            var sig=Pluck.ar(
                WhiteNoise.ar(0.1),1,
                freq.reciprocal,freq.reciprocal,
                10,coef: 0.308);
            EnvGen.ar(Env.perc(0.001,0.5),doneAction: 2);
            Out.ar(0,Pan2.ar(sig));
        }).add;

        SynthDef(\walking,{
            arg freq = 220;
            var sig=Pluck.ar(BrownNoise.ar(1.1),1,2200.reciprocal,freq.reciprocal,-1,0.985);
            sig = sig +  Pluck.ar(BrownNoise.ar(1.1),TDelay.ar(Trig.ar(1),0.0125),2200.reciprocal,(freq-10).reciprocal,-1,0.985);
            EnvGen.ar(Env.perc(0.001,0.5),doneAction: 2);
            Out.ar(0,Pan2.ar(sig));
        }).add;

        SynthDef(\hit,{
            var sig = BrownNoise.ar();
            var env = EnvGen.ar(Env.perc(0.01,0.125/2), doneAction: 2);
            var amp = 0.9;
            // sig = BMoog.ar(sig,XLine.ar(2000,2,0.5));
            Out.ar(0,Pan2.ar(sig*env*amp));
        }).add;

        SynthDef(\dash,{
            var sig = BrownNoise.ar();
            var env = EnvGen.ar(Env.perc(0.01,1), doneAction: 2);
            var amp = 0.5;
            // sig = BMoog.ar(sig,XLine.ar(0.1,10000,0.5),1,mode: 0);
            Out.ar(0,Pan2.ar(sig*env*amp));
        }).add;

        s.sync;
        s.prepareForRecord("vowel.wav",2);
        s.sync;
        fork {
            var dur = 1;
            s.record;
            Synth(\vowel_i, [\pitchDev,20,\sus,0.33,\dur, dur, \freq, 60.midicps, \amp, 0.75]);
            dur.wait;
            Synth(\vowel_e, [\pitchDev,20,\sus,0.33,\dur, dur, \freq, 60.midicps, \amp, 0.75]);
            dur.wait;
            Synth(\vowel_o, [\pitchDev,20,\sus,0.33,\dur, dur, \freq, 60.midicps, \amp, 0.75]);
            dur.wait;
            Synth(\vowel_u, [\pitchDev,20,\sus,0.33,\dur, dur, \freq, 60.midicps, \amp, 0.75]);
            dur.wait;
            s.stopRecording;
            s.sync;

            s.prepareForRecord("hit.wav",2);
            s.sync;
            s.record;
            Synth(\hit);
            dur.wait;

            Synth(\hit);
            dur.wait;

            Synth(\hit);
            dur.wait;

            s.stopRecording;
            s.sync;



            s.prepareForRecord("walk.wav",2);
            s.sync;
            s.record;
            Synth(\walking,[\freq,220]);
            0.0125.wait;
            Synth(\walking,[\freq,196]);
            dur.wait;

            Synth(\walking,[\freq,220]);
            0.0125.wait;
            Synth(\walking,[\freq,196]);
            dur.wait;

            Synth(\walking,[\freq,220]);
            0.0125.wait;
            Synth(\walking,[\freq,196]);
            dur.wait;



            s.stopRecording;
            s.sync;

            s.prepareForRecord("item.wav",2);
            s.sync;
            s.record;
            Synth(\itempickup1);
            dur.wait;
            Synth(\itempickup1);
            dur.wait;
            Synth(\itempickup1);
            dur.wait;
            s.stopRecording;
            s.sync;
            ~runConvert.("item");
            ~runConvert.("hit");
            ~runConvert.("vowel");
            ~runConvert.("walk");


            // s.exit;
        }
    });
)

