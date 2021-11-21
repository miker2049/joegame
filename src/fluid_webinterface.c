
/*
** A file for interfacing with fluidlite through the webassembly Module generated by emscripten
*/







#include <stdio.h>
#include <string.h>
#include "../Fluidlite/include/fluidlite.h"
#include "fluidsynth_priv.h"

#define SAMPLES 128

typedef float process_buffs[2][128];

fluid_synth_t* fluid_web_init_synth() {
    fluid_settings_t* settings;
    settings = new_fluid_settings();

    return new_fluid_synth(settings);
}

process_buffs* fluid_web_create_buffers() {
    return FLUID_NEW(process_buffs);
}

void* fluid_web_get_chan_buff(process_buffs* buffs, int n){
    return buffs[n];
}

int fluid_web_noteon(fluid_synth_t* s, int ch, int note, int vel){
    int f = fluid_synth_noteon(s, ch,  note,  vel);
    printf("synth pointer %d\n", s);
    printf("a note on plays %d\n", f);
    return f;
}

int fluid_web_process(fluid_synth_t* synth, process_buffs buffs){
        // array of buffers used to setup channel mapping
        float *dry[1 * 2], *fx[1 * 2];
        float* left, *right;
        left=buffs[0];
        right=buffs[1];
        // first make sure to zero out the sample buffers everytime before calling fluid_synth_process()
        memset(left, 0, sizeof(*left));
        memset(right, 0, sizeof(*right));


        // setup channel mapping for a single stereo channel to which to render all dry audio to
        dry[0] = left;
        dry[1] = right;

        // Setup channel mapping for a single stereo channel to which to render effects to.
        // Just using the same sample buffers as for dry audio is fine here, as it will cause the effects to be mixed with dry output.
        // Note: reverb and chorus together make up two stereo channels. Setting up only one stereo channel is sufficient
        // as the channels wraps around (i.e. chorus will be mixed with reverb channel).
        fx[0] = left;
        fx[1] = right;

        int err = fluid_synth_process(synth, SAMPLES, 2, fx, 2, dry);
        printf("a float %f", fx[0][43]);

        return err;

}
