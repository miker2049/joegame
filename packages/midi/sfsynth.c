#define TSF_IMPLEMENTATION
#define TML_IMPLEMENTATION
/* #define TSF_STATIC */
/* #define TSF_NO_STDIO */
/* #define TML_NO_STDIO */

#define TML_ERROR(arg) printf("ERROR: %s\n", arg);
#define TML_WARN(arg) printf("ERROR: %s\n", arg);
#include "./stb_vorbis.c"
#include "tml.h"
#include "tsf.h"
/* #include "emscripten.h" */

#define PLENGTH 128

static double MSSTEP = 128 * (1000.0 / 44100.0);
static float buff[256];
static unsigned int t;

tsf *init_web(char *fontdata, int sizefontdata) {
  tsf *synth;
  char *f;
  f = fontdata;
  synth = tsf_load_memory((unsigned char *)fontdata, sizefontdata);
  tsf_set_output(synth, TSF_STEREO_UNWEAVED, 44100, -10);
  tsf_set_max_voices(synth, 512);
  return synth;
}

void noteon_web(tsf *s, int preset, int note, float vel) {
  tsf_note_on(s, preset, note, vel); // C2
}

void noteoff_web(tsf *s, int preset, int note, float vel) {
  tsf_note_off(s, preset, note); // C2
}

void *process_web(tsf *s) {
  tsf_render_float(s, (float *)buff, PLENGTH, 0);
  return buff;
}

tml_message *load_midi_web(char *data, int size) {
  tml_message *mf;
  mf = tml_load_memory((unsigned char *)data, size);
  return mf;
}

const char *get_preset(tsf *synth, int i) {
  return tsf_get_presetname(synth, i);
}

int get_presets_count(tsf *synth) {
  int count;
  count = tsf_get_presetcount(synth);
  return count;
}

unsigned int get_midi_total_msec_web(tml_message *first) {
  unsigned int dur;
  tml_get_info(first, NULL, NULL, NULL, NULL, &dur);
  return dur;
}

tml_message *seek_to_msec_web(tml_message *tml, unsigned int time) {
  while (tml && tml->time < time) {
    tml = tml->next;
  }
  return tml;
}

tml_message *process_midi_web(tml_message *song, double g_Msec, tsf *synth) {
  // Loop through all MIDI messages which need to be played up until the current
  // playback time
  //  For each SAMPLE
  for (g_Msec += MSSTEP; song && g_Msec >= song->time; song = song->next) {
    switch (song->type) {
    case TML_PROGRAM_CHANGE: // channel program (preset) change (special
                             // handling for 10th MIDI channel with drums)
      tsf_channel_set_presetnumber(synth, song->channel, song->program,
                                   (song->channel == 9));
      break;
    case TML_NOTE_ON: // play a note
      tsf_channel_note_on(synth, song->channel, song->key,
                          song->velocity / 127.0f);
      break;
    case TML_NOTE_OFF: // stop a note
      tsf_channel_note_off(synth, song->channel, song->key);
      break;
    case TML_PITCH_BEND: // pitch wheel modification
      tsf_channel_set_pitchwheel(synth, song->channel, song->pitch_bend);
      break;
    case TML_CONTROL_CHANGE: // MIDI controller messages
      tsf_channel_midi_control(synth, song->channel, song->control,
                               song->control_value);
      break;
    }
  }
  return song;
}
