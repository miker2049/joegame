#define TSF_IMPLEMENTATION
/* #define TSF_STATIC */
/* #define TSF_NO_STDIO */
/* #define TML_NO_STDIO */
#define SMF_CRIT printf
#include "tsf.h"
#include "emscripten.h"
#include "./smf.h"
#define PLENGTH 128


static double MSSTEP = 128 * (1000.0 / 44100.0);
static float buff[256];
static unsigned int t;

const static char MinimalSoundFont[] = {
#define TEN0 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    'R', 'I',  'F',  'F',  220,  1,    0,    0,    's',  'f',  'b',  'k',  'L',
    'I', 'S',  'T',  88,   1,    0,    0,    'p',  'd',  't',  'a',  'p',  'h',
    'd', 'r',  76,   TEN0, TEN0, TEN0, TEN0, 0,    0,    0,    0,    TEN0, 0,
    0,   0,    0,    0,    0,    0,    255,  0,    255,  0,    1,    TEN0, 0,
    0,   0,    'p',  'b',  'a',  'g',  8,    0,    0,    0,    0,    0,    0,
    0,   1,    0,    0,    0,    'p',  'm',  'o',  'd',  10,   TEN0, 0,    0,
    0,   'p',  'g',  'e',  'n',  8,    0,    0,    0,    41,   0,    0,    0,
    0,   0,    0,    0,    'i',  'n',  's',  't',  44,   TEN0, TEN0, 0,    0,
    0,   0,    0,    0,    0,    0,    TEN0, 0,    0,    0,    0,    0,    0,
    0,   1,    0,    'i',  'b',  'a',  'g',  8,    0,    0,    0,    0,    0,
    0,   0,    2,    0,    0,    0,    'i',  'm',  'o',  'd',  10,   TEN0, 0,
    0,   0,    'i',  'g',  'e',  'n',  12,   0,    0,    0,    54,   0,    1,
    0,   53,   0,    0,    0,    0,    0,    0,    0,    's',  'h',  'd',  'r',
    92,  TEN0, TEN0, 0,    0,    0,    0,    0,    0,    0,    50,   0,    0,
    0,   0,    0,    0,    0,    49,   0,    0,    0,    34,   86,   0,    0,
    60,  0,    0,    0,    1,    TEN0, TEN0, TEN0, TEN0, 0,    0,    0,    0,
    0,   0,    0,    'L',  'I',  'S',  'T',  112,  0,    0,    0,    's',  'd',
    't', 'a',  's',  'm',  'p',  'l',  100,  0,    0,    0,    86,   0,    119,
    3,   31,   7,    147,  10,   43,   14,   169,  17,   58,   21,   189,  24,
    73,  28,   204,  31,   73,   35,   249,  38,   46,   42,   71,   46,   250,
    48,  150,  53,   242,  55,   126,  60,   151,  63,   108,  66,   126,  72,
    207, 70,   86,   83,   100,  72,   74,   100,  163,  39,   241,  163,  59,
    175, 59,   179,  9,    179,  134,  187,  6,    186,  2,    194,  5,    194,
    15,  200,  6,    202,  96,   206,  159,  209,  35,   213,  213,  216,  45,
    220, 221,  223,  76,   227,  221,  230,  91,   234,  242,  237,  105,  241,
    8,   245,  118,  248,  32,   252};

tsf *init_web(char *fontdata, int sizefontdata, unsigned int isogg) {
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

smf_t *load_midi_web(char *data, int size) {
  smf_t *mf;
  mf = smf_load_from_memory((unsigned char *)data, size);
  return mf;
}


void *process_midi_web(smf_t *song, double g_Msec, tsf *synth) {
  // Loop through all MIDI messages which need to be played up until the current
  // playback time
  //  For each SAMPLE
  smf_event_t *event = smf_get_next_event(song);
  for (g_Msec += MSSTEP; song && g_Msec >= event->time_seconds * 1000; event = smf_get_next_event(song)) {
  /*   switch (song->type) { */
  /*   case TML_PROGRAM_CHANGE: // channel program (preset) change (special */
  /*                            // handling for 10th MIDI channel with drums) */
  /*     tsf_channel_set_presetnumber(synth, song->channel, song->program, */
  /*                                  (song->channel == 9)); */
  /*     break; */
  /*   case TML_NOTE_ON: // play a note */
  /*     tsf_channel_note_on(synth, song->channel, song->key, */
  /*                         song->velocity / 127.0f); */
  /*     break; */
  /*   case TML_NOTE_OFF: // stop a note */
  /*     tsf_channel_note_off(synth, song->channel, song->key); */
  /*     break; */
  /*   case TML_PITCH_BEND: // pitch wheel modification */
  /*     tsf_channel_set_pitchwheel(synth, song->channel, song->pitch_bend); */
  /*     break; */
  /*   case TML_CONTROL_CHANGE: // MIDI controller messages */
  /*     tsf_channel_midi_control(synth, song->channel, song->control, */
  /*                              song->control_value); */
  /*     break; */
  /*   } */
  }
  return song;
}
