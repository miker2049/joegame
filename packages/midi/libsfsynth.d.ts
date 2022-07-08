declare function loadSynth(conf: any): Promise<SfSynthModule>


declare namespace loadSynth {

    export type pointer = number
    export interface SfSynthModule extends EmscriptenModule {
        _init_web(ptr: pointer, s: number): pointer
        _process_web(s: pointer): pointer
        _noteoff_web(s: pointer, preset: number, note: number, vel: number): void
        _noteon_web(s: pointer, preset: number, note: number, vel: number): void
        _process_midi_web(m: pointer, sec: number, s: pointer): pointer
        _load_midi_web(m: pointer, s: number): pointer
        _tsf_note_off_all(s: pointer): void
        _get_presets_count(s: pointer): number
        _get_preset(s: pointer, i: number): pointer
        _seek_to_msec_web(m: pointer, msec: number): pointer
        _get_midi_total_msec_web(s: pointer): number
        UTF8ToString(p: pointer): string
    }

    export type LoadSynthFunction = (conf: any) => Promise<Synth.SfSynthModule>
    export type ProcessorInMessages = {
        sfdata: Uint8Array
        type: "loadsf"
    } | {
        mididata: Uint8Array
        type: "loadmidi"
    } | {
        type: "play"
    } | {
        type: "pause"
    } | {
        type: "stop"
    } | {
        type: "on",
        note: number,
        preset: number
    } | {
        type: "off",
        note: number,
        preset: number
    } | {
        type: "getpresetnames"
    } | {
        type: "seekmidi",
        msec: number
    }
}
export = loadSynth
