import { useEffect, useState } from 'preact/hooks'
import { createSynth } from 'midi'
import { PianoKeyboard } from './PianoKeyboard'
import synthWorkletURL from 'midi/synth-worklet.js?url'

export function App() {
    let [node, setNode]=useState(undefined)
    useEffect(() => {
        const context = new AudioContext()
        createSynth(context, synthWorkletURL).then(n=>{
            setNode(n)
        })
    });



    return (
        <>
            <h1> joegame daw </h1>
            <PianoKeyboard
                numberOfOctaves={3}
                startOctave={0}
                showLabels={true}
                withShiftButtons={false}
                withNoteSelection={false}
                withClefSelection={false}
                displayedOctaves={3}
                handleKeyClick={console.log}
            />
        </>
    )
}
