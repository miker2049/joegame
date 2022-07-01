import { useEffect, useState } from 'preact/hooks'
import { createSynth, loadFont } from 'midi'
import { PianoKeyboard } from './PianoKeyboard'
import synthWorkletURL from 'midi/synth-worklet.js?url'
import sfURL from 'midi/florestan-subset.sf2?url'

export function App(props: { context: AudioContext }) {
    let [node, setNode] = useState<undefined | AudioWorkletNode>(undefined)
    let [ready, setReady] = useState(false)
    if (!ready)
        useEffect(() => {
            createSynth(props.context, synthWorkletURL).then(n => {
                console.log(n)
                n.connect(props.context.destination)
                return n
            }).then(n=> {
                n.onprocessorerror = (err)=>console.log(err)
                return n
            }).then(n =>{
                loadFont(n, sfURL)
                return n
            }).then(n =>{
                setNode(n)
                setReady(true)
            }).then(_ => props.context.resume())
        });

    const handleNoteOn = (note: number) => {
        props.context.resume()

        if (node && props.context.state === 'running') {
            console.log(note, props.context.state, "hmmm")
            node.port.postMessage({type: 'on', note})
        }
    }
    const handleNoteOff = (note: number) => {
        props.context.resume()
        if (node) {
            console.log(note, "hrm")
            node.port.postMessage({type: 'off', note})
        }
    }

    const handleMsg = (msg:any)=> console.log(msg)

    if(node)
        node.port.onmessage = handleMsg

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
                handleMouseDown={handleNoteOn}
                handleMouseUp={handleNoteOff}
            />
        </>
    )
}
