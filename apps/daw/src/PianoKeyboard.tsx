import "./piano-style.css"
interface PianoKeyboardProps {
    numberOfOctaves: number
    startOctave: number
    showLabels: boolean
    withShiftButtons: boolean
    withNoteSelection: boolean
    withClefSelection: boolean
    displayedOctaves: number
    handleMouseDown: (key: number)=>void
    handleMouseUp: (key: number)=>void
}
const MAX_OCTAVES = 6

export function PianoKeyboard(props: PianoKeyboardProps) {
    if (props.numberOfOctaves < 1 || props.numberOfOctaves > MAX_OCTAVES)
        return <p>Wrong number of octaves specified</p>;
    props.startOctave = Math.max(0, Math.min(MAX_OCTAVES, props.startOctave))

    return (
        <div class="DA-Keyboardcontainer">'
            <ul class="DA-PianoKeyboard">
                {Array(props.displayedOctaves).fill(0).map((i, idx) => {
                    return (
                        <>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(0+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(0+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey">
                                <p>C{props.startOctave + (idx + 1)}</p>
                            </li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(1+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(1+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey">
                                {props.showLabels ?? <p>♯</p>}
                            </li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(2+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(2+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(3+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(3+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(4+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(4+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(5+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(5+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(6+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(6+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(7+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(7+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(8+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(8+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(9+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(9+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(10+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(10+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onMouseDown={()=>
                                    props.handleMouseDown(11+(12*(props.startOctave + (idx + 2))))}
                                onMouseUp={()=>
                                    props.handleMouseUp(11+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                        </>
                    );
                })}
            </ul>
        </div>
    );

}
