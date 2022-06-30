import "./piano-style.css"
interface PianoKeyboardProps {
    numberOfOctaves: number
    startOctave: number
    showLabels: boolean
    withShiftButtons: boolean
    withNoteSelection: boolean
    withClefSelection: boolean
    displayedOctaves: number
    handleKeyClick: (key: number)=>void
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
                                onClick={()=>
                                    props.handleKeyClick(0+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey">
                                <p>C{props.startOctave + (idx + 1)}</p>
                            </li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(1+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey">
                                {props.showLabels ?? <p>♯</p>}
                            </li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(2+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(3+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(4+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(5+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(6+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(7+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(8+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(9+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(10+(12*(props.startOctave + (idx + 2))))}
                                class="blackKey"></li>
                            <li
                                onClick={()=>
                                    props.handleKeyClick(11+(12*(props.startOctave + (idx + 2))))}
                                class="whiteKey"></li>
                        </>
                    );
                })}
            </ul>
        </div>
    );

}
