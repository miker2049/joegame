import "./piano-style.css"
interface PianoKeyboardProps {
    numberOfOctaves: number
    startOctave: number
    showLabels: boolean
    withShiftButtons: boolean
    withNoteSelection: boolean
    withClefSelection: boolean
    displayedOctaves: number

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
                            <li onClick={()=>console.log('howdy')} class="whiteKey">
                                {props.showLabels ?? <p>C{props.startOctave + (idx + 1)}</p>}
                            </li>
                            <li class="blackKey">
                                {props.showLabels ?? <p>♯</p>}
                            </li>
                            <li class="whiteKey"></li>
                            <li class="blackKey"></li>
                            <li class="whiteKey"></li>
                            <li class="whiteKey"></li>
                            <li class="blackKey"></li>
                            <li class="whiteKey"></li>
                            <li class="blackKey"></li>
                            <li class="whiteKey"></li>
                            <li class="blackKey"></li>
                            <li class="whiteKey"></li>
                        </>
                    );
                })}
            </ul>
        </div>
    );

}
