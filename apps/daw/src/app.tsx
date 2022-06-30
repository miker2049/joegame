import { Logo } from './logo'
import { PianoKeyboard } from './PianoKeyboard'

export function App() {

    return (
        <>
            <h1> joegame daw </h1>
            <PianoKeyboard
                numberOfOctaves={3}
                startOctave={2}
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
