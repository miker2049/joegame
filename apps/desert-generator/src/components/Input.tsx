import { ControlPanel } from "./ControlPanel";

export function Input({ name }: { name: string }) {
    return (
        <ControlPanel name={name} oneLine>
            <input type="text" name="" value="" className="" />
        </ControlPanel>
    );
}
