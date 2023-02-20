import { render } from "preact";
import { App } from "./app";
import "./index.css";
import { MapObjects } from "./Objects";

render(
    <MapObjects odata={[]} />,
    document.getElementById("app") as HTMLElement
);
