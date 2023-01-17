import { loadMap } from "joegamelib/src";
import { ILevelComponents } from "joegamelib/src/ILevel";
import { useEffect, useState } from "preact/hooks";

export function LevelView({ path }: { path?: string }) {
    const [level, setLevel] = useState<ILevelComponents>();
    useEffect(() => {
        if (!level)
            loadMap({
                mapPath: path || "assets/maps/small-garden-phaedrus.json",
                gameConfigOverrides: {
                    scale: {},
                },
            }).then(([level, facade]) => {
                setLevel(level);
            });
        return () => {
            if (level) {
                level.machineRegistry.stopAll();
                level.scene.game.destroy(true);
                // woo
                setLevel(undefined);
            }
        };
    }, [level]);
    return <div id={"frame"}></div>;
}
