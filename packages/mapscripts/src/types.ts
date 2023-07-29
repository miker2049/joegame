type JDBTables = "tweets";

type ConfItem = {
    type: "signal" | "terrain";
    name: string; // perlin, grass
};

type ConfItemFilter = {
    type: string;
    params: [string, number][];
};

type ConfSignal = ConfItemParent & {
    type: "signal";
    params: [string, number][];
    filters: ConfItemFilter[];
};

type ConfTerrain = ConfItemParent & {
    type: "terrain";
};

type ConfItemParent = ConfItem & {
    type: "signal" | "terrain";
    children: (ConfSignal | ConfTerrain)[];
};
export type worldconf = ConfItemParent;
