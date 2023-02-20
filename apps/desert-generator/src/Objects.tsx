import { useEffect, useState } from "preact/hooks";
import _data from "assets/data.json";
const data: {
    image: Record<string, TextureData>;
    mapobject: Record<string, ObjectData>;
} = _data;

type TextureData = {
    key: string;
    url: string;
    source: string;
    frameConfig?: {
        frameWidth: number;
        frameHeight: number;
        margin?: number;
        spacing?: number;
        columns: number;
        imagewidth: number;
        imageheight: number;
    };
};
type ObjectData = {
    req_image: string[];
    name: string;
    width?: number;
    height?: number;
    tile_config?: {
        width: number;
        texture: string;
        tiles: number[];
    };
    body_config?: {
        x: number;
        y: number;
        width: number;
        height: number;
    };
};

export function MapObjects({ odata }: { odata: ObjectData[] }) {
    odata = Object.entries(data.mapobject).map((entry) => entry[1]);
    return (
        <div>
            {odata.map((obj) => {
                if (!obj.tile_config)
                    return (
                        <SimpleImageObject
                            key={`${obj.name}-object-viewer-entry`}
                            imgpath={
                                "assets/images/" + obj.req_image[0] + ".png"
                            }
                            name={obj.name}
                        />
                    );
                else {
                    const text = data.image[obj.tile_config.texture];
                    if (text) return <TileObject obj={obj} text={text} />;
                    else return <></>;
                }
            })}
        </div>
    );
}

function SimpleImageObject({
    imgpath,
    name,
}: {
    imgpath: string;
    name: string;
}) {
    return (
        <div>
            <p>{name}</p>
            <img src={imgpath} />
        </div>
    );
}

function TileObject({ obj, text }: { obj: ObjectData; text: TextureData }) {
    const [image, setImage] = useState<string>();
    useEffect(() => {
        if (image && image.length > 0) return;
        if (!obj.tile_config) return;
        if (!text.frameConfig) return;

        const objheight = Math.floor(
            obj.tile_config.tiles.length / obj.tile_config.width
        );
        const tilesize = text.frameConfig.frameWidth;
        const cnv = new OffscreenCanvas(
            obj.tile_config.width * tilesize * 2,
            objheight * tilesize * 2
        );
        const img = new Image();
        img.src = "/assets/images/" + obj.tile_config.texture + ".png";
        const ctx = cnv.getContext("2d");
        if (!ctx) return;
        const margin = text.frameConfig.margin || 0;
        const spacing = text.frameConfig.spacing || 0;
        function getTileCoords(n: number) {
            if (!text.frameConfig) return;
            const x = n % text.frameConfig.columns;
            const y = Math.floor(n / text.frameConfig.columns);
            return {
                x: x * tilesize + Math.max(x - 1, 0) * spacing + margin,
                y: y * tilesize + Math.max(y - 1, 0) * spacing + margin,
            };
        }
        obj.tile_config.tiles.forEach((tile, idx) => {
            if (!obj.tile_config) return;
            const { x: srcX, y: srcY } = getTileCoords(tile) || { x: 0, y: 0 };
            const outX = (idx % obj.tile_config.width) * tilesize;
            const outY = Math.floor(idx / obj.tile_config.width) * tilesize;
            ctx.drawImage(
                img,
                srcX,
                srcY,
                tilesize,
                tilesize,
                outX,
                outY,
                tilesize,
                tilesize
            );
        });
        cnv.convertToBlob()
            .then((b) => setImage(URL.createObjectURL(b)))
            .catch((err) => console.log(err));
    });
    return (
        <div>
            <p>{obj.name}</p>
            <img src={image}></img>
        </div>
    );
}
