import { useEffect, useState } from "preact/hooks";
import { getWangPreviews } from "./snapshotMap";
import "./app.css";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";

export function WangSetView({
    tiledjson,
    mappath,
}: {
    tiledjson: TiledRawJSON;
    mappath: string;
}) {
    const [images, setImages] = useState<{ name: string; data: string }[]>([]);
    useEffect(() => {
        let mounted = true;
        if (images.length === 0)
            getWangPreviews(tiledjson, mappath).then((ims) => {
                if (mounted)
                    setImages(
                        ims.map(([name, data]) => ({
                            name,
                            data,
                        }))
                    );
            });
        return () => {
            mounted = false;
            setImages([]);
        };
    }, []);
    return (
        <div className="flex">
            {images.map(({ name, data }, idx) => (
                <div>
                    <img
                        className={
                            "wang-preview transition-all hover:-translate-y-1 hover:scale-110"
                        }
                        src={data}
                        key={`wang-preview-${idx}`}
                    ></img>
                    <p>{name}</p>
                </div>
            ))}
        </div>
    );
}
