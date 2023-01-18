import { useEffect, useState } from "preact/hooks";
import { getWangPreviews } from "./snapshotMap";
import "./app.css";

export function LevelView({ path }: { path?: string }) {
    const [images, setImages] = useState<string[]>([]);
    useEffect(() => {
        getWangPreviews("/assets/maps/desert/desert-stamps2.json").then((ims) =>
            setImages(ims)
        );
    }, []);
    return (
        <div className="flex">
            {images.map((imsrc, idx) => (
                <img
                    className={
                        "wang-preview hover:-translate-y-1 hover:scale-110"
                    }
                    src={imsrc}
                    key={`wang-preview-${idx}`}
                ></img>
            ))}
        </div>
    );
}
