import data from "assets/data.json";

export function getCharacter(name: string) {
    return data.character[name];
}
export function getObject(name: string) {
    return data.mapobject[name];
}
export function getImage(name: string) {
    return data.image[name];
}
